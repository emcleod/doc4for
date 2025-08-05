import unittest
from pathlib import Path
from pyfakefs.fake_filesystem_unittest import TestCase
from doc4for.f90.generate_module_tree import extract_module_data
from doc4for.models.common import BindingTypeEnum
from doc4for.models.procedure_models import PassType

class TestProcedurePassType(TestCase):

    def setUp(self):
        self.setUpPyfakefs()
   
    def test_pass_attributes_and_access_specifiers(self):
        self.fs.create_file(
            "/fake/path/measurement_handling.f90",
            contents="""\
    module measurement_handling
        implicit none
        private
        
        type, public :: Measurement
            private
            real :: value
            real :: uncertainty
        contains
            ! Standard accessor that uses the default pass(this) behavior
            procedure :: get_value => get_measurement_value
            
            ! Utility function that doesn't need instance data
            procedure, nopass :: is_valid_uncertainty
            
            ! Comparison that shows explicit pass usage for clarity
            procedure, pass(measurement_a) :: compare_measurements
        end type Measurement

    contains
        ! Standard method - implicitly receives the instance as 'this'
        function get_measurement_value(this) result(val)
            class(Measurement), intent(in) :: this
            real :: val
            val = this%value
        end function

        ! Utility function that operates on raw values, doesn't need instance data
        function is_valid_uncertainty(uncertainty_value) result(is_valid)
            real, intent(in) :: uncertainty_value
            logical :: is_valid
            is_valid = uncertainty_value >= 0.0
        end function

        ! Comparison showing explicit argument naming
        function compare_measurements(measurement_a, measurement_b) result(is_equal)
            class(Measurement), intent(in) :: measurement_a
            class(Measurement), intent(in) :: measurement_b
            logical :: is_equal
            is_equal = abs(measurement_a%value - measurement_b%value) <= &
                      max(measurement_a%uncertainty, measurement_b%uncertainty)
        end function
    end module
    """
        )
        
        result = extract_module_data([Path("/fake/path/measurement_handling.f90")], False)
        module = result[0]
        
        # Check module name
        self.assertEqual(module["module_name"], "measurement_handling")
        
        # Check that we have the Measurement type
        self.assertIn("Measurement", module["types"])
        measurement_type = module["types"]["Measurement"]
        
        # Check type attributes - should be PUBLIC and have PRIVATE components
        self.assertEqual(measurement_type["attributes"], ["PUBLIC"])
        
        # Check data components have private access
        self.assertIn("value", measurement_type["data_components"])
        self.assertIn("uncertainty", measurement_type["data_components"])
        self.assertEqual(measurement_type["data_components"]["value"]["attributes"], ["PRIVATE"])
        self.assertEqual(measurement_type["data_components"]["uncertainty"]["attributes"], ["PRIVATE"])
        
        # Check procedures
        self.assertIn("get_value", measurement_type["procedures"])
        self.assertIn("is_valid_uncertainty", measurement_type["procedures"])
        self.assertIn("compare_measurements", measurement_type["procedures"])
        
        # Test get_value - default pass behavior
        get_value_proc = measurement_type["procedures"]["get_value"]
        self.assertEqual(get_value_proc["pass_type"], PassType.DEFAULT)
        self.assertIsNone(get_value_proc["pass_name"])
        self.assertEqual(get_value_proc["implementation"], "get_measurement_value")
        
        # Test is_valid_uncertainty - nopass
        is_valid_proc = measurement_type["procedures"]["is_valid_uncertainty"]
        self.assertEqual(is_valid_proc["pass_type"], PassType.NONE)
        self.assertIsNone(is_valid_proc["pass_name"])
        self.assertIsNone(is_valid_proc["implementation"])  # No renaming, direct binding
        
        # Test compare_measurements - explicit pass with named argument
        compare_proc = measurement_type["procedures"]["compare_measurements"]
        self.assertEqual(compare_proc["pass_type"], PassType.NAMED)
        self.assertEqual(compare_proc["pass_name"], "measurement_a")
        self.assertIsNone(compare_proc["implementation"])  # No renaming, direct binding
        
        # Check that the actual implementation functions are captured at module level
        self.assertIn("get_measurement_value", module["functions"])
        self.assertIn("is_valid_uncertainty", module["functions"])
        self.assertIn("compare_measurements", module["functions"])
        
        # Check function signatures
        get_measurement_value_func = module["functions"]["get_measurement_value"]
        self.assertEqual(get_measurement_value_func["arguments"], ["this"])
        self.assertIn("this", get_measurement_value_func["in"])
        self.assertEqual(get_measurement_value_func["return"]["type"], "REAL")
        
        is_valid_uncertainty_func = module["functions"]["is_valid_uncertainty"]
        self.assertEqual(is_valid_uncertainty_func["arguments"], ["uncertainty_value"])
        self.assertIn("uncertainty_value", is_valid_uncertainty_func["in"])
        self.assertEqual(is_valid_uncertainty_func["return"]["type"], "LOGICAL")
        
        compare_measurements_func = module["functions"]["compare_measurements"]
        self.assertEqual(compare_measurements_func["arguments"], ["measurement_a", "measurement_b"])
        self.assertIn("measurement_a", compare_measurements_func["in"])
        self.assertIn("measurement_b", compare_measurements_func["in"])
        self.assertEqual(compare_measurements_func["return"]["type"], "LOGICAL")
        
        # Since module has implicit none and private as default, 
        # the functions should not have explicit PUBLIC attributes
        # (they're private by default at module level)
        self.assertEqual(get_measurement_value_func["attributes"], ["PRIVATE"])
        self.assertEqual(is_valid_uncertainty_func["attributes"], ["PRIVATE"])
        self.assertEqual(compare_measurements_func["attributes"], ["PRIVATE"])

if __name__ == "__main__":
    unittest.main()
    