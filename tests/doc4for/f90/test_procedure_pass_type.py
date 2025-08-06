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
        self.assertEqual(get_value_proc["attributes"], ["PUBLIC"])  

        # Test is_valid_uncertainty - nopass
        is_valid_proc = measurement_type["procedures"]["is_valid_uncertainty"]
        self.assertEqual(is_valid_proc["pass_type"], PassType.NONE)
        self.assertIsNone(is_valid_proc["pass_name"])
        self.assertIsNone(is_valid_proc["implementation"])  # No renaming, direct binding
        self.assertEqual(is_valid_proc["attributes"], ["PUBLIC"])  
        
        # Test compare_measurements - explicit pass with named argument
        compare_proc = measurement_type["procedures"]["compare_measurements"]
        self.assertEqual(compare_proc["pass_type"], PassType.NAMED)
        self.assertEqual(compare_proc["pass_name"], "measurement_a")
        self.assertIsNone(compare_proc["implementation"])  # No renaming, direct binding
        self.assertEqual(compare_proc["attributes"], ["PUBLIC"])  
        
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

    def test_pass_attributes_with_documentation(self):
        self.fs.create_file(
            "/fake/path/measurement_handling_doc.f90",
            contents="""\
    module measurement_handling
        implicit none
        private
        
        !!*
        ! Type for representing measurements with uncertainty
        ! Encapsulates value and uncertainty with type-bound procedures
        !*!
        type, public :: Measurement
            private
            !!* The measured value *!
            real :: value
            !!* The uncertainty in the measurement *!
            real :: uncertainty
        contains
            !!* Standard accessor that uses the default pass(this) behavior *!
            procedure :: get_value => get_measurement_value
            
            !!* Utility function that doesn't need instance data *!
            procedure, nopass :: is_valid_uncertainty
            
            !!* Comparison that shows explicit pass usage for clarity *!
            procedure, pass(measurement_a) :: compare_measurements
        end type Measurement

    contains
        !!*
        ! Gets the value from a measurement
        ! @in this The measurement instance
        ! @return The measurement value
        !*!
        function get_measurement_value(this) result(val)
            class(Measurement), intent(in) :: this
            real :: val
            val = this%value
        end function

        !!*
        ! Checks if an uncertainty value is valid
        ! @in uncertainty_value The uncertainty to check
        ! @return True if uncertainty is non-negative
        !*!
        function is_valid_uncertainty(uncertainty_value) result(is_valid)
            real, intent(in) :: uncertainty_value
            logical :: is_valid
            is_valid = uncertainty_value >= 0.0
        end function

        !!*
        ! Compares two measurements considering their uncertainties
        ! @in measurement_a First measurement
        ! @in measurement_b Second measurement  
        ! @return True if measurements are equal within uncertainty
        !*!
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
        
        result = extract_module_data([Path("/fake/path/measurement_handling_doc.f90")], False)
        module = result[0]
        
        # Check module name
        self.assertEqual(module["module_name"], "measurement_handling")
        
        # Check that we have the Measurement type
        self.assertIn("Measurement", module["types"])
        measurement_type = module["types"]["Measurement"]
        
        # Check type documentation
        self.assertEqual(measurement_type["description"], 
                        "Type for representing measurements with uncertainty\n"
                        "Encapsulates value and uncertainty with type-bound procedures\n")
        
        # Check type attributes
        self.assertEqual(measurement_type["attributes"], ["PUBLIC"])
        
        # Check data components have documentation and private access
        self.assertIn("value", measurement_type["data_components"])
        self.assertIn("uncertainty", measurement_type["data_components"])
        self.assertEqual(measurement_type["data_components"]["value"]["attributes"], ["PRIVATE"])
        self.assertEqual(measurement_type["data_components"]["value"]["description"], "The measured value\n")
        self.assertEqual(measurement_type["data_components"]["uncertainty"]["attributes"], ["PRIVATE"])
        self.assertEqual(measurement_type["data_components"]["uncertainty"]["description"], "The uncertainty in the measurement\n")
        
        # Check procedures and their documentation
        self.assertIn("get_value", measurement_type["procedures"])
        self.assertIn("is_valid_uncertainty", measurement_type["procedures"])
        self.assertIn("compare_measurements", measurement_type["procedures"])
        
        # Test get_value procedure attributes and documentation
        get_value_proc = measurement_type["procedures"]["get_value"]
        self.assertEqual(get_value_proc["description"], "Standard accessor that uses the default pass(this) behavior\n")
        self.assertEqual(get_value_proc["pass_type"], PassType.DEFAULT)
        self.assertIsNone(get_value_proc["pass_name"])
        self.assertEqual(get_value_proc["implementation"], "get_measurement_value")
        self.assertEqual(get_value_proc["attributes"], ["PUBLIC"])  # Inherits from type's private statement
        
        # Test is_valid_uncertainty procedure
        is_valid_proc = measurement_type["procedures"]["is_valid_uncertainty"]
        self.assertEqual(is_valid_proc["description"], "Utility function that doesn&#x27;t need instance data\n")
        self.assertEqual(is_valid_proc["pass_type"], PassType.NONE)
        self.assertIsNone(is_valid_proc["pass_name"])
        self.assertIsNone(is_valid_proc["implementation"])
        self.assertEqual(is_valid_proc["attributes"], ["PUBLIC"])
        
        # Test compare_measurements procedure
        compare_proc = measurement_type["procedures"]["compare_measurements"]
        self.assertEqual(compare_proc["description"], "Comparison that shows explicit pass usage for clarity\n")
        self.assertEqual(compare_proc["pass_type"], PassType.NAMED)
        self.assertEqual(compare_proc["pass_name"], "measurement_a")
        self.assertIsNone(compare_proc["implementation"])
        self.assertEqual(compare_proc["attributes"], ["PUBLIC"])
        
        # Check that the actual implementation functions are captured at module level
        self.assertIn("get_measurement_value", module["functions"])
        self.assertIn("is_valid_uncertainty", module["functions"])
        self.assertIn("compare_measurements", module["functions"])
        
        # Check function documentation and signatures
        get_measurement_value_func = module["functions"]["get_measurement_value"]
        self.assertEqual(get_measurement_value_func["description"], 
                        "Gets the value from a measurement\n\n")
        self.assertEqual(get_measurement_value_func["arguments"], ["this"])
        self.assertIn("this", get_measurement_value_func["in"])
        self.assertEqual(get_measurement_value_func["in"]["this"]["description"], "The measurement instance")
        self.assertEqual(get_measurement_value_func["return"]["type"], "REAL")
        self.assertEqual(get_measurement_value_func["return"]["description"], "The measurement value")
        
        is_valid_uncertainty_func = module["functions"]["is_valid_uncertainty"]
        self.assertEqual(is_valid_uncertainty_func["description"],
                        "Checks if an uncertainty value is valid\n\n")
        self.assertEqual(is_valid_uncertainty_func["arguments"], ["uncertainty_value"])
        self.assertIn("uncertainty_value", is_valid_uncertainty_func["in"])
        self.assertEqual(is_valid_uncertainty_func["in"]["uncertainty_value"]["description"], "The uncertainty to check")
        self.assertEqual(is_valid_uncertainty_func["return"]["type"], "LOGICAL")
        self.assertEqual(is_valid_uncertainty_func["return"]["description"], "True if uncertainty is non-negative")
        
        compare_measurements_func = module["functions"]["compare_measurements"]
        self.assertEqual(compare_measurements_func["description"],
                        "Compares two measurements considering their uncertainties\n\n")
        self.assertEqual(compare_measurements_func["arguments"], ["measurement_a", "measurement_b"])
        self.assertIn("measurement_a", compare_measurements_func["in"])
        self.assertEqual(compare_measurements_func["in"]["measurement_a"]["description"], "First measurement")
        self.assertIn("measurement_b", compare_measurements_func["in"])
        self.assertEqual(compare_measurements_func["in"]["measurement_b"]["description"], "Second measurement")
        self.assertEqual(compare_measurements_func["return"]["type"], "LOGICAL")
        self.assertEqual(compare_measurements_func["return"]["description"], "True if measurements are equal within uncertainty")
        
        # Check access attributes
        self.assertEqual(get_measurement_value_func["attributes"], ["PRIVATE"])
        self.assertEqual(is_valid_uncertainty_func["attributes"], ["PRIVATE"])
        self.assertEqual(compare_measurements_func["attributes"], ["PRIVATE"])

    def test_pass_attributes_with_private_procedures(self):
        self.fs.create_file(
            "/fake/path/measurement_private_procs.f90",
            contents="""\
    module measurement_handling
        implicit none
        private
        
        type, public :: Measurement
            private
            real :: value
            real :: uncertainty
        contains
            private  ! Make all following procedures private by default
            
            ! Private accessor - not visible outside
            procedure :: get_value => get_measurement_value
            
            ! Private utility function
            procedure, nopass :: is_valid_uncertainty
            
            ! Private comparison
            procedure, pass(measurement_a) :: compare_measurements
            
            ! Explicitly public procedures
            procedure, public :: get_value_public => get_measurement_value
            procedure, public :: is_valid_public => is_valid_uncertainty_impl
            
            ! Generic interface that's public
            generic, public :: operator(==) => compare_measurements
            
            ! Another private procedure after public ones
            procedure :: internal_helper
            
            ! Final procedure (no access specifier)
            final :: cleanup
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
        
        ! Alternative implementation for public binding
        function is_valid_uncertainty_impl(uncertainty_value) result(is_valid)
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
        
        ! Internal helper
        subroutine internal_helper(this)
            class(Measurement), intent(inout) :: this
            ! Some internal processing
        end subroutine
        
        ! Finalizer
        subroutine cleanup(this)
            type(Measurement), intent(inout) :: this
            ! Cleanup code
        end subroutine
    end module
    """
        )
        
        result = extract_module_data([Path("/fake/path/measurement_private_procs.f90")], False)
        module = result[0]
        
        # Check module name
        self.assertEqual(module["module_name"], "measurement_handling")
        
        # Check that we have the Measurement type
        self.assertIn("Measurement", module["types"])
        measurement_type = module["types"]["Measurement"]
        
        # Check type attributes
        self.assertEqual(measurement_type["attributes"], ["PUBLIC"])
        
        # Check data components have private access
        self.assertIn("value", measurement_type["data_components"])
        self.assertIn("uncertainty", measurement_type["data_components"])
        self.assertEqual(measurement_type["data_components"]["value"]["attributes"], ["PRIVATE"])
        self.assertEqual(measurement_type["data_components"]["uncertainty"]["attributes"], ["PRIVATE"])
        
        # Check procedures - private by default after private statement
        self.assertIn("get_value", measurement_type["procedures"])
        self.assertIn("is_valid_uncertainty", measurement_type["procedures"])
        self.assertIn("compare_measurements", measurement_type["procedures"])
        self.assertIn("get_value_public", measurement_type["procedures"]) 
        self.assertIn("is_valid_public", measurement_type["procedures"])
        self.assertIn("internal_helper", measurement_type["procedures"])
        self.assertIn("cleanup", measurement_type["procedures"])
        
        # Test get_value - should be PRIVATE (follows private statement)
        get_value_proc = measurement_type["procedures"]["get_value"]
        self.assertEqual(get_value_proc["pass_type"], PassType.DEFAULT)
        self.assertIsNone(get_value_proc["pass_name"])
        self.assertEqual(get_value_proc["implementation"], "get_measurement_value")
        self.assertEqual(get_value_proc["attributes"], ["PRIVATE"])

        # Test is_valid_uncertainty - should be PRIVATE
        is_valid_proc = measurement_type["procedures"]["is_valid_uncertainty"]
        self.assertEqual(is_valid_proc["pass_type"], PassType.NONE)
        self.assertIsNone(is_valid_proc["pass_name"])
        self.assertIsNone(is_valid_proc["implementation"])
        self.assertEqual(is_valid_proc["attributes"], ["PRIVATE"])
        
        # Test compare_measurements - should be PRIVATE
        compare_proc = measurement_type["procedures"]["compare_measurements"]
        self.assertEqual(compare_proc["pass_type"], PassType.NAMED)
        self.assertEqual(compare_proc["pass_name"], "measurement_a")
        self.assertIsNone(compare_proc["implementation"])
        self.assertEqual(compare_proc["attributes"], ["PRIVATE"])
        
        # Test explicitly public procedures
        get_value_public_proc = measurement_type["procedures"]["get_value_public"]
        self.assertEqual(get_value_public_proc["pass_type"], PassType.DEFAULT)
        self.assertEqual(get_value_public_proc["implementation"], "get_measurement_value")
        self.assertEqual(get_value_public_proc["attributes"], ["PUBLIC"])
        
        is_valid_public_proc = measurement_type["procedures"]["is_valid_public"]
        self.assertEqual(is_valid_public_proc["pass_type"], PassType.DEFAULT)
        self.assertEqual(is_valid_public_proc["implementation"], "is_valid_uncertainty_impl")
        self.assertEqual(is_valid_public_proc["attributes"], ["PUBLIC"])
        
        # Test internal_helper - should be PRIVATE (after public ones, still uses default)
        internal_helper_proc = measurement_type["procedures"]["internal_helper"]
        self.assertEqual(internal_helper_proc["attributes"], ["PRIVATE"])
        
        # Test final procedure - no access attributes
        cleanup_proc = measurement_type["procedures"]["cleanup"]
        self.assertTrue(cleanup_proc["is_final"])
        self.assertEqual(cleanup_proc["attributes"], [])  # Final procedures don't have access specifiers
        
        # Check generic interface
        self.assertIn("operator(==)", measurement_type["generic_interfaces"])
        eq_generic = measurement_type["generic_interfaces"]["operator(==)"]
        self.assertEqual(eq_generic["attributes"], ["PUBLIC"])
        self.assertIn("compare_measurements", eq_generic["specific_procedures"])
        
        # Check module-level functions are still PRIVATE
        self.assertEqual(module["functions"]["get_measurement_value"]["attributes"], ["PRIVATE"])
        self.assertEqual(module["functions"]["is_valid_uncertainty"]["attributes"], ["PRIVATE"])
        self.assertEqual(module["functions"]["is_valid_uncertainty_impl"]["attributes"], ["PRIVATE"])
        self.assertEqual(module["functions"]["compare_measurements"]["attributes"], ["PRIVATE"])
        self.assertEqual(module["subroutines"]["internal_helper"]["attributes"], ["PRIVATE"])
        self.assertEqual(module["subroutines"]["cleanup"]["attributes"], ["PRIVATE"])
      
if __name__ == "__main__":
    unittest.main()
    