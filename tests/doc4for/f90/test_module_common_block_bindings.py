import unittest
from pathlib import Path
from typing import cast
from pyfakefs.fake_filesystem_unittest import TestCase
from doc4for.f90.generate_module_tree import extract_module_data
from doc4for.models.common import BindingTypeEnum, ExpressionType, Expression
from doc4for.models.dimension_models import ArrayBound, BoundType

class TestCommonBlockBindings(TestCase):

    def setUp(self):
        self.setUpPyfakefs()

    def test_common_block_binding(self):
        self.fs.create_file(
            "/fake/path/common_block_binding.f90",
            contents="""\
    module common_block_binding_mod
        use iso_c_binding
        implicit none 

        real(c_double) :: x, y, z
        !!* Basic common block with C binding *!
        common /c_data_block/ x, y, z
        bind(c, name="c_data") :: /c_data_block/
        
        !!* Common block without binding *!
        common /normal_block/ a, b, c
        integer :: a, b, c
        
        real(c_double) :: r, theta, phi
        !!* Common block with binding and unusual spacing *!
        common /coords_block/ r, theta, phi
        bind  (  c  ,  name = "coords_data"  ) :: /coords_block/
        
        integer(c_int) :: flags(10)
        real(c_double) :: values(10)
        !!* Common block with multiple bindings (though unusual) *!
        common /multiblock1/ flags
        common /multiblock2/ values
        bind(c) :: /multiblock1/, /multiblock2/
        
        !!* Common block with binding but no name *!
        common /temp_block/ temps
        real(c_float) :: temps(5)
        bind(c) :: /temp_block/

    contains
        
        !!* Function that uses common block data *!
        function get_x() bind(c) result(val)
            real(c_double) :: val
            val = x  ! Uses the common block variable
        end function

    end module common_block_binding_mod
    """
        )
        
        result = extract_module_data([Path("/fake/path/common_block_binding.f90")])
        self.assertEqual(len(result), 1)
        module = result[0]
        
        # Basic module structure tests
        self.assertEqual(module["module_name"], "common_block_binding_mod")
        self.assertEqual(len(module["common_blocks"]), 6)
        self.assertEqual(len(module["functions"]), 1)
        
        # Test c_data_block binding
        c_block = module["common_blocks"]["c_data_block"]
        self.assertEqual(c_block["name"], "c_data_block")
        self.assertEqual(c_block["description"], "Basic common block with C binding\n")
        self.assertIn("binding_type", c_block)
        self.assertEqual(c_block["binding_type"]["type"], BindingTypeEnum.BIND_C)
        self.assertEqual(c_block["binding_type"]["name"], "c_data")
        
        # Check variables in c_data_block
        self.assertEqual(len(c_block["variables"]), 3)
        self.assertIn("x", c_block["variables"])
        self.assertIn("y", c_block["variables"])
        self.assertIn("z", c_block["variables"])
        
        x_var = c_block["variables"]["x"]
        self.assertEqual(x_var["name"], "x")
        self.assertEqual(x_var["type"], "REAL")
        self.assertEqual(x_var["kind"], "c_double")
        
        # Test normal block - should have no binding
        normal_block = module["common_blocks"]["normal_block"]
        self.assertEqual(normal_block["name"], "normal_block")
        self.assertEqual(normal_block["description"], "Common block without binding\n")
        self.assertIn("binding_type", normal_block)
        self.assertIsNone(normal_block["binding_type"])
        
        # Check variables in normal_block
        self.assertEqual(len(normal_block["variables"]), 3)
        self.assertIn("a", normal_block["variables"])
        self.assertIn("b", normal_block["variables"])
        self.assertIn("c", normal_block["variables"])
        
        a_var = normal_block["variables"]["a"]
        self.assertEqual(a_var["name"], "a")
        self.assertEqual(a_var["type"], "INTEGER")
        self.assertIsNone(a_var.get("kind"))  # No explicit kind specified
        
        # Test unusual spacing
        coords_block = module["common_blocks"]["coords_block"]
        self.assertEqual(coords_block["name"], "coords_block")
        self.assertEqual(coords_block["description"], "Common block with binding and unusual spacing\n")
        self.assertIn("binding_type", coords_block)
        self.assertEqual(coords_block["binding_type"]["type"], BindingTypeEnum.BIND_C)
        self.assertEqual(coords_block["binding_type"]["name"], "coords_data")
        
        # Check variables in coords_block
        self.assertEqual(len(coords_block["variables"]), 3)
        self.assertIn("r", coords_block["variables"])
        self.assertIn("theta", coords_block["variables"])
        self.assertIn("phi", coords_block["variables"])
        
        r_var = coords_block["variables"]["r"]
        self.assertEqual(r_var["name"], "r")
        self.assertEqual(r_var["type"], "REAL")
        self.assertEqual(r_var["kind"], "c_double")
        
        # Test multiblock1 (first of the multiple bindings)
        multiblock1 = module["common_blocks"]["multiblock1"]
        self.assertEqual(multiblock1["name"], "multiblock1")
        self.assertEqual(multiblock1["description"], "Common block with multiple bindings (though unusual)\n")
        self.assertIn("binding_type", multiblock1)
        self.assertEqual(multiblock1["binding_type"]["type"], BindingTypeEnum.BIND_C)
        self.assertIsNone(multiblock1["binding_type"]["name"])  # No explicit name
        
        # Check variables in multiblock1
        self.assertEqual(len(multiblock1["variables"]), 1)
        self.assertIn("flags", multiblock1["variables"])
        
        flags_var = multiblock1["variables"]["flags"]
        self.assertEqual(flags_var["name"], "flags")
        self.assertEqual(flags_var["type"], "INTEGER")
        self.assertEqual(flags_var["kind"], "c_int")
        self.assertEqual(flags_var["dimension"]["dimensions"], 
                        [ArrayBound(BoundType.FIXED, Expression(ExpressionType.LITERAL, "1"), Expression(ExpressionType.LITERAL, "10"))])
        
        # Test multiblock2 (second of the multiple bindings)
        multiblock2 = module["common_blocks"]["multiblock2"]
        self.assertEqual(multiblock2["name"], "multiblock2")
        self.assertEqual(multiblock2["description"], "")  # Should be empty since comment was for multiblock1
        self.assertIn("binding_type", multiblock2)
        self.assertEqual(multiblock2["binding_type"]["type"], BindingTypeEnum.BIND_C)
        self.assertIsNone(multiblock2["binding_type"]["name"])  # No explicit name
        
        # Check variables in multiblock2
        self.assertEqual(len(multiblock2["variables"]), 1)
        self.assertIn("values", multiblock2["variables"])
        
        values_var = multiblock2["variables"]["values"]
        self.assertEqual(values_var["name"], "values")
        self.assertEqual(values_var["type"], "REAL")
        self.assertEqual(values_var["kind"], "c_double")
        self.assertEqual(values_var["dimension"]["dimensions"], 
                        [ArrayBound(BoundType.FIXED, Expression(ExpressionType.LITERAL, "1"), Expression(ExpressionType.LITERAL, "10"))])
        
        # Test temp_block (binding without name)
        temp_block = module["common_blocks"]["temp_block"]
        self.assertEqual(temp_block["name"], "temp_block")
        self.assertEqual(temp_block["description"], "Common block with binding but no name\n")
        self.assertIn("binding_type", temp_block)
        self.assertEqual(temp_block["binding_type"]["type"], BindingTypeEnum.BIND_C)
        self.assertIsNone(temp_block["binding_type"]["name"])  # No explicit name
        
        # Check variables in temp_block
        self.assertEqual(len(temp_block["variables"]), 1)
        self.assertIn("temps", temp_block["variables"])
        
        temps_var = temp_block["variables"]["temps"]
        self.assertEqual(temps_var["name"], "temps")
        self.assertEqual(temps_var["type"], "REAL")
        self.assertEqual(temps_var["kind"], "c_float")
        self.assertEqual(temps_var["dimension"]["dimensions"], 
                        [ArrayBound(BoundType.FIXED, Expression(ExpressionType.LITERAL, "1"), Expression(ExpressionType.LITERAL, "5"))])
        
        # Verify function binding still works normally
        get_x = module["functions"]["get_x"]
        self.assertEqual(get_x["description"], "Function that uses common block data\n")
        self.assertIn("binding_type", get_x)
        self.assertEqual(get_x["binding_type"]["type"], BindingTypeEnum.BIND_C)
        self.assertIsNone(get_x["binding_type"]["name"])  # No explicit name for function
        
        # Check function return type
        self.assertEqual(get_x["return"]["type"], "REAL")
        self.assertEqual(get_x["return"]["kind"], "c_double")
        
        # Verify that all variables are properly associated with their common blocks
        # and that the module-level variables dict contains all variables
        all_common_vars = set()
        for cb_name, cb_data in module["common_blocks"].items():
            all_common_vars.update(cb_data["variables"].keys())
        
        # All these variables should be in the module's variables dict
        expected_vars = {"x", "y", "z", "a", "b", "c", "r", "theta", "phi", "flags", "values", "temps"}
        self.assertEqual(all_common_vars, expected_vars)
        
        # Check that all common block variables are also in the module's variables dict
        for var_name in expected_vars:
            self.assertIn(var_name, module["variables"])


if __name__ == "__main__":
    unittest.main()
