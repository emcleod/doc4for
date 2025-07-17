import unittest
from pathlib import Path
from pyfakefs.fake_filesystem_unittest import TestCase
from doc4for.f90.generate_file_tree import extract_file_data
from doc4for.models.common import BindingTypeEnum, Expression, ExpressionType
from doc4for.models.dimension_models import ArrayBound, BoundType

class TestBlockDataBindings(TestCase):
    maxDiff=None

    def setUp(self):
        self.setUpPyfakefs()
                
    def test_block_data_binding(self):
        self.fs.create_file(
            "/fake/path/block_data_binding.f90",
            contents="""\
        !!* Block data with C-bound common blocks *!
        block data c_constants
            use iso_c_binding
            implicit none
            
            !!* Physical constants in C-compatible block *!
            common /math_constants/ pi, e, phi
            real(c_double) :: pi, e, phi
            bind(c, name="c_math_constants") :: /math_constants/
            
            !!* Unit conversion factors *!
            common /conversion_factors/ inch_to_cm, pound_to_kg, gallon_to_liter
            real(c_double) :: inch_to_cm, pound_to_kg, gallon_to_liter
            bind(c) :: /conversion_factors/
            
            !!* System parameters without binding *!
            common /sys_params/ max_files, default_buffer_size
            integer :: max_files, default_buffer_size
            
            data pi /3.14159265358979d0/
            data e /2.71828182845905d0/
            data phi /1.61803398874989d0/
            
            data inch_to_cm /2.54d0/
            data pound_to_kg /0.45359237d0/
            data gallon_to_liter /3.78541178d0/
            
            data max_files /20/
            data default_buffer_size /8192/
        end block data

        !!* Another block data with unusual binding syntax *!
        block data more_constants
            use iso_c_binding
            implicit none
            
            !!* Array constants *!
            common /seq_data/ fibonacci
            integer(c_int) :: fibonacci(10)
            bind  (  c  ,  NAME = "c_sequences"  ) ::  /seq_data/
            
            !!* Common block with mixed-type binding *!
            common /temp_data/ temps
            real(c_float) :: temps(4)
            bind(c, name="c_temp_data") :: /temp_data/
            
            data fibonacci /1, 1, 2, 3, 5, 8, 13, 21, 34, 55/
            data temps /0.0, 32.0, 100.0, 212.0/
        end block data
        """
        )
        
        result = extract_file_data([Path("/fake/path/block_data_binding.f90")])
        self.assertEqual(len(result), 1)
        file_data = result[0]
        self.assertEqual(file_data["file_name"], "/fake/path/block_data_binding.f90")
        self.assertEqual(file_data["file_description"], "Block data with C-bound common blocks\n")
        self.assertEqual(len(file_data["block_data"]), 2)
        block_data = file_data["block_data"]
            
        # First block data: c_constants
        c_constants = block_data["c_constants"]
        self.assertEqual(c_constants["name"], "c_constants")
        self.assertEqual(len(c_constants["common_blocks"]), 3)
        
        # Test math_constants block
        math_block = c_constants["common_blocks"]["math_constants"]
        self.assertEqual(math_block["name"], "math_constants")
        self.assertEqual(math_block["description"], "Physical constants in C-compatible block\n")
        self.assertIn("binding_type", math_block)
        self.assertEqual(math_block["binding_type"]["type"], BindingTypeEnum.BIND_C)
        self.assertEqual(math_block["binding_type"]["name"], "c_math_constants")
        
        # Check variables in math_constants
        self.assertEqual(len(math_block["variables"]), 3)
        self.assertIn("pi", math_block["variables"])
        self.assertIn("e", math_block["variables"])
        self.assertIn("phi", math_block["variables"])
        
        pi_var = math_block["variables"]["pi"]
        self.assertEqual(pi_var["name"], "pi")
        self.assertEqual(pi_var["type"], "REAL")
        self.assertEqual(pi_var["kind"], "c_double")
        
        # Test conversion_factors block
        conv_block = c_constants["common_blocks"]["conversion_factors"]
        self.assertEqual(conv_block["name"], "conversion_factors")
        self.assertEqual(conv_block["description"], "Unit conversion factors\n")
        self.assertIn("binding_type", conv_block)
        self.assertEqual(conv_block["binding_type"]["type"], BindingTypeEnum.BIND_C)
        self.assertIsNone(conv_block["binding_type"]["name"])  # No explicit name given
        
        # Check variables in conversion_factors
        self.assertEqual(len(conv_block["variables"]), 3)
        self.assertIn("inch_to_cm", conv_block["variables"])
        self.assertIn("pound_to_kg", conv_block["variables"])
        self.assertIn("gallon_to_liter", conv_block["variables"])
        
        # Test sys_params block (no binding)
        sys_block = c_constants["common_blocks"]["sys_params"]
        self.assertEqual(sys_block["name"], "sys_params")
        self.assertEqual(sys_block["description"], "System parameters without binding\n")
        self.assertIn("binding_type", sys_block)
        self.assertIsNone(sys_block["binding_type"])
        
        # Check variables in sys_params
        self.assertEqual(len(sys_block["variables"]), 2)
        self.assertIn("max_files", sys_block["variables"])
        self.assertIn("default_buffer_size", sys_block["variables"])
        
        max_files_var = sys_block["variables"]["max_files"]
        self.assertEqual(max_files_var["name"], "max_files")
        self.assertEqual(max_files_var["type"], "INTEGER")
                        
        # Second block data: more_constants
        more_constants = block_data["more_constants"]
        self.assertEqual(more_constants["name"], "more_constants")
        self.assertEqual(more_constants["description"], "Another block data with unusual binding syntax\n")
        self.assertEqual(len(more_constants["common_blocks"]), 2)
        
        # Test seq_data block
        seq_block = more_constants["common_blocks"]["seq_data"]
        self.assertEqual(seq_block["name"], "seq_data")
        self.assertEqual(seq_block["description"], "Array constants\n")
        self.assertIn("binding_type", seq_block)
        self.assertEqual(seq_block["binding_type"]["type"], BindingTypeEnum.BIND_C)
        self.assertEqual(seq_block["binding_type"]["name"], "c_sequences")
        
        # Check fibonacci variable
        self.assertEqual(len(seq_block["variables"]), 1)
        self.assertIn("fibonacci", seq_block["variables"])
        
        fib_var = seq_block["variables"]["fibonacci"]
        self.assertEqual(fib_var["name"], "fibonacci")
        self.assertEqual(fib_var["type"], "INTEGER")
        self.assertEqual(fib_var["kind"], "c_int")
        self.assertEqual(fib_var["dimension"]["dimensions"], 
                         [ArrayBound(BoundType.FIXED, Expression(ExpressionType.LITERAL, "1"), Expression(ExpressionType.LITERAL, "10"))])  
        
        # Test temp_data block
        temp_block = more_constants["common_blocks"]["temp_data"]
        self.assertEqual(temp_block["name"], "temp_data")
        self.assertEqual(temp_block["description"], "Common block with mixed-type binding\n")
        self.assertIn("binding_type", temp_block)
        self.assertEqual(temp_block["binding_type"]["type"], BindingTypeEnum.BIND_C)
        self.assertEqual(temp_block["binding_type"]["name"], "c_temp_data")
        
        # Check temps variable
        self.assertEqual(len(temp_block["variables"]), 1)
        self.assertIn("temps", temp_block["variables"])
        
        temps_var = temp_block["variables"]["temps"]
        self.assertEqual(temps_var["name"], "temps")
        self.assertEqual(temps_var["type"], "REAL")
        self.assertEqual(temps_var["kind"], "c_float")
        self.assertEqual(temps_var["dimension"]["dimensions"], 
                         [ArrayBound(BoundType.FIXED, Expression(ExpressionType.LITERAL, "1"), Expression(ExpressionType.LITERAL, "4"))])
        
        # Test that other_variables is empty (all variables are in common blocks)
        self.assertEqual(len(c_constants["other_variables"]), 0)
        self.assertEqual(len(more_constants["other_variables"]), 0)                


    def test_block_data_mixed_binding(self):
        """Test block data with bind statement that includes both common block and variable."""
        self.fs.create_file(
            "/fake/path/block_data_mixed_binding.f90",
            contents="""\
            !!* Block data with mixed binding targets *!
            block data physics_constants
                use iso_c_binding
                implicit none
                
                !!* Constants and variables for physics calculations *!
                common /physics/ gravity, light_speed
                real(c_double) :: pi = 3.14159265358979d0
                real(c_double) :: gravity, light_speed
                bind(c, name="c_physics_block") :: /physics/, pi
                
                data gravity /9.80665d0/
                data light_speed /299792458.0d0/
            end block data
            """
        )
        
        result = extract_file_data([Path("/fake/path/block_data_mixed_binding.f90")])
        self.assertEqual(len(result), 1)
        file_data = result[0]
        self.assertEqual(file_data["file_name"], "/fake/path/block_data_mixed_binding.f90")
        self.assertEqual(len(file_data["block_data"]), 1)
        
        # Get the block data
        physics_block_data = file_data["block_data"]["physics_constants"]
        self.assertEqual(physics_block_data["name"], "physics_constants")
        
        # Check common block
        self.assertEqual(len(physics_block_data["common_blocks"]), 1)
        physics_common = physics_block_data["common_blocks"]["physics"]
        self.assertEqual(physics_common["name"], "physics")
        self.assertEqual(physics_common["description"], "Constants and variables for physics calculations\n")
        
        # Check that the common block has the binding
        self.assertIn("binding_type", physics_common)
        self.assertEqual(physics_common["binding_type"]["type"], BindingTypeEnum.BIND_C)
        self.assertEqual(physics_common["binding_type"]["name"], "c_physics_block")
        
        # Check variables in common block
        self.assertEqual(len(physics_common["variables"]), 2)
        self.assertIn("gravity", physics_common["variables"])
        self.assertIn("light_speed", physics_common["variables"])
        
        gravity_var = physics_common["variables"]["gravity"]
        self.assertEqual(gravity_var["type"], "REAL")
        self.assertEqual(gravity_var["kind"], "c_double")
        
        light_speed_var = physics_common["variables"]["light_speed"]
        self.assertEqual(light_speed_var["type"], "REAL")
        self.assertEqual(light_speed_var["kind"], "c_double")
        
        # Check if pi appears in other_variables (not in common block)
        # It should be there as a parameter
        self.assertIn("pi", physics_block_data["other_variables"])
        pi_var = physics_block_data["other_variables"]["pi"]
        self.assertEqual(pi_var["name"], "pi")
        self.assertEqual(pi_var["type"], "REAL")
        self.assertEqual(pi_var["kind"], "c_double")
        self.assertEqual(pi_var["initial_value"], "3.14159265358979D0")
        
        # Check if pi also has binding information
        # This is the unusual part - a parameter with a bind attribute
        self.assertIn("binding_type", pi_var)
        self.assertEqual(pi_var["binding_type"]["type"], BindingTypeEnum.BIND_C)
        self.assertEqual(pi_var["binding_type"]["name"], "c_physics_block")
        

    # TODO haven't got parameter information in BlockDataDescription
    # def test_block_data_mixed_binding(self):
    #     """Test block data with bind statement that includes both common block and variable."""
    #     self.fs.create_file(
    #         "/fake/path/block_data_mixed_binding.f90",
    #         contents="""\
    #         !!* Block data with mixed binding targets *!
    #         block data physics_constants
    #             use iso_c_binding
    #             implicit none
                
    #             !!* Constants and variables for physics calculations *!
    #             common /physics/ gravity, light_speed
    #             real(c_double), parameter :: pi = 3.14159265358979d0
    #             real(c_double) :: gravity, light_speed
    #             bind(c, name="c_physics_block") :: /physics/, pi
                
    #             data gravity /9.80665d0/
    #             data light_speed /299792458.0d0/
    #         end block data
    #         """
    #     )
        
    #     result = extract_file_data([Path("/fake/path/block_data_mixed_binding.f90")])
    #     self.assertEqual(len(result), 1)
    #     file_data = result[0]
    #     self.assertEqual(file_data["file_name"], "/fake/path/block_data_mixed_binding.f90")
    #     self.assertEqual(len(file_data["block_data"]), 1)
        
    #     # Get the block data
    #     physics_block_data = file_data["block_data"]["physics_constants"]
    #     self.assertEqual(physics_block_data["name"], "physics_constants")
        
    #     # Check common block
    #     self.assertEqual(len(physics_block_data["common_blocks"]), 1)
    #     physics_common = physics_block_data["common_blocks"]["physics"]
    #     self.assertEqual(physics_common["name"], "physics")
    #     self.assertEqual(physics_common["description"], "Constants and variables for physics calculations\n")
        
    #     # Check that the common block has the binding
    #     self.assertIn("binding_type", physics_common)
    #     self.assertEqual(physics_common["binding_type"]["type"], BindingTypeEnum.BIND_C)
    #     self.assertEqual(physics_common["binding_type"]["name"], "c_physics_block")
        
    #     # Check variables in common block
    #     self.assertEqual(len(physics_common["variables"]), 2)
    #     self.assertIn("gravity", physics_common["variables"])
    #     self.assertIn("light_speed", physics_common["variables"])
        
    #     gravity_var = physics_common["variables"]["gravity"]
    #     self.assertEqual(gravity_var["type"], "REAL")
    #     self.assertEqual(gravity_var["kind"], "c_double")
        
    #     light_speed_var = physics_common["variables"]["light_speed"]
    #     self.assertEqual(light_speed_var["type"], "REAL")
    #     self.assertEqual(light_speed_var["kind"], "c_double")
        
    #     # Check if pi appears in other_variables (not in common block)
    #     # It should be there as a parameter
    #     self.assertIn("pi", physics_block_data["other_variables"])
    #     pi_var = physics_block_data["other_variables"]["pi"]
    #     self.assertEqual(pi_var["name"], "pi")
    #     self.assertEqual(pi_var["type"], "REAL")
    #     self.assertEqual(pi_var["kind"], "c_double")
    #     self.assertEqual(pi_var["initial_value"], "3.14159265358979D0")
        
    #     # Check if pi also has binding information
    #     # This is the unusual part - a parameter with a bind attribute
    #     self.assertIn("binding_type", pi_var)
    #     self.assertEqual(pi_var["binding_type"]["type"], BindingTypeEnum.BIND_C)
    #     self.assertEqual(pi_var["binding_type"]["name"], "c_physics_block")
        
    #     # Check data statements
    #     data_statements = physics_block_data["data_statements"]
    #     self.assertEqual(len(data_statements), 2)
        
    #     gravity_data = next((d for d in data_statements if d["variable"] == "gravity"), None)
    #     self.assertIsNotNone(gravity_data)
    #     self.assertEqual(gravity_data["value"], "9.80665d0")
        
    #     light_speed_data = next((d for d in data_statements if d["variable"] == "light_speed"), None)
    #     self.assertIsNotNone(light_speed_data)
    #     self.assertEqual(light_speed_data["value"], "299792458.0d0")

if __name__ == "__main__":
    unittest.main()

