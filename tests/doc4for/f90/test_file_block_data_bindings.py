import unittest
from pathlib import Path
from pyfakefs.fake_filesystem_unittest import TestCase
from doc4for.f90.generate_file_tree import extract_file_data
from doc4for.models.common import BindingTypeEnum

class TestBlockDataBindings(TestCase):

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
        real(c_double) :: pi, e, phi
        common /math_constants/ pi, e, phi
        bind(c, name="c_math_constants") /math_constants/
        
        !!* Unit conversion factors *!
        real(c_double) :: inch_to_cm, pound_to_kg, gallon_to_liter
        common /conversion_factors/ inch_to_cm, pound_to_kg, gallon_to_liter
        bind(c) /conversion_factors/
        
        !!* System parameters without binding *!
        integer :: max_files, default_buffer_size
        common /sys_params/ max_files, default_buffer_size
        
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
        integer(c_int) :: fibonacci(10)
        common /seq_data/ fibonacci
        bind  (  c  ,  NAME = "c_sequences"  )  /seq_data/
        
        !!* Common block with mixed-type binding *!
        real(c_float) :: temps(4)
        common /temp_data/ temps
        bind(c, name="c_temp_data") /temp_data/
        
        data fibonacci /1, 1, 2, 3, 5, 8, 13, 21, 34, 55/
        data temps /0.0, 32.0, 100.0, 212.0/
    end block data
    """
        )
        
        result = extract_file_data([Path("/fake/path/block_data_binding.f90")])
        self.assertEqual(len(result), 1)
        file_data = result[0]
        self.assertEqual(file_data["file_name"], "/fake/path/block_data_binding.f90")
        self.assertEqual(len(file_data["block_data"]), 2)
        block_data = file_data["block_data"]
            
        # First block data: c_constants
        c_constants = block_data["c_constants"]
        # Test math_constants block
        math_block = c_constants["common_blocks"]["math_constants"]
        self.assertIn("binding_type", math_block)
        self.assertEqual(math_block["binding_type"]["type"], BindingTypeEnum.BIND_C)
        self.assertEqual(math_block["binding_type"]["name"], "c_math_constants")
            
        # Test conversion_factors block
        conv_block = c_constants["common_blocks"]["conversion_factors"]
        self.assertIn("binding_type", conv_block)
        self.assertEqual(conv_block["binding_type"]["type"], BindingTypeEnum.BIND_C)
            
        sys_block = c_constants["common_blocks"]["sys_params"]
        self.assertIn("binding_type", sys_block)
        self.assertEqual(sys_block["binding_type"]["type"], BindingTypeEnum.DEFAULT)
        
        # Second block data: more_constants
        more_constants = block_data["more_constants"]
        seq_block = more_constants["common_blocks"]["seq_data"]
        self.assertIn("binding_type", seq_block)
        self.assertEqual(seq_block["binding_type"]["type"], BindingTypeEnum.BIND_C)
        self.assertEqual(seq_block["binding_type"]["name"], "c_sequences")
                

if __name__ == "__main__":
    unittest.main()

# TODO note variable is bound as well - probably will compile but not really used
# block data
# real(c_double), parameter :: pi = 3.14159265358979d0
# real(c_double) :: gravity, light_speed
# common /physics/ gravity, light_speed
# bind(c, name="c_physics_block") :: /physics/, pi
#end block data