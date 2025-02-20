import unittest
from pathlib import Path
from pyfakefs.fake_filesystem_unittest import TestCase
from doc4for.f90.generate_module_tree import extract_module_data
from doc4for.models.common import BindingTypeEnum

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

        !!* Basic common block with C binding *!
        real(c_double) :: x, y, z
        common /c_data_block/ x, y, z
        bind(c, name='c_data') /c_data_block/
        
        !!* Common block without binding *!
        integer :: a, b, c
        common /normal_block/ a, b, c
        
        !!* Common block with binding and unusual spacing *!
        real(c_double) :: r, theta, phi
        common /coords_block/ r, theta, phi
        bind  (  c  ,  name = 'coords_data'  )  /coords_block/
        
        !!* Common block with multiple bindings (though unusual) *!
        integer(c_int) :: flags(10)
        real(c_double) :: values(10)
        common /multiblock1/ flags
        common /multiblock2/ values
        bind(c) /multiblock1/, /multiblock2/
        
        !!* Common block with binding but no name *!
        real(c_float) :: temps(5)
        common /temp_block/ temps
        bind(c) /temp_block/

    contains
        
        !!* Function that uses common block data *!
        function get_x() bind(c) result(val)
            real(c_double) :: val
            val = x  ! Uses the common block variable
        end function

    end module common_block_binding_mod
    """
        )
        
        # This test is expected to fail initially, so we can use try/except to document 
        # the current behavior without breaking the test suite
        try:
            self.assertTrue(False) # want it to fail
            result = extract_module_data([Path('/fake/path/common_block_binding.f90')])
            module = result[0]
            
            # If we get here, common blocks are being parsed. Let's check binding types:
            if 'common_blocks' in module:
                # Test c_data_block binding
                c_block = next((block for block in module['common_blocks'] 
                            if block['name'] == 'c_data_block'), None)
                if c_block:
                    self.assertIn('binding_type', c_block)
                    self.assertEqual(c_block['binding_type']['type'], BindingTypeEnum.BIND_C)
                    self.assertEqual(c_block['binding_type']['name'], 'c_data')
                
                # Test normal block - should have default binding
                normal_block = next((block for block in module['common_blocks'] 
                                    if block['name'] == 'normal_block'), None)
                if normal_block:
                    self.assertIn('binding_type', normal_block)
                    self.assertEqual(normal_block['binding_type']['type'], BindingTypeEnum.DEFAULT)
                    self.assertIsNone(normal_block['binding_type']['name'])
                
                # Test unusual spacing
                coords_block = next((block for block in module['common_blocks'] 
                                if block['name'] == 'coords_block'), None)
                if coords_block:
                    self.assertIn('binding_type', coords_block)
                    self.assertEqual(coords_block['binding_type']['type'], BindingTypeEnum.BIND_C)
                    self.assertEqual(coords_block['binding_type']['name'], 'coords_data')
            
            # Verify function binding still works normally
            get_x = module['functions']['get_x']
            self.assertEqual(get_x['binding_type']['type'], BindingTypeEnum.BIND_C)
            
        except Exception as e:
            # Since we expect this to fail initially, we'll just print the error
            print(f"Expected failure in common block test: {e}")
            # Consider this a passing test since we expect failure
            self.assertTrue(False)            

if __name__ == '__main__':
    unittest.main()
