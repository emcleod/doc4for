import unittest
from pathlib import Path
from pyfakefs.fake_filesystem_unittest import TestCase
from doc4for.f90.generate_module_tree import extract_module_data
from doc4for.models.common import BindingTypeEnum

class TestGlobalVariableBindings(TestCase):

    def setUp(self):
        self.setUpPyfakefs()

    def test_variable_binding(self):
        self.fs.create_file(
            "/fake/path/variable_binding.f90",
            contents="""\
    module variable_binding_mod
        use iso_c_binding
        implicit none

        !!* Integer with C binding and explicit name *!
        integer(c_int), bind(c, name='c_counter') :: counter = 0

        !!* Double precision with C binding but default name *!
        real(c_double), bind(c) :: pi = 3.14159265358979

        !!* Array with C binding *!
        real(c_float), dimension(3), bind(c, name='c_coords') :: coordinates = [0.0, 0.0, 0.0]
        
        !!* Character with C binding *!
        character(kind=c_char, len=20), bind(c) :: c_string = "Hello from Fortran"
        
        !!* Variable without binding *!
        integer :: normal_counter = 0
        
        !!* Variable with binding and multiple attributes *!
        integer(c_int), parameter, bind(c, name='MAX_SIZE') :: max_size = 100
        
        !!* Variable with unusual spacing/capitalization in binding *!
        real(c_double), bind  (  C  ,  NAME = 'weird_spacing'  ) :: weird_var = 1.0

    end module variable_binding_mod
    """
        )
        result = extract_module_data([Path('/fake/path/variable_binding.f90')])
        module = result[0]
        
        # Check counter with explicit name
        counter = module['variables']['counter']
        self.assertIn('binding_type', counter)
        self.assertEqual(counter['binding_type']['type'], BindingTypeEnum.BIND_C)
        self.assertEqual(counter['binding_type']['name'], 'c_counter')
        
        # Check pi with default name
        pi = module['variables']['pi']
        self.assertIn('binding_type', pi)
        self.assertEqual(pi['binding_type']['type'], BindingTypeEnum.BIND_C)
        self.assertIsNone(pi['binding_type']['name'])
        
        # Check array with binding
        coords = module['variables']['coordinates']
        self.assertIn('binding_type', coords)
        self.assertEqual(coords['binding_type']['type'], BindingTypeEnum.BIND_C)
        self.assertEqual(coords['binding_type']['name'], 'c_coords')
        
        # Check character with binding
        c_string = module['variables']['c_string']
        self.assertIn('binding_type', c_string)
        self.assertEqual(c_string['binding_type']['type'], BindingTypeEnum.BIND_C)
        self.assertIsNone(c_string['binding_type']['name'])
        
        # Check variable without binding
        normal = module['variables']['normal_counter']
        self.assertIn('binding_type', normal)
        self.assertEqual(normal['binding_type']['type'], BindingTypeEnum.DEFAULT)
        self.assertIsNone(normal['binding_type']['name'])
        
        # Check variable with binding and other attributes
        max_size = module['parameters']['max_size']
        self.assertIn('binding_type', max_size)
        self.assertEqual(max_size['binding_type']['type'], BindingTypeEnum.BIND_C)
        self.assertEqual(max_size['binding_type']['name'], 'MAX_SIZE')
        self.assertIn('parameter', max_size['attributes'])
        
        # Check variable with unusual spacing/capitalization
        weird = module['variables']['weird_var']
        self.assertIn('binding_type', weird)
        self.assertEqual(weird['binding_type']['type'], BindingTypeEnum.BIND_C)
        self.assertEqual(weird['binding_type']['name'], 'weird_spacing')
    
if __name__ == '__main__':
    unittest.main()

