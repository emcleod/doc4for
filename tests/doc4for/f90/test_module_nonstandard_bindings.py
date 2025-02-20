import unittest
from pathlib import Path
from pyfakefs.fake_filesystem_unittest import TestCase
from doc4for.f90.generate_module_tree import extract_module_data
from doc4for.models.common import BindingTypeEnum

class TestNonStandardBindings(TestCase):
    # just documenting what we do for now - this should fail if we support
    # bindings other than C and default
    
    def setUp(self):
        self.setUpPyfakefs()
        
    def test_nonstandard_binding_types(self):
        self.fs.create_file(
            "/fake/path/nonstandard_binding.f90",
            contents="""\
    module nonstandard_binding_mod
        implicit none
        
        ! Note: These non-standard bindings are for testing only - they're not valid Fortran
        
        !!* Java binding - not standard Fortran but might be in vendor extensions *!
        interface
            function java_func(x) bind(java, name='JavaFunction') result(y)
                real, value :: x
                real :: y
            end function
        end interface
        
        !!* Python binding - completely fictitious for testing *!
        function py_func(x) bind(python, name='py_function') result(y)
            real, value :: x
            real :: y
        end function
        
        !!* Binding with unusual parameter - not standard *!
        subroutine unusual_binding(x) bind(c, version=2)
            real, intent(inout) :: x
        end subroutine
        
        !!* Multiple language binding - definitely not standard *!
        subroutine multi_lang(x) bind(c, java, python)
            real, intent(inout) :: x
        end subroutine
        
        !!* Complex binding expression - not standard *!
        function complex_binding(x) bind(language=c, options='strict') result(y)
            real, intent(in) :: x
            real :: y
        end function
        
        !!* Empty binding - not valid but testing parser robustness *!
        function empty_binding(x) bind() result(y)
            real, intent(in) :: x
            real :: y
        end function
        
    contains
        
        !!* Implementation of non-standard bound functions *!
        function java_func(x) bind(java, name='JavaFunction') result(y)
            real, value :: x
            real :: y
            y = x * 2.0
        end function
        
        function py_func(x) bind(python, name='py_function') result(y)
            real, value :: x
            real :: y
            y = x * 3.0
        end function
        
        subroutine unusual_binding(x) bind(c, version=2)
            real, intent(inout) :: x
            x = x * 2.0
        end subroutine
        
        subroutine multi_lang(x) bind(c, java, python)
            real, intent(inout) :: x
            x = x * 3.0
        end subroutine
        
        function complex_binding(x) bind(language=c, options='strict') result(y)
            real, intent(in) :: x
            real :: y
            y = x * 4.0
        end function
        
        function empty_binding(x) bind() result(y)
            real, intent(in) :: x
            real :: y
            y = x * 5.0
        end function

    end module nonstandard_binding_mod
    """
        )
        result = extract_module_data([Path('/fake/path/nonstandard_binding.f90')])
        module = result[0]
        
        # Check Java binding - should be treated as DEFAULT in current implementation
        java_func = module['functions']['java_func']
        self.assertIn('binding_type', java_func)
        self.assertEqual(java_func['binding_type']['type'], BindingTypeEnum.DEFAULT)
        self.assertIsNone(java_func['binding_type']['name'])
        
        # Check Python binding - should also be treated as DEFAULT
        py_func = module['functions']['py_func']
        self.assertIn('binding_type', py_func)
        self.assertEqual(py_func['binding_type']['type'], BindingTypeEnum.DEFAULT)
        self.assertIsNone(py_func['binding_type']['name'])
        
        # Check unusual binding with extra parameters - might extract the 'c' part
        unusual = module['subroutines']['unusual_binding']
        self.assertIn('binding_type', unusual)
        # This could be either DEFAULT or BIND_C depending on how robust your parser is
        # We'll accept either behavior for now
        binding_type = unusual['binding_type']['type']
        self.assertTrue(binding_type == BindingTypeEnum.DEFAULT or 
                    binding_type == BindingTypeEnum.BIND_C,
                    f"Expected DEFAULT or BIND_C, got {binding_type}")
        
        # Check multi-language binding - might extract just the 'c' part
        multi = module['subroutines']['multi_lang']
        self.assertIn('binding_type', multi)
        # This could be either DEFAULT or BIND_C depending on implementation
        binding_type = multi['binding_type']['type']
        self.assertTrue(binding_type == BindingTypeEnum.DEFAULT or 
                    binding_type == BindingTypeEnum.BIND_C,
                    f"Expected DEFAULT or BIND_C, got {binding_type}")
        
        # Check complex binding expression
        complex_bind = module['functions']['complex_binding']
        self.assertIn('binding_type', complex_bind)
        # Should be DEFAULT as the complex expression likely won't be recognized
        self.assertEqual(complex_bind['binding_type']['type'], BindingTypeEnum.DEFAULT)
        
        # Check empty binding - should definitely be DEFAULT
        empty = module['functions']['empty_binding']
        self.assertIn('binding_type', empty)
        self.assertEqual(empty['binding_type']['type'], BindingTypeEnum.DEFAULT)
        
        # Check interface function
        interface = module['interfaces'][0]
        java_interface_func = interface['procedures']['java_func']
        self.assertIn('binding_type', java_interface_func)
        self.assertEqual(java_interface_func['binding_type']['type'], BindingTypeEnum.DEFAULT)


if __name__ == '__main__':
    unittest.main()
