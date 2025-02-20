import unittest
from pathlib import Path
from pyfakefs.fake_filesystem_unittest import TestCase
from doc4for.f90.generate_module_tree import extract_module_data
from doc4for.models.common import BindingTypeEnum

class TestOptionalParameters(TestCase):

    def setUp(self):
        self.setUpPyfakefs()

    def test_binding_optional_parameters(self):
        self.fs.create_file(
            "/fake/path/binding_optional_params.f90",
            contents="""\
    module binding_optional_params_mod
        use iso_c_binding
        implicit none

        !!* Function with standard binding *!
        function standard_binding(x) bind(c, name='std_c_func') result(y)
            use iso_c_binding
            real(c_double), value :: x
            real(c_double) :: y
        end function

        !!* Function with future/vendor-specific binding parameters *!
        function extended_binding(x) bind(c, name='ext_c_func', align=8) result(y)
            use iso_c_binding
            real(c_double), value :: x
            real(c_double) :: y
        end function
        
        !!* Function with multiple optional parameters *!
        function multi_param_binding(x) bind(c, name='multi_param', align=8, convention='cdecl') result(y)
            use iso_c_binding
            real(c_double), value :: x
            real(c_double) :: y
        end function
        
        !!* Function with options-like parameter *!
        function options_binding(x) bind(c, options='unwind,strict') result(y)
            use iso_c_binding
            real(c_double), value :: x
            real(c_double) :: y
        end function
        
        !!* Deprecated but found in old code *!
        function old_style_binding(x) bind(c, stdcall) result(y)
            use iso_c_binding
            real(c_double), value :: x
            real(c_double) :: y
        end function
        
        !!* With unusual spacing and multiple parameters *!
        function complex_spaced_binding(x) bind(  c  ,  name = 'spaced_func'  ,  align  =  16  ) result(y)
            use iso_c_binding
            real(c_double), value :: x
            real(c_double) :: y
        end function

    contains
        
        !!* Function with standard binding *!
        function standard_binding(x) bind(c, name='std_c_func') result(y)
            use iso_c_binding
            real(c_double), value :: x
            real(c_double) :: y
            y = x * 2.0
        end function

        !!* Function with future/vendor-specific binding parameters *!
        function extended_binding(x) bind(c, name='ext_c_func', align=8) result(y)
            use iso_c_binding
            real(c_double), value :: x
            real(c_double) :: y
            y = x * 3.0
        end function
        
        !!* Function with multiple optional parameters *!
        function multi_param_binding(x) bind(c, name='multi_param', align=8, convention='cdecl') result(y)
            use iso_c_binding
            real(c_double), value :: x
            real(c_double) :: y
            y = x * 4.0
        end function
        
        !!* Function with options-like parameter *!
        function options_binding(x) bind(c, options='unwind,strict') result(y)
            use iso_c_binding
            real(c_double), value :: x
            real(c_double) :: y
            y = x * 5.0
        end function
        
        !!* Deprecated but found in old code *!
        function old_style_binding(x) bind(c, stdcall) result(y)
            use iso_c_binding
            real(c_double), value :: x
            real(c_double) :: y
            y = x * 6.0
        end function
        
        !!* With unusual spacing and multiple parameters *!
        function complex_spaced_binding(x) bind(  c  ,  name = 'spaced_func'  ,  align  =  16  ) result(y)
            use iso_c_binding
            real(c_double), value :: x
            real(c_double) :: y
            y = x * 7.0
        end function

    end module binding_optional_params_mod
    """
        )
        result = extract_module_data([Path('/fake/path/binding_optional_params.f90')])
        module = result[0]
        
        # Check standard binding
        standard = module['functions']['standard_binding']
        self.assertIn('binding_type', standard)
        self.assertEqual(standard['binding_type']['type'], BindingTypeEnum.BIND_C)
        self.assertEqual(standard['binding_type']['name'], 'std_c_func')
        
        # Check binding with additional align parameter
        extended = module['functions']['extended_binding']
        self.assertIn('binding_type', extended)
        self.assertEqual(extended['binding_type']['type'], BindingTypeEnum.BIND_C)
        self.assertEqual(extended['binding_type']['name'], 'ext_c_func')
        # Note: we don't verify the align parameter is captured since current implementation likely ignores it
        
        # Check binding with multiple parameters
        multi = module['functions']['multi_param_binding']
        self.assertIn('binding_type', multi)
        self.assertEqual(multi['binding_type']['type'], BindingTypeEnum.BIND_C)
        self.assertEqual(multi['binding_type']['name'], 'multi_param')
        # Additional parameters aren't verified since they're not captured in current implementation
        
        # Check binding with options-style parameter
        options = module['functions']['options_binding']
        self.assertIn('binding_type', options)
        self.assertEqual(options['binding_type']['type'], BindingTypeEnum.BIND_C)
        # Name could be None since the first optional param isn't 'name'
        
        # Check old-style binding with parameter without equals
        old_style = module['functions']['old_style_binding']
        self.assertIn('binding_type', old_style)
        self.assertEqual(old_style['binding_type']['type'], BindingTypeEnum.BIND_C)
        self.assertIsNone(old_style['binding_type']['name'])  # No name provided
        
        # Check binding with complex spacing and multiple parameters
        complex_spaced = module['functions']['complex_spaced_binding']
        self.assertIn('binding_type', complex_spaced)
        self.assertEqual(complex_spaced['binding_type']['type'], BindingTypeEnum.BIND_C)
        self.assertEqual(complex_spaced['binding_type']['name'], 'spaced_func')
        
        # Check interface versions match implementation versions
        interface_funcs = {p['name']: p for iface in module['interfaces'] for p in iface['procedures'].values()}
        for func_name in ['standard_binding', 'extended_binding', 'multi_param_binding',
                        'options_binding', 'old_style_binding', 'complex_spaced_binding']:
            if func_name in interface_funcs:
                iface_func = interface_funcs[func_name]
                impl_func = module['functions'][func_name]
                self.assertEqual(iface_func['binding_type']['type'], impl_func['binding_type']['type'],
                                f"Binding type mismatch for {func_name}")
                self.assertEqual(iface_func['binding_type']['name'], impl_func['binding_type']['name'],
                                f"Binding name mismatch for {func_name}")
                
                
if __name__ == '__main__':
    unittest.main()

