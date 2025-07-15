import unittest
import warnings
import sys
import io
from pathlib import Path
from pyfakefs.fake_filesystem_unittest import TestCase
from fparser.two.utils import FortranSyntaxError
from doc4for.f90.generate_module_tree import extract_module_data
from doc4for.models.common import BindingTypeEnum


class TestOptionalParameters(TestCase):
    """Test binding optional parameters - both valid and invalid cases."""
    
    def setUp(self):
        self.setUpPyfakefs()
        self._created_files = []
        
        # Suppress warnings about unclosed files and exceptions in __del__
        warnings.filterwarnings('ignore', category=ResourceWarning)
        warnings.filterwarnings('ignore', message='Exception ignored in.*')
        
        # Also redirect stderr to suppress __del__ exceptions that bypass warnings
        self._stderr = sys.stderr
        sys.stderr = io.StringIO()
        
    def tearDown(self):
        # Restore stderr
        sys.stderr = self._stderr
        
        # Clean up any files that weren't already cleaned
        for filepath in self._created_files:
            if self.fs.exists(filepath):
                self.fs.remove(filepath)
                
        # Reset warnings
        warnings.resetwarnings()
        
        super().tearDown()
        
    def _create_test_file(self, filepath, contents):
        """Create a test file and track it for cleanup."""
        self.fs.create_file(filepath, contents=contents)
        self._created_files.append(filepath)

    def test_valid_binding_parameters(self):
        """Test that valid BIND(C) parameters work correctly."""
        filepath = "/fake/path/valid_binding_params.f90"
        self._create_test_file(
            filepath,
            contents="""\
module valid_binding_params_mod
    use iso_c_binding
    implicit none

contains
    
    !!* Function with standard binding *!
    function standard_binding(x) bind(c, name='std_c_func') result(y)
        use iso_c_binding
        real(c_double), value :: x
        real(c_double) :: y
        y = x * 2.0
    end function

    !!* Function with just bind(c) *!
    function simple_binding(x) bind(c) result(y)
        use iso_c_binding
        real(c_double), value :: x
        real(c_double) :: y
        y = x * 3.0
    end function
    
    !!* Function with unusual spacing but valid syntax *!
    function spaced_binding(x) bind(  c  ,  name = 'spaced_func'  ) result(y)
        use iso_c_binding
        real(c_double), value :: x
        real(c_double) :: y
        y = x * 4.0
    end function

end module valid_binding_params_mod
"""
        )
        
        result = extract_module_data([Path(filepath)])
        module = result[0]
        
        # Check standard binding
        standard = module['functions']['standard_binding']
        self.assertIn('binding_type', standard)
        self.assertEqual(standard['binding_type']['type'], BindingTypeEnum.BIND_C)
        self.assertEqual(standard['binding_type']['name'], 'std_c_func')
        
        # Check simple binding without name
        simple = module['functions']['simple_binding']
        self.assertIn('binding_type', simple)
        self.assertEqual(simple['binding_type']['type'], BindingTypeEnum.BIND_C)
        self.assertIsNone(simple['binding_type']['name'])
        
        # Check binding with unusual spacing
        spaced = module['functions']['spaced_binding']
        self.assertIn('binding_type', spaced)
        self.assertEqual(spaced['binding_type']['type'], BindingTypeEnum.BIND_C)
        self.assertEqual(spaced['binding_type']['name'], 'spaced_func')

    def test_binding_with_align_parameter_fails(self):
        """Test that bind(c) with align parameter is not supported."""
        filepath = "/fake/path/align_binding.f90"
        self._create_test_file(
            filepath,
            contents="""\
module align_binding_mod
    use iso_c_binding
    implicit none

contains
    
    function extended_binding(x) bind(c, name='ext_c_func', align=8) result(y)
        use iso_c_binding
        real(c_double), value :: x
        real(c_double) :: y
        y = x * 3.0
    end function

end module align_binding_mod
"""
        )
        
        with self.assertRaises(FortranSyntaxError) as context:
            extract_module_data([Path(filepath)])
        
        self.assertIn("line 7", str(context.exception))

    def test_binding_with_multiple_extra_parameters_fails(self):
        """Test that bind(c) with multiple non-standard parameters is not supported."""
        filepath = "/fake/path/multi_param_binding.f90"
        self._create_test_file(
            filepath,
            contents="""\
module multi_param_binding_mod
    use iso_c_binding
    implicit none

contains
    
    function multi_param_binding(x) bind(c, name='multi_param', align=8, convention='cdecl') result(y)
        use iso_c_binding
        real(c_double), value :: x
        real(c_double) :: y
        y = x * 4.0
    end function

end module multi_param_binding_mod
"""
        )
        
        with self.assertRaises(FortranSyntaxError) as context:
            extract_module_data([Path(filepath)])
        
        self.assertIn("line 7", str(context.exception))

    def test_binding_with_options_parameter_fails(self):
        """Test that bind(c) with options parameter is not supported."""
        filepath = "/fake/path/options_binding.f90"
        self._create_test_file(
            filepath,
            contents="""\
module options_binding_mod
    use iso_c_binding
    implicit none

contains
    
    function options_binding(x) bind(c, options='unwind,strict') result(y)
        use iso_c_binding
        real(c_double), value :: x
        real(c_double) :: y
        y = x * 5.0
    end function

end module options_binding_mod
"""
        )
        
        with self.assertRaises(FortranSyntaxError) as context:
            extract_module_data([Path(filepath)])
        
        self.assertIn("line 7", str(context.exception))

    def test_old_style_binding_parameter_fails(self):
        """Test that old-style bind(c, stdcall) without equals is not supported."""
        filepath = "/fake/path/old_style_binding.f90"
        self._create_test_file(
            filepath,
            contents="""\
module old_style_binding_mod
    use iso_c_binding
    implicit none

contains
    
    function old_style_binding(x) bind(c, stdcall) result(y)
        use iso_c_binding
        real(c_double), value :: x
        real(c_double) :: y
        y = x * 6.0
    end function

end module old_style_binding_mod
"""
        )
        
        with self.assertRaises(FortranSyntaxError) as context:
            extract_module_data([Path(filepath)])
        
        self.assertIn("line 7", str(context.exception))

    def test_complex_spaced_binding_with_align_fails(self):
        """Test that bind(c) with spacing and align parameter is not supported."""
        filepath = "/fake/path/complex_spaced_binding.f90"
        self._create_test_file(
            filepath,
            contents="""\
module complex_spaced_binding_mod
    use iso_c_binding
    implicit none

contains
    
    function complex_spaced_binding(x) bind(  c  ,  name = 'spaced_func'  ,  align  =  16  ) result(y)
        use iso_c_binding
        real(c_double), value :: x
        real(c_double) :: y
        y = x * 7.0
    end function

end module complex_spaced_binding_mod
"""
        )
        
        with self.assertRaises(FortranSyntaxError) as context:
            extract_module_data([Path(filepath)])
        
        self.assertIn("line 7", str(context.exception))

    def test_interface_with_invalid_binding_fails(self):
        """Test that interfaces with invalid binding parameters also fail."""
        filepath = "/fake/path/interface_invalid_binding.f90"
        self._create_test_file(
            filepath,
            contents="""\
module interface_invalid_binding_mod
    use iso_c_binding
    implicit none

    interface
        function interface_func(x) bind(c, name='iface_func', align=8) result(y)
            use iso_c_binding
            real(c_double), value :: x
            real(c_double) :: y
        end function
    end interface

end module interface_invalid_binding_mod
"""
        )
        
        with self.assertRaises(FortranSyntaxError) as context:
            extract_module_data([Path(filepath)])
        
        self.assertIn("line 6", str(context.exception))


if __name__ == '__main__':
    unittest.main()