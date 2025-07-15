import unittest
import warnings
import sys
import io
from pathlib import Path
from pyfakefs.fake_filesystem_unittest import TestCase
from fparser.two.utils import FortranSyntaxError
from doc4for.f90.generate_module_tree import extract_module_data
from doc4for.models.common import BindingTypeEnum


class TestNonStandardBindings(TestCase):
    """Test that non-standard bindings fail as expected.
    
    These tests document that fparser2 correctly rejects non-standard
    binding syntax. If any of these tests fail (i.e., don't raise errors),
    it means fparser2 has changed to support these constructs.
    """
    
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
        
    def test_java_binding_fails(self):
        """Test that Java binding is not supported."""
        filepath = "/fake/path/java_binding.f90"
        self._create_test_file(
            filepath,
            contents="""\
module java_binding_mod
    implicit none
    
    interface
        function java_func(x) bind(java, name='JavaFunction') result(y)
            real, value :: x
            real :: y
        end function
    end interface
    
end module java_binding_mod
"""
        )
        
        with self.assertRaises(FortranSyntaxError) as context:
            extract_module_data([Path(filepath)])
        
        self.assertIn("line 5", str(context.exception))
        
    def test_python_binding_fails(self):
        """Test that Python binding is not supported."""
        filepath = "/fake/path/python_binding.f90"
        self._create_test_file(
            filepath,
            contents="""\
module python_binding_mod
    implicit none
    
    function py_func(x) bind(python, name='py_function') result(y)
        real, value :: x
        real :: y
        y = x * 3.0
    end function
    
end module python_binding_mod
"""
        )
        
        with self.assertRaises(FortranSyntaxError) as context:
            extract_module_data([Path(filepath)])
        
        self.assertIn("line 4", str(context.exception))
        
    def test_binding_with_extra_parameters_fails(self):
        """Test that bind(c) with extra parameters is not supported."""
        filepath = "/fake/path/extra_params.f90"
        self._create_test_file(
            filepath,
            contents="""\
module extra_params_mod
    implicit none
    
    subroutine unusual_binding(x) bind(c, version=2)
        real, intent(inout) :: x
        x = x * 2.0
    end subroutine
    
end module extra_params_mod
"""
        )
        
        with self.assertRaises(FortranSyntaxError) as context:
            extract_module_data([Path(filepath)])
        
        self.assertIn("line 4", str(context.exception))
        
    def test_multiple_language_binding_fails(self):
        """Test that multiple language bindings are not supported."""
        filepath = "/fake/path/multi_lang.f90"
        self._create_test_file(
            filepath,
            contents="""\
module multi_lang_mod
    implicit none
    
    subroutine multi_lang(x) bind(c, java, python)
        real, intent(inout) :: x
        x = x * 3.0
    end subroutine
    
end module multi_lang_mod
"""
        )
        
        with self.assertRaises(FortranSyntaxError) as context:
            extract_module_data([Path(filepath)])
        
        self.assertIn("line 4", str(context.exception))
        
    def test_complex_binding_expression_fails(self):
        """Test that complex binding expressions are not supported."""
        filepath = "/fake/path/complex_binding.f90"
        self._create_test_file(
            filepath,
            contents="""\
module complex_binding_mod
    implicit none
    
    function complex_binding(x) bind(language=c, options='strict') result(y)
        real, intent(in) :: x
        real :: y
        y = x * 4.0
    end function
    
end module complex_binding_mod
"""
        )
        
        with self.assertRaises(FortranSyntaxError) as context:
            extract_module_data([Path(filepath)])
        
        self.assertIn("line 4", str(context.exception))
        
    def test_empty_binding_fails(self):
        """Test that empty bind() is not supported."""
        filepath = "/fake/path/empty_binding.f90"
        self._create_test_file(
            filepath,
            contents="""\
module empty_binding_mod
    implicit none
    
    function empty_binding(x) bind() result(y)
        real, intent(in) :: x
        real :: y
        y = x * 5.0
    end function
    
end module empty_binding_mod
"""
        )
        
        with self.assertRaises(FortranSyntaxError) as context:
            extract_module_data([Path(filepath)])
        
        self.assertIn("line 4", str(context.exception))
        
    def test_valid_c_binding_works(self):
        """Control test: verify that valid BIND(C) syntax works."""
        filepath = "/fake/path/valid_binding.f90"
        self._create_test_file(
            filepath,
            contents="""\
module valid_binding_mod
    implicit none
    
contains
    
    function c_func(x) bind(c, name='c_function') result(y)
        real, value :: x
        real :: y
        y = x * 2.0
    end function
    
    subroutine c_sub(x) bind(c)
        real, intent(inout) :: x
        x = x * 3.0
    end subroutine
    
end module valid_binding_mod
"""
        )
        
        # This should NOT raise an error
        result = extract_module_data([Path(filepath)])
        module = result[0]
        
        # Verify the C bindings were parsed correctly
        c_func = module['functions']['c_func']
        self.assertEqual(c_func['binding_type']['type'], BindingTypeEnum.BIND_C)
        self.assertEqual(c_func['binding_type']['name'], 'c_function')
        
        c_sub = module['subroutines']['c_sub']
        self.assertEqual(c_sub['binding_type']['type'], BindingTypeEnum.BIND_C)
        self.assertIsNone(c_sub['binding_type']['name'])


if __name__ == '__main__':
    unittest.main()