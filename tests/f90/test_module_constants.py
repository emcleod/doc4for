import unittest
from pyfakefs.fake_filesystem_unittest import TestCase
import os
import sys

# Add the project root to the Python path
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), '..', '..')))
from generate_module_tree import process_modules

class TestConstants(TestCase):
    def setUp(self):
        self.setUpPyfakefs()

    def test_find_modules_with_complex_content(self):
        # Create a fake Fortran file with more complex content
        self.fs.create_file(
            '/fake/path/complex.f90',
            contents='''\
!!*
! Module with constants and procedures
!*!
module complex
    ! Constants
    real, parameter :: PI = 3.14159

    ! Functions
    function square(x) result(y)
        real, intent(in) :: x
        real :: y
        y = x * x
    end function square

    ! Subroutines
    subroutine greet(name)
        character(len=*), intent(in) :: name
        print *, 'Hello, ', trim(name), '!'
    end subroutine greet
end module complex
        ''',
        )

        # Call the function with the fake file
        result = process_modules(['/fake/path/complex.f90'])

        # Assertions
        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(module['module_name'], 'complex')
        self.assertEqual(module['file_name'], '/fake/path/complex.f90')
        self.assertEqual(
            module['module_description'], ' Module with constants and procedures\n'
        )
        #self.assertIn('PI', module['constants'])
        self.assertIn('square', module['functions'])
        #self.assertIn('greet', module['subroutines'])

if __name__ == '__main__':
    unittest.main()