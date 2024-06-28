import unittest
from pathlib import Path
from pyfakefs.fake_filesystem_unittest import TestCase
from doc4for.f90.generate_module_tree import extract_module_data

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
        result = extract_module_data([Path('/fake/path/complex.f90')])

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

#     def test_parameters_defined_in_program(self):
#         self.fs.create_file(
#             '/fake/path/parameters.f90',
#             contents='''\
# program global_parameters_program
#     implicit none

#     !!* Defines pi *!
#     real, parameter :: PI = 3.14159265358979323846
#     integer, parameter :: MAX_ITERATIONS = 1000

#     ! Rest of the program code

# end program global_parameters_program
# ''')
#         result = extract_file_data([Path('/fake/path/parameters.f90')])
#         self.assertEqual(len(result), 1)
#         file_data = result[0]
#         # self.assertEqual(file_data['file_name'], '/fake/path/parameters.f90')
#         # self.assertEqual(len(file_data['parameters']), 2)
#         # self.assertIn('PI', file_data['constants'])
#         # pi_info = file_data['constants']['PI']['details']
#         # self.assertEqual(pi_info['type'], 'real')
#         # self.assertEqual(pi_info['name'], 'PI')
#         # self.assertEqual(pi_info['value'], '3.14159265358979323846')
#         # self.assertIn('MAX_ITERATIONS', file_data['constants'])
#         # max_iter_info = file_data['constants']['MAX_ITERATIONS']['details']
#         # self.assertEqual(max_iter_info['type'], 'integer')
#         # self.assertEqual(max_iter_info['name'], 'MAX_ITERATIONS')
#         # self.assertEqual(max_iter_info['value'], '1000')
#         # self.assertEqual(pi_info['description'], 'Defines pi\n')
#         # self.assertEqual(max_iter_info['description'], '')

if __name__ == '__main__':
    unittest.main()