import unittest
from pathlib import Path
from pyfakefs.fake_filesystem_unittest import TestCase
from doc4for.f90.generate_file_tree import extract_file_data

class TestProgramProcedureCalls(TestCase):
    def setUp(self):
        self.setUpPyfakefs()

    def test_program_single_procedure_call(self):
        self.fs.create_file(
            '/fake/path/main.f90',
            contents='''\
program my_program
    implicit none
    call my_subroutine()
end program my_program
''')
        result = extract_file_data([Path('/fake/path/main.f90')])
        self.assertEqual(len(result), 1)
        file_data = result[0]
        self.assertEqual(file_data['file_name'], '/fake/path/main.f90')
        self.assertEqual(len(file_data['programs']), 1)
        program_details = file_data['programs']['my_program']
        self.assertEqual(program_details['program_name'], 'my_program')
        self.assertEqual(program_details['procedure_calls'], ['my_subroutine'])

    def test_program_multiple_procedure_calls(self):
        self.fs.create_file(
            '/fake/path/main.f90',
            contents='''\
program my_program
    implicit none
    call setup()
    call process_data()
    call cleanup()
end program my_program
''')
        result = extract_file_data([Path('/fake/path/main.f90')])
        self.assertEqual(len(result), 1)
        file_data = result[0]
        self.assertEqual(file_data['file_name'], '/fake/path/main.f90')
        self.assertEqual(len(file_data['programs']), 1)
        program_details = file_data['programs']['my_program']
        self.assertEqual(program_details['program_name'], 'my_program')
        self.assertEqual(program_details['procedure_calls'], ['setup', 'process_data', 'cleanup'])

    def test_program_procedure_calls_with_arguments(self):
        self.fs.create_file(
            '/fake/path/main.f90',
            contents='''\
program my_program
    implicit none
    integer :: x = 5, y = 10
    call calculate(x, y)
    call print_result(x + y)
end program my_program
''')
        result = extract_file_data([Path('/fake/path/main.f90')])
        self.assertEqual(len(result), 1)
        file_data = result[0]
        self.assertEqual(file_data['file_name'], '/fake/path/main.f90')
        self.assertEqual(len(file_data['programs']), 1)
        program_details = file_data['programs']['my_program']
        self.assertEqual(program_details['program_name'], 'my_program')
        self.assertEqual(program_details['procedure_calls'], ['calculate', 'print_result'])

    def test_program_no_procedure_calls(self):
        self.fs.create_file(
            '/fake/path/main.f90',
            contents='''\
program my_program
    implicit none
    print *, "Hello, World!"
end program my_program
''')
        result = extract_file_data([Path('/fake/path/main.f90')])
        self.assertEqual(len(result), 1)
        file_data = result[0]
        self.assertEqual(file_data['file_name'], '/fake/path/main.f90')
        self.assertEqual(len(file_data['programs']), 1)
        program_details = file_data['programs']['my_program']
        self.assertEqual(program_details['program_name'], 'my_program')
        self.assertEqual(program_details['procedure_calls'], [])

    def test_procedure_call_in_separate_file(self):
        self.fs.create_file(
            '/fake/path/main.f90',
            contents='''\
program main_program
    use my_module, only: my_subroutine
    implicit none
    call my_subroutine()
end program main_program
''')
        self.fs.create_file(
            '/fake/path/my_module.f90',
            contents='''\
module my_module
    implicit none
contains
    subroutine my_subroutine()
        print *, "Hello from my_subroutine!"
    end subroutine my_subroutine
end module my_module
''')
        result = extract_file_data([Path('/fake/path/main.f90'), Path('/fake/path/my_module.f90')])
        main_file_data = next(file for file in result if file['file_name'] == '/fake/path/main.f90')
        self.assertEqual(len(main_file_data['programs']), 1)
        program_details = main_file_data['programs']['main_program']
        self.assertEqual(program_details['program_name'], 'main_program')
        self.assertEqual(program_details['procedure_calls'], ['my_subroutine'])
        self.assertEqual(len(program_details['uses']), 1)
        self.assertEqual(program_details['uses']['my_module']['module_name'], 'my_module')
        self.assertEqual(program_details['uses']['my_module']['selections'], ['my_subroutine'])

if __name__ == '__main__':
    unittest.main()