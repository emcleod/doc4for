import unittest
from pathlib import Path
from pyfakefs.fake_filesystem_unittest import TestCase
from doc4for.f90.generate_file_tree import extract_file_data

class TestProgramUses(TestCase):
    def setUp(self):
        self.setUpPyfakefs()

    def test_program_single_use(self):
        self.fs.create_file(
            '/fake/path/main.f90',
            contents='''\
program my_program
    implicit none
    use other_module
    ! Program code
end program my_program
''')
        result = extract_file_data([Path('/fake/path/main.f90')])
        self.assertEqual(len(result), 1)
        file_data = result[0]
        self.assertEqual(file_data['file_name'], '/fake/path/main.f90')
        self.assertEqual(len(file_data['programs']), 1)
        program_details = file_data['programs']['my_program']
        self.assertEqual(program_details['program_name'], 'my_program')
        self.assertEqual(len(program_details['uses']), 1)
        self.assertEqual(program_details['uses']['other_module']['module_name'], 'other_module')
        self.assertEqual(program_details['uses']['other_module']['selections'], [])

    def test_program_multiple_use(self):
        self.fs.create_file(
            '/fake/path/main.f90',
            contents='''\
program my_program
    implicit none
    use other_module
    use test_module
    ! Program code
end program my_program
''')
        result = extract_file_data([Path('/fake/path/main.f90')])
        self.assertEqual(len(result), 1)
        file_data = result[0]
        self.assertEqual(file_data['file_name'], '/fake/path/main.f90')
        self.assertEqual(len(file_data['programs']), 1)
        program_details = file_data['programs']['my_program']
        self.assertEqual(program_details['program_name'], 'my_program')
        self.assertEqual(len(program_details['uses']), 2)
        self.assertEqual(program_details['uses']['other_module']['module_name'], 'other_module')
        self.assertEqual(program_details['uses']['other_module']['selections'], [])
        self.assertEqual(program_details['uses']['test_module']['module_name'], 'test_module')
        self.assertEqual(program_details['uses']['test_module']['selections'], [])

    def test_program_use_only_single(self):
        self.fs.create_file(
            '/fake/path/main.f90',
            contents='''\
program my_program
    implicit none
    use iso_fortran_env, only: real64
end program my_program
''')
        result = extract_file_data([Path('/fake/path/main.f90')])
        self.assertEqual(len(result), 1)
        file_data = result[0]
        self.assertEqual(file_data['file_name'], '/fake/path/main.f90')
        self.assertEqual(len(file_data['programs']), 1)
        program_details = file_data['programs']['my_program']
        self.assertEqual(len(program_details['uses']), 1)
        self.assertEqual(program_details['uses']['iso_fortran_env']['module_name'], 'iso_fortran_env')
        self.assertEqual(program_details['uses']['iso_fortran_env']['selections'], ['real64'])

    def test_program_use_only_multiple(self):
        self.fs.create_file(
            '/fake/path/main.f90',
            contents='''\
program my_program
    implicit none
    use iso_fortran_env, only: real64, int32
end program my_program
''')
        result = extract_file_data([Path('/fake/path/main.f90')])
        self.assertEqual(len(result), 1)
        file_data = result[0]
        self.assertEqual(file_data['file_name'], '/fake/path/main.f90')
        self.assertEqual(len(file_data['programs']), 1)
        program_details = file_data['programs']['my_program']
        self.assertEqual(len(program_details['uses']), 1)
        self.assertEqual(program_details['uses']['iso_fortran_env']['module_name'], 'iso_fortran_env')
        self.assertEqual(program_details['uses']['iso_fortran_env']['selections'], ['real64', 'int32'])

    def test_program_use_many_only(self):
        self.fs.create_file(
            '/fake/path/main.f90',
            contents='''\
program my_program
    implicit none
    use iso_fortran_env, only: real64, int32
    use my_module, only: test1
end program my_program
''')
        result = extract_file_data([Path('/fake/path/main.f90')])
        self.assertEqual(len(result), 1)
        file_data = result[0]
        self.assertEqual(file_data['file_name'], '/fake/path/main.f90')
        self.assertEqual(len(file_data['programs']), 1)
        program_details = file_data['programs']['my_program']
        self.assertEqual(len(program_details['uses']), 2)
        self.assertEqual(program_details['uses']['iso_fortran_env']['module_name'], 'iso_fortran_env')
        self.assertEqual(program_details['uses']['iso_fortran_env']['selections'], ['real64', 'int32'])
        self.assertEqual(program_details['uses']['my_module']['selections'], ['test1'])

    def test_program_use_many_only_repeated_declarations(self):
        self.fs.create_file(
            '/fake/path/main.f90',
            contents='''\
program my_program
    implicit none
    use iso_fortran_env, only: real64, int32
    use my_module, only: test1
    use iso_fortran_env, only: real32
end program my_program
''')
        result = extract_file_data([Path('/fake/path/main.f90')])
        self.assertEqual(len(result), 1)
        file_data = result[0]
        self.assertEqual(file_data['file_name'], '/fake/path/main.f90')
        self.assertEqual(len(file_data['programs']), 1)
        program_details = file_data['programs']['my_program']
        self.assertEqual(len(program_details['uses']), 2)
        self.assertEqual(program_details['uses']['iso_fortran_env']['module_name'], 'iso_fortran_env')
        self.assertEqual(program_details['uses']['iso_fortran_env']['selections'], ['real64', 'int32', 'real32'])
        self.assertEqual(program_details['uses']['my_module']['module_name'], 'my_module')
        self.assertEqual(program_details['uses']['my_module']['selections'], ['test1'])

    def test_program_repeated_use_removes_selections(self):
        self.fs.create_file(
            '/fake/path/main.f90',
            contents='''\
program my_program
    implicit none
    use iso_fortran_env, only: real64, int32
    use my_module, only: test1
    use iso_fortran_env
end program my_program
''')
        result = extract_file_data([Path('/fake/path/main.f90')])
        self.assertEqual(len(result), 1)
        file_data = result[0]
        self.assertEqual(file_data['file_name'], '/fake/path/main.f90')
        self.assertEqual(len(file_data['programs']), 1)
        program_details = file_data['programs']['my_program']
        self.assertEqual(len(program_details['uses']), 2)
        self.assertEqual(program_details['uses']['iso_fortran_env']['module_name'], 'iso_fortran_env')
        self.assertEqual(program_details['uses']['iso_fortran_env']['selections'], [])
        self.assertEqual(program_details['uses']['my_module']['module_name'], 'my_module')
        self.assertEqual(program_details['uses']['my_module']['selections'], ['test1'])


if __name__ == '__main__':
    unittest.main()