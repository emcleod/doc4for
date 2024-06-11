import unittest
from pyfakefs.fake_filesystem_unittest import TestCase
import os
import sys

# Add the project root to the Python path
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), '..', '..')))
from generate_module_tree import process_modules

class TestFunctions(TestCase):
    def setUp(self):
        self.setUpPyfakefs()

    def test_find_pure_functions(self):
        self.fs.create_file(
            '/fake/path/pure.f90',
            contents='''\
module pure_functions
contains
!!*
! A test pure function
!*!
pure function test()
end function test
end module pure_functions
                            ''',
        )
        result = process_modules(['/fake/path/pure.f90'])

        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(module['module_name'], 'pure_functions')
        self.assertEqual(module['file_name'], '/fake/path/pure.f90')
        self.assertNotIn('module_description', module)
        self.assertEqual(len(module['functions']), 1)
        self.assertIn('test', module['functions'])
        function = module['functions']['test']
        self.assertEqual(function['description'], 'A test pure function')
        self.assertIn('details', function)
        self.assertIn('attributes', function['details'])
        self.assertEqual(function['details']['arguments'], {})
        attributes = function['details']['attributes']
        self.assertEqual(attributes, ['pure'])

    def test_find_elemental_functions(self):
        self.fs.create_file(
            '/fake/path/elemental.f90',
            contents='''\
    module elemental_functions
    contains
    !!*
    ! An elemental function
    !*!
    elemental function elem_func()
        elem_func = 42
    end function elem_func
    end module elemental_functions
                            ''',
        )
        result = process_modules(['/fake/path/elemental.f90'])

        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(module['module_name'], 'elemental_functions')
        self.assertEqual(len(module['functions']), 1)
        self.assertIn('elem_func', module['functions'])
        function = module['functions']['elem_func']
        self.assertIn('details', function)
        self.assertIn('attributes', function['details'])
        attributes = function['details']['attributes']
        self.assertEqual(attributes, ['elemental'])


    def test_find_recursive_functions(self):
        self.fs.create_file(
            '/fake/path/recursive.f90',
            contents='''\
    module recursive_functions
    contains
    !!*
    ! A recursive function
    !*!
    recursive function fact() result(res)
        integer :: res
        res = 1
    end function fact
    end module recursive_functions
                            ''',
        )
        result = process_modules(['/fake/path/recursive.f90'])

        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(module['module_name'], 'recursive_functions')
        self.assertEqual(len(module['functions']), 1)
        self.assertIn('fact', module['functions'])
        function = module['functions']['fact']
        self.assertIn('details', function)
        self.assertIn('attributes', function['details'])
        attributes = function['details']['attributes']
        self.assertEqual(attributes, ['recursive'])

    def test_find_combined_functions(self):
        self.fs.create_file(
            '/fake/path/combined.f90',
            contents='''\
    module combined_functions
    contains
    !!*
    ! A pure elemental function
    !*!
    pure elemental function square()
        square = 4
    end function square

    !!*
    ! A recursive function
    !*!
    recursive function fact() result(res)
        integer :: res
        res = 1
    end function fact
    end module combined_functions
                            ''',
        )
        result = process_modules(['/fake/path/combined.f90'])

        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(module['module_name'], 'combined_functions')
        self.assertEqual(len(module['functions']), 2)
        self.assertIn('square', module['functions'])
        square_function = module['functions']['square']
        self.assertIn('details', square_function)
        self.assertIn('attributes', square_function['details'])
        square_attributes = square_function['details']['attributes']
        self.assertEqual(square_attributes, ['pure', 'elemental'])

        self.assertIn('fact', module['functions'])
        fact_function = module['functions']['fact']
        self.assertIn('details', fact_function)
        self.assertIn('attributes', fact_function['details'])
        fact_attributes = fact_function['details']['attributes']
        self.assertEqual(fact_attributes, ['recursive'])

if __name__ == '__main__':
    unittest.main()