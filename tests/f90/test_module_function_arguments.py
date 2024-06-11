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

    def test_find_function_with_scalar_args_1(self):
        self.fs.create_file(
            '/fake/path/scalar_args.f90',
            contents='''\
    module scalar_args_functions
    contains
    !!*
    ! A function with scalar arguments
    !*!
    function add_numbers(x, y) result(res)
        real, intent(in) :: x, y
        real :: res
        res = x + y
    end function add_numbers
    end module scalar_args_functions
                            ''',
        )
        result = process_modules(['/fake/path/scalar_args.f90'])

        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(module['module_name'], 'scalar_args_functions')
        self.assertEqual(len(module['functions']), 1)
        self.assertIn('add_numbers', module['functions'])
        function = module['functions']['add_numbers']
        self.assertIn('details', function)
        self.assertIn('arguments', function['details'])
        self.assertEqual(len(function['details']['arguments']), 0)
        inputs = function['details']['inputs']
        outputs = function['details']['outputs']
        results = function['details']['return']
        self.assertEqual(len(inputs), 2)
        self.assertEqual(inputs['x'], {'type': 'real', 'name': 'x'})
        self.assertEqual(inputs['y'], {'type': 'real', 'name': 'y'})
        self.assertEqual(len(outputs), 0)
        self.assertEqual(len(results), 1)
        self.assertEqual(results['res'], {'type': 'real', 'name': 'res'})

    def test_find_function_with_scalar_args_2(self):
        self.fs.create_file(
            '/fake/path/scalar_args.f90',
            contents='''\
    module scalar_args_functions
    contains
    !!*
    ! A function with scalar arguments
    !*!
    function add_numbers(x, y) result(res)
        real, intent(in) :: x
        real, intent(in) :: y
        real :: res
        res = x + y
    end function add_numbers
    end module scalar_args_functions
                            ''',
        )
        result = process_modules(['/fake/path/scalar_args.f90'])

        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(module['module_name'], 'scalar_args_functions')
        self.assertEqual(len(module['functions']), 1)
        self.assertIn('add_numbers', module['functions'])
        function = module['functions']['add_numbers']
        self.assertIn('details', function)
        self.assertIn('arguments', function['details'])
        self.assertEqual(len(function['details']['arguments']), 0)
        inputs = function['details']['inputs']
        outputs = function['details']['outputs']
        results = function['details']['return']
        self.assertEqual(len(inputs), 2)
        self.assertEqual(inputs['x'], {'type': 'real', 'name': 'x'})
        self.assertEqual(inputs['y'], {'type': 'real', 'name': 'y'})
        self.assertEqual(len(outputs), 0)
        self.assertEqual(len(results), 1)
        self.assertEqual(results['res'], {'type': 'real', 'name': 'res'})

    def test_find_function_with_scalar_args_3(self):
        self.fs.create_file(
            '/fake/path/scalar_args.f90',
            contents='''\
    module scalar_args_functions
    contains
    !!*
    ! A function with scalar arguments
    !*!
    function add_numbers(x, y) 
        real, intent(in) :: x
        real, intent(in) :: y
        real :: add_numbers
        add_numbers = x + y
    end function add_numbers
    end module scalar_args_functions
                            ''',
        )
        result = process_modules(['/fake/path/scalar_args.f90'])

        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(module['module_name'], 'scalar_args_functions')
        self.assertEqual(len(module['functions']), 1)
        self.assertIn('add_numbers', module['functions'])
        function = module['functions']['add_numbers']
        self.assertIn('details', function)
        self.assertIn('arguments', function['details'])
        self.assertEqual(len(function['details']['arguments']), 0)
        inputs = function['details']['inputs']
        outputs = function['details']['outputs']
        results = function['details']['return']
        self.assertEqual(len(inputs), 2)
        self.assertEqual(inputs['x'], {'type': 'real', 'name': 'x'})
        self.assertEqual(inputs['y'], {'type': 'real', 'name': 'y'})
        self.assertEqual(len(outputs), 0)
        self.assertEqual(len(results), 1)
        self.assertEqual(results['add_numbers'], {'type': 'real', 'name': 'add_numbers'})

    def test_find_function_with_scalar_args_4(self):
        self.fs.create_file(
            '/fake/path/scalar_args.f90',
            contents='''\
    module scalar_args_functions
    contains
    !!*
    ! A function with scalar arguments
    !*!
    function add_numbers(x, y) 
        real, intent(in) :: x
        real, intent(in) :: y
        add_numbers = x + y
    end function add_numbers
    end module scalar_args_functions
                            ''',
        )
        result = process_modules(['/fake/path/scalar_args.f90'])

        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(module['module_name'], 'scalar_args_functions')
        self.assertEqual(len(module['functions']), 1)
        self.assertIn('add_numbers', module['functions'])
        function = module['functions']['add_numbers']
        self.assertIn('details', function)
        self.assertIn('arguments', function['details'])
        self.assertEqual(len(function['details']['arguments']), 0)
        inputs = function['details']['inputs']
        outputs = function['details']['outputs']
        results = function['details']['return']
        self.assertEqual(len(inputs), 2)
        self.assertEqual(inputs['x'], {'type': 'real', 'name': 'x'})
        self.assertEqual(inputs['y'], {'type': 'real', 'name': 'y'})
        self.assertEqual(len(outputs), 0)
        self.assertEqual(len(results), 1)
        self.assertEqual(results['add_numbers'], {'type': None, 'name': 'add_numbers'})

    # def test_find_function_with_array_args(self):
    #     self.fs.create_file(
    #         '/fake/path/array_args.f90',
    #         contents='''\
    # module array_args_functions
    # contains
    # !!*
    # ! A function with array arguments
    # !*!
    # function sum_array(arr) result(res)
    #     real, intent(in) :: arr(:)
    #     real :: res
    #     res = sum(arr)
    # end function sum_array
    # end module array_args_functions
    #                         ''',
    #     )
    #     result = process_modules(['/fake/path/array_args.f90'])

    #     self.assertEqual(len(result), 1)
    #     module = result[0]
    #     self.assertEqual(module['module_name'], 'array_args_functions')
    #     self.assertEqual(len(module['functions']), 1)
    #     self.assertIn('sum_array', module['functions'])
    #     function = module['functions']['sum_array']
    #     self.assertIn('details', function)
    #     self.assertIn('arguments', function['details'])
    #     arguments = function['details']['arguments']
    #     self.assertEqual(len(arguments), 1)
    #     self.assertIn('arr', arguments)
    #     self.assertEqual(arguments['arr'], {'type': 'real', 'intent': 'in', 'dimension': '(:)'})

    # def test_find_function_with_mixed_args(self):
    #     self.fs.create_file(
    #         '/fake/path/mixed_args.f90',
    #         contents='''\
    # module mixed_args_functions
    # contains
    # !!*
    # ! A function with mixed scalar and array arguments
    # !*!
    # function multiply_scalar_array(scalar, arr) result(res)
    #     real, intent(in) :: scalar
    #     real, intent(in) :: arr(:)
    #     real, allocatable :: res(:)
    #     res = scalar * arr
    # end function multiply_scalar_array
    # end module mixed_args_functions
    #                         ''',
    #     )
    #     result = process_modules(['/fake/path/mixed_args.f90'])

    #     self.assertEqual(len(result), 1)
    #     module = result[0]
    #     self.assertEqual(module['module_name'], 'mixed_args_functions')
    #     self.assertEqual(len(module['functions']), 1)
    #     self.assertIn('multiply_scalar_array', module['functions'])
    #     function = module['functions']['multiply_scalar_array']
    #     self.assertIn('details', function)
    #     self.assertIn('arguments', function['details'])
    #     arguments = function['details']['arguments']
    #     self.assertEqual(len(arguments), 2)
    #     self.assertIn('scalar', arguments)
    #     self.assertIn('arr', arguments)
    #     self.assertEqual(arguments['scalar'], {'type': 'real', 'intent': 'in'})
    #     self.assertEqual(arguments['arr'], {'type': 'real', 'intent': 'in', 'dimension': '(:)'})

if __name__ == '__main__':
    unittest.main()
