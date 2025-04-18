import unittest
from pathlib import Path
from pyfakefs.fake_filesystem_unittest import TestCase
from doc4for.f90.generate_module_tree import extract_module_data

class TestFunctionArguments(TestCase):
    maxDiff=None

    def setUp(self):
        self.setUpPyfakefs()

    def test_function_with_no_args(self):
        self.fs.create_file(
            '/fake/path/scalar_args.f90',
            contents='''\
    module no_args_functions
    contains
    !!*
    ! A function with no arguments
    !*!
    function add_numbers() 
        real :: x
        real :: y
        real :: add_numbers
        x = 2
        y = 3
        add_numbers = x + y
    end function add_numbers
    end module no_args_functions
                            ''',
        )
        result = extract_module_data([Path('/fake/path/scalar_args.f90')])
        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(module['module_name'], 'no_args_functions')
        self.assertEqual(len(module['functions']), 1)
        self.assertIn('add_numbers', module['functions'])
        function = module['functions']['add_numbers']
        inputs = function['in']
        outputs = function['out']
        results = function['return']
        self.assertEqual(len(inputs), 0)
        self.assertEqual(len(outputs), 0)
        self.assertEqual(len(results), 1)
        self.assertEqual(results['add_numbers'], {'type': 'real', 'description': '', 'dimension': ''})
        self.assertEqual(function['arguments'], [])

    def test_function_with_scalar_args_1(self):
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
        result = extract_module_data([Path('/fake/path/scalar_args.f90')])

        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(module['module_name'], 'scalar_args_functions')
        self.assertEqual(len(module['functions']), 1)
        self.assertIn('add_numbers', module['functions'])
        function = module['functions']['add_numbers']
        inputs = function['in']
        outputs = function['out']
        results = function['return']
        self.assertEqual(len(inputs), 2)
        self.assertEqual(inputs['x'], {'type': 'real', 'description': '', 'dimension': ''})
        self.assertEqual(inputs['y'], {'type': 'real', 'description': '', 'dimension': ''})
        self.assertEqual(len(outputs), 0)
        self.assertEqual(len(results), 1)
        self.assertEqual(results['res'], {'type': 'real', 'description': '', 'dimension': ''})

    def test_function_with_scalar_args_2(self):
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
        result = extract_module_data([Path('/fake/path/scalar_args.f90')])

        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(module['module_name'], 'scalar_args_functions')
        self.assertEqual(len(module['functions']), 1)
        self.assertIn('add_numbers', module['functions'])
        function = module['functions']['add_numbers']
        inputs = function['in']
        outputs = function['out']
        results = function['return']
        self.assertEqual(len(inputs), 2)
        self.assertEqual(inputs['x'], {'type': 'real', 'description': '', 'dimension': ''})
        self.assertEqual(inputs['y'], {'type': 'real', 'description': '', 'dimension': ''})
        self.assertEqual(len(outputs), 0)
        self.assertEqual(len(results), 1)
        self.assertEqual(results['res'], {'type': 'real', 'description': '', 'dimension': ''})

    def test_function_with_scalar_args_3(self):
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
        result = extract_module_data([Path('/fake/path/scalar_args.f90')])

        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(module['module_name'], 'scalar_args_functions')
        self.assertEqual(len(module['functions']), 1)
        self.assertIn('add_numbers', module['functions'])
        function = module['functions']['add_numbers']
        inputs = function['in']
        outputs = function['out']
        results = function['return']
        self.assertEqual(len(inputs), 2)
        self.assertEqual(inputs['x'], {'type': 'real', 'description': '', 'dimension': ''})
        self.assertEqual(inputs['y'], {'type': 'real', 'description': '', 'dimension': ''})
        self.assertEqual(len(outputs), 0)
        self.assertEqual(len(results), 1)
        self.assertEqual(results['add_numbers'], {'type': 'real', 'description': '', 'dimension': ''})

    def test_function_with_scalar_args_4(self):
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
        result = extract_module_data([Path('/fake/path/scalar_args.f90')])

        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(module['module_name'], 'scalar_args_functions')
        self.assertEqual(len(module['functions']), 1)
        self.assertIn('add_numbers', module['functions'])
        function = module['functions']['add_numbers']
        inputs = function['in']
        outputs = function['out']
        results = function['return']
        self.assertEqual(len(inputs), 2)
        self.assertEqual(inputs['x'], {'type': 'real', 'description': '', 'dimension': ''})
        self.assertEqual(inputs['y'], {'type': 'real', 'description': '', 'dimension': ''})
        self.assertEqual(len(outputs), 0)
        self.assertEqual(len(results), 1)
        self.assertEqual(results['add_numbers'], {'type': 'Unknown', 'description': '', 'dimension': ''})

    def test_function_with_array_args(self):
        self.fs.create_file(
            '/fake/path/array_args.f90',
            contents='''\
    module array_args_functions
    contains
    !!*
    ! A function with array arguments
    !*!
    function sum_array(arr) result(res)
        real, intent(in) :: arr(:)
        real :: res
        res = sum(arr)
    end function sum_array
    end module array_args_functions
                            ''',
        )
        result = extract_module_data([Path('/fake/path/array_args.f90')])

        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(module['module_name'], 'array_args_functions')
        self.assertEqual(len(module['functions']), 1)
        self.assertIn('sum_array', module['functions'])
        function = module['functions']['sum_array']
        inputs = function['in']
        outputs = function['out']
        results = function['return']
        self.assertEqual(len(inputs), 1)
        self.assertEqual(inputs['arr'], {
            'type': 'real', 
            'description': '', 
            'dimension': ': (allocatable)'})
        self.assertEqual(len(outputs), 0)
        self.assertEqual(len(results), 1)
        self.assertEqual(results['res'], {
            'type': 'real', 
            'description': '', 
            'dimension': ''})

    def test_function_with_mixed_args(self):
        self.fs.create_file(
            '/fake/path/mixed_args.f90',
            contents='''\
    module mixed_args_functions
    contains
    !!*
    ! A function with mixed scalar and array arguments
    !*!
    function multiply_scalar_array(scalar, arr) result(res)
        real, intent(in) :: scalar
        real, intent(in) :: arr(:)
        real, allocatable :: res(:)
        res = scalar * arr
    end function multiply_scalar_array
    end module mixed_args_functions
                            ''',
        )
        result = extract_module_data([Path('/fake/path/mixed_args.f90')])

        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(module['module_name'], 'mixed_args_functions')
        self.assertEqual(len(module['functions']), 1)
        self.assertIn('multiply_scalar_array', module['functions'])
        function = module['functions']['multiply_scalar_array']
        inputs = function['in']
        outputs = function['out']
        results = function['return']
        self.assertEqual(len(inputs), 2)
        self.assertEqual(inputs['scalar'], {'type': 'real', 'description': '', 'dimension': ''})
        self.assertEqual(inputs['arr'], {
            'type': 'real', 
            'description': '', 
            'dimension': ': (allocatable)'})
        self.assertEqual(len(outputs), 0)
        self.assertEqual(len(results), 1)
        self.assertEqual(results['res'], {
            'type': 'real',
            'description': '',
            'dimension': ': (allocatable)'})

if __name__ == '__main__':
    unittest.main()
