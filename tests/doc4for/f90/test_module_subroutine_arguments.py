import unittest
from pathlib import Path
from pyfakefs.fake_filesystem_unittest import TestCase
from doc4for.f90.generate_module_tree import process_modules

class TestSubroutineArguments(TestCase):
    def setUp(self):
        self.setUpPyfakefs()

    def test_find_subroutine_with_scalar_args_1(self):
        self.fs.create_file(
            '/fake/path/scalar_args.f90',
            contents='''\
    module scalar_args_subroutines
    contains
    !!*
    ! A subroutine with scalar arguments
    !*!
    subroutine add_numbers(x, y, res)
        real, intent(in) :: x, y
        real, intent(out) :: res
        res = x + y
    end subroutine add_numbers
    end module scalar_args_subroutines
                            ''',
        )
        result = process_modules([Path('/fake/path/scalar_args.f90')])

        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(module['module_name'], 'scalar_args_subroutines')
        self.assertEqual(len(module['subroutines']), 1)
        self.assertIn('add_numbers', module['subroutines'])
        subroutine = module['subroutines']['add_numbers']
        self.assertIn('details', subroutine)
        inputs = subroutine['details']['in']
        outputs = subroutine['details']['out']
        self.assertEqual(len(inputs), 2)
        self.assertEqual(inputs['x'], {'type': 'real', 'description': '', 'dimension': ''})
        self.assertEqual(inputs['y'], {'type': 'real', 'description': '', 'dimension': ''})
        self.assertEqual(len(outputs), 1)
        self.assertEqual(outputs['res'], {'type': 'real', 'description': '', 'dimension': ''})

    def test_find_subroutine_with_scalar_args_2(self):
        self.fs.create_file(
            '/fake/path/scalar_args.f90',
            contents='''\
    module scalar_args_subroutines
    contains
    !!*
    ! A subroutine with scalar arguments
    !*!
    subroutine add_numbers(x, y, res)
        real, intent(in) :: x
        real, intent(in) :: y
        real, intent(out) :: res
        res = x + y
    end subroutine add_numbers
    end module scalar_args_subroutines
                            ''',
        )
        result = process_modules([Path('/fake/path/scalar_args.f90')])

        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(module['module_name'], 'scalar_args_subroutines')
        self.assertEqual(len(module['subroutines']), 1)
        self.assertIn('add_numbers', module['subroutines'])
        subroutine = module['subroutines']['add_numbers']
        self.assertIn('details', subroutine)
        inputs = subroutine['details']['in']
        outputs = subroutine['details']['out']
        self.assertEqual(len(inputs), 2)
        self.assertEqual(inputs['x'], {'type': 'real', 'description': '', 'dimension': ''})
        self.assertEqual(inputs['y'], {'type': 'real', 'description': '', 'dimension': ''})
        self.assertEqual(len(outputs), 1)
        self.assertEqual(outputs['res'], {'type': 'real', 'description': '', 'dimension': ''})

    def test_find_subroutine_with_array_args(self):
        self.fs.create_file(
            '/fake/path/array_args.f90',
            contents='''\
    module array_args_subroutines
    contains
    !!*
    ! A subroutine with array arguments
    !*!
    subroutine sum_array(arr, res)
        real, intent(in) :: arr(:)
        real, intent(out) :: res
        res = sum(arr)
    end subroutine sum_array
    end module array_args_subroutines
                            ''',
        )
        result = process_modules([Path('/fake/path/array_args.f90')])

        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(module['module_name'], 'array_args_subroutines')
        self.assertEqual(len(module['subroutines']), 1)
        self.assertIn('sum_array', module['subroutines'])
        subroutine = module['subroutines']['sum_array']
        self.assertIn('details', subroutine)
        inputs = subroutine['details']['in']
        outputs = subroutine['details']['out']
        self.assertEqual(len(inputs), 1)
        self.assertEqual(inputs['arr'], {'type': 'real', 'description': '', 'dimension': 'allocatable &times; allocatable'})
        self.assertEqual(len(outputs), 1)
        self.assertEqual(outputs['res'], {'type': 'real', 'description': '', 'dimension': ''})

    def test_find_subroutine_with_mixed_args(self):
        self.fs.create_file(
            '/fake/path/mixed_args.f90',
            contents='''\
    module mixed_args_subroutines
    contains
    !!*
    ! A subroutine with mixed scalar and array arguments
    !*!
    subroutine multiply_scalar_array(scalar, arr, res)
        real, intent(in) :: scalar
        real, intent(in) :: arr(:)
        real, intent(out), allocatable :: res(:)
        res = scalar * arr
    end subroutine multiply_scalar_array
    end module mixed_args_subroutines
                            ''',
        )
        result = process_modules([Path('/fake/path/mixed_args.f90')])

        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(module['module_name'], 'mixed_args_subroutines')
        self.assertEqual(len(module['subroutines']), 1)
        self.assertIn('multiply_scalar_array', module['subroutines'])
        subroutine = module['subroutines']['multiply_scalar_array']
        self.assertIn('details', subroutine)
        inputs = subroutine['details']['in']
        outputs = subroutine['details']['out']
        self.assertEqual(len(inputs), 2)
        self.assertEqual(inputs['scalar'], {'type': 'real', 'description': '', 'dimension': ''})
        self.assertEqual(inputs['arr'], {'type': 'real', 'description': '', 'dimension': 'allocatable &times; allocatable'})
        self.assertEqual(len(outputs), 1)
        self.assertEqual(outputs['res'], {'type': 'real', 'description': '', 'dimension': 'allocatable &times; allocatable'})

    def test_find_subroutine_with_inout_args(self):
        self.fs.create_file(
            '/fake/path/inout_args.f90',
            contents='''\
    module inout_args_subroutines
    contains
    !!*
    ! A subroutine with inout arguments
    !*!
    subroutine increment(x)
        real, intent(inout) :: x
        x = x + 1.0
    end subroutine increment
    end module inout_args_subroutines
                            ''',
        )
        result = process_modules([Path('/fake/path/inout_args.f90')])

        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(module['module_name'], 'inout_args_subroutines')
        self.assertEqual(len(module['subroutines']), 1)
        self.assertIn('increment', module['subroutines'])
        subroutine = module['subroutines']['increment']
        self.assertIn('details', subroutine)
        inputs = subroutine['details']['in']
        outputs = subroutine['details']['out']
        self.assertEqual(len(inputs), 1)
        self.assertEqual(inputs['x'], {'type': 'real', 'description': '', 'dimension': ''})
        self.assertEqual(len(outputs), 1)
        self.assertEqual(outputs['x'], {'type': 'real', 'description': '', 'dimension': ''})

if __name__ == '__main__':
    unittest.main()