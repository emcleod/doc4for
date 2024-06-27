import unittest
from pathlib import Path
from pyfakefs.fake_filesystem_unittest import TestCase
from doc4for.f90.generate_module_tree import process_modules

class TestSubroutineAnnotations(TestCase):
    def setUp(self):
        self.setUpPyfakefs()
    def test_subroutine_in_annotation_name_match(self):
        self.fs.create_file(
            '/fake/path/subroutine_in_annotation.f90',
            contents='''\
    module subroutine_in_annotation_module
    contains
    !!*
    ! A subroutine with @in annotation
    ! @in x: real
    ! @in y: real
    !*!
    subroutine test_subroutine_in_annotation(x, y)
        real, intent(in) :: x, y
        print *, x + y
    end subroutine test_subroutine_in_annotation
    end module subroutine_in_annotation_module
                            ''',
        )
        result = process_modules([Path('/fake/path/subroutine_in_annotation.f90')])

        module = result[0]
        subroutine = module['subroutines']['test_subroutine_in_annotation']
        inputs = subroutine['details']['in']
        self.assertEqual(inputs['x'], {'type': 'real', 'description': '', 'dimension': ''})
        self.assertEqual(inputs['y'], {'type': 'real', 'description': '', 'dimension': ''})

    def test_subroutine_in_annotation_name_mismatch(self):
        self.fs.create_file(
            '/fake/path/subroutine_in_annotation.f90',
            contents='''\
    module subroutine_in_annotation_module
    contains
    !!*
    ! A subroutine with @in annotation
    ! @in z: real
    !*!
    subroutine test_subroutine_in_annotation(x, y)
        real, intent(in) :: x, y
        print *, x + y
    end subroutine test_subroutine_in_annotation
    end module subroutine_in_annotation_module
                            ''',
        )
        result = process_modules([Path('/fake/path/subroutine_in_annotation.f90')])

        module = result[0]
        subroutine = module['subroutines']['test_subroutine_in_annotation']
        inputs = subroutine['details']['in']
        self.assertEqual(inputs['x'], {'type': 'real', 'description': '', 'dimension': ''})
        self.assertEqual(inputs['y'], {'type': 'real', 'description': '', 'dimension': ''})
        self.assertNotIn('z', inputs)

    def test_subroutine_out_annotation_name_match(self):
        self.fs.create_file(
            '/fake/path/subroutine_out_annotation.f90',
            contents='''\
    module subroutine_out_annotation_module
    contains
    !!*
    ! A subroutine with @out annotation
    ! @out y: real
    !*!
    subroutine test_subroutine_out_annotation(x, y)
        real, intent(in) :: x
        real, intent(out) :: y
        y = x + 1.0
    end subroutine test_subroutine_out_annotation
    end module subroutine_out_annotation_module
                            ''',
        )
        result = process_modules([Path('/fake/path/subroutine_out_annotation.f90')])

        module = result[0]
        subroutine = module['subroutines']['test_subroutine_out_annotation']
        inputs = subroutine['details']['in']
        self.assertEqual(inputs['x'], {'type': 'real', 'description': '', 'dimension': ''})
        outputs = subroutine['details']['out']
        self.assertEqual(outputs['y'], {'type': 'real', 'description': '', 'dimension': ''})

    def test_subroutine_out_annotation_name_mismatch(self):
        self.fs.create_file(
            '/fake/path/subroutine_out_annotation.f90',
            contents='''\
    module subroutine_out_annotation_module
    contains
    !!*
    ! A subroutine with @out annotation
    ! @out z: real
    !*!
    subroutine test_subroutine_out_annotation(x, y)
        real, intent(in) :: x
        real, intent(out) :: y
        y = x + 1.0
    end subroutine test_subroutine_out_annotation
    end module subroutine_out_annotation_module
                            ''',
        )
        result = process_modules([Path('/fake/path/subroutine_out_annotation.f90')])

        module = result[0]
        subroutine = module['subroutines']['test_subroutine_out_annotation']
        inputs = subroutine['details']['in']
        self.assertEqual(inputs['x'], {'type': 'real', 'description': '', 'dimension': ''})
        outputs = subroutine['details']['out']
        self.assertEqual(outputs['y'], {'type': 'real', 'description': '', 'dimension': ''})
        self.assertNotIn('z', outputs)

    def test_subroutine_inout_annotation_name_match(self):
        self.fs.create_file(
            '/fake/path/subroutine_inout_annotation.f90',
            contents='''\
    module subroutine_inout_annotation_module
    contains
    !!*
    ! A subroutine with @inout annotation
    ! @inout x: real
    !*!
    subroutine test_subroutine_inout_annotation(x)
        real, intent(inout) :: x
        x = x + 1.0
    end subroutine test_subroutine_inout_annotation
    end module subroutine_inout_annotation_module
                            ''',
        )
        result = process_modules([Path('/fake/path/subroutine_inout_annotation.f90')])

        module = result[0]
        subroutine = module['subroutines']['test_subroutine_inout_annotation']
        inputs = subroutine['details']['in']
        outputs = subroutine['details']['out']
        self.assertEqual(inputs['x'], {'type': 'real', 'description': '', 'dimension': ''})
        self.assertEqual(outputs['x'], {'type': 'real', 'description': '', 'dimension': ''})

    def test_subroutine_inout_annotation_name_mismatch(self):
        self.fs.create_file(
            '/fake/path/subroutine_inout_annotation.f90',
            contents='''\
    module subroutine_inout_annotation_module
    contains
    !!*
    ! A subroutine with @inout annotation
    ! @inout z: real
    !*!
    subroutine test_subroutine_inout_annotation(x)
        real, intent(inout) :: x
        x = x + 1.0
    end subroutine test_subroutine_inout_annotation
    end module subroutine_inout_annotation_module
                            ''',
        )
        result = process_modules([Path('/fake/path/subroutine_inout_annotation.f90')])

        module = result[0]
        subroutine = module['subroutines']['test_subroutine_inout_annotation']
        inputs = subroutine['details']['in']
        outputs = subroutine['details']['out']
        self.assertEqual(inputs['x'], {'type': 'real', 'description': '', 'dimension': ''})
        self.assertEqual(outputs['x'], {'type': 'real', 'description': '', 'dimension': ''})
        self.assertNotIn('z', inputs)
        self.assertNotIn('z', outputs)

    def test_subroutine_in_annotation_name_match_with_description(self):
        self.fs.create_file(
            '/fake/path/subroutine_in_annotation.f90',
            contents='''\
    module subroutine_in_annotation_module
    contains
    !!*
    ! A subroutine with @in annotation
    ! @in x: real The first input
    ! @in y: real The second input
    !*!
    subroutine test_subroutine_in_annotation(x, y)
        real, intent(in) :: x, y
        print *, x + y
    end subroutine test_subroutine_in_annotation
    end module subroutine_in_annotation_module
                            ''',
        )
        result = process_modules([Path('/fake/path/subroutine_in_annotation.f90')])

        module = result[0]
        subroutine = module['subroutines']['test_subroutine_in_annotation']
        inputs = subroutine['details']['in']
        self.assertEqual(inputs['x'], {'type': 'real', 'description': 'The first input', 'dimension': ''})
        self.assertEqual(inputs['y'], {'type': 'real', 'description': 'The second input', 'dimension': ''})

    def test_subroutine_out_annotation_name_match_with_description(self):
        self.fs.create_file(
            '/fake/path/subroutine_out_annotation.f90',
            contents='''\
    module subroutine_out_annotation_module
    contains
    !!*
    ! A subroutine with @out annotation
    ! @in x: real The input
    ! @out y: real The output
    !*!
    subroutine test_subroutine_out_annotation(x, y)
        real, intent(in) :: x
        real, intent(out) :: y
        y = x + 1.0
    end subroutine test_subroutine_out_annotation
    end module subroutine_out_annotation_module
                            ''',
        )
        result = process_modules([Path('/fake/path/subroutine_out_annotation.f90')])

        module = result[0]
        subroutine = module['subroutines']['test_subroutine_out_annotation']
        inputs = subroutine['details']['in']
        self.assertEqual(inputs['x'], {'type': 'real', 'description': 'The input', 'dimension': ''})
        outputs = subroutine['details']['out']
        self.assertEqual(outputs['y'], {'type': 'real', 'description': 'The output', 'dimension': ''})

    def test_subroutine_inout_annotation_name_match_with_description(self):
        self.fs.create_file(
            '/fake/path/subroutine_inout_annotation.f90',
            contents='''\
    module subroutine_inout_annotation_module
    contains
    !!*
    ! A subroutine with @inout annotation
    ! @inout x: real The variable to be updated
    !*!
    subroutine test_subroutine_inout_annotation(x)
        real, intent(inout) :: x
        x = x + 1.0
    end subroutine test_subroutine_inout_annotation
    end module subroutine_inout_annotation_module
                            ''',
        )
        result = process_modules([Path('/fake/path/subroutine_inout_annotation.f90')])

        module = result[0]
        subroutine = module['subroutines']['test_subroutine_inout_annotation']
        inputs = subroutine['details']['in']
        outputs = subroutine['details']['out']
        self.assertEqual(inputs['x'], {'type': 'real', 'description': 'The variable to be updated', 'dimension': ''})
        self.assertEqual(outputs['x'], {'type': 'real', 'description': 'The variable to be updated', 'dimension': ''})

if __name__ == '__main__':
    unittest.main()
