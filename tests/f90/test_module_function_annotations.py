import unittest
from pyfakefs.fake_filesystem_unittest import TestCase
import os
import sys

# Add the project root to the Python path
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), '..', '..')))
from generate_module_tree import process_modules

class TestFunctionAnnotations(TestCase):
    def setUp(self):
        self.setUpPyfakefs()

    def test_in_annotation_name_match(self):
        self.fs.create_file(
            '/fake/path/in_annotation.f90',
            contents='''\
    module in_annotation_module
    contains
    !!*
    ! A function with @in annotation
    ! @in x: real
    ! @in y: real
    !*!
    function test_in_annotation(x, y) result(res)
        real, intent(in) :: x, y
        real :: res
        res = x + y
    end function test_in_annotation
    end module in_annotation_module
                            ''',
        )
        result = process_modules(['/fake/path/in_annotation.f90'])

        module = result[0]
        function = module['functions']['test_in_annotation']
        inputs = function['details']['in']
        self.assertEqual(inputs['x'], {'type': 'real', 'description': '', 'dimension': ''})
        self.assertEqual(inputs['y'], {'type': 'real', 'description': '', 'dimension': ''})

    def test_in_annotation_name_mismatch(self):
        self.fs.create_file(
            '/fake/path/in_annotation.f90',
            contents='''\
    module in_annotation_module
    contains
    !!*
    ! A function with @in annotation
    ! @in z: real
    !*!
    function test_in_annotation(x, y) result(res)
        real, intent(in) :: x, y
        real :: res
        res = x + y
    end function test_in_annotation
    end module in_annotation_module
                            ''',
        )
        result = process_modules(['/fake/path/in_annotation.f90'])

        module = result[0]
        function = module['functions']['test_in_annotation']
        inputs = function['details']['in']
        self.assertEqual(inputs['x'], {'type': 'real', 'description': '', 'dimension': ''})
        self.assertNotIn('z', inputs)
        #TODO
        # with self.assertLogs('root', level='WARNING') as cm:
        #     process_modules(['/fake/path/in_annotation.f90'])
        # self.assertIn('WARNING:root:Warning: in annotation z found that is not present in arguments [dict_keys([\'x\', \'y\'])]', cm.output)

    def test_in_annotation_type_match(self):
        self.fs.create_file(
            '/fake/path/in_annotation.f90',
            contents='''\
    module in_annotation_module
    contains
    !!*
    ! A function with @in annotation
    ! @in x: real
    !*!
    function test_in_annotation(x, y) result(res)
        real, intent(in) :: x, y
        real :: res
        res = x + y
    end function test_in_annotation
    end module in_annotation_module
                            ''',
        )
        result = process_modules(['/fake/path/in_annotation.f90'])

        module = result[0]
        function = module['functions']['test_in_annotation']
        inputs = function['details']['in']
        self.assertEqual(inputs['x'], {'type': 'real', 'description': '', 'dimension': ''})

    def test_in_annotation_type_mismatch(self):
        self.fs.create_file(
            '/fake/path/in_annotation.f90',
            contents='''\
    module in_annotation_module
    contains
    !!*
    ! A function with @in annotation
    ! @in x: integer
    !*!
    function test_in_annotation(x, y) result(res)
        real, intent(in) :: x, y
        real :: res
        res = x + y
    end function test_in_annotation
    end module in_annotation_module
                            ''',
        )
        result = process_modules(['/fake/path/in_annotation.f90'])

        module = result[0]
        function = module['functions']['test_in_annotation']
        inputs = function['details']['in']
        self.assertEqual(inputs['x'], {'type': 'real', 'description': '', 'dimension': ''})
        #TODO
        # with self.assertLogs('root', level='WARNING') as cm:
        #     process_modules(['/fake/path/in_annotation.f90'])
        # self.assertIn('WARNING:root:Warning: in annotation x type integer does not match value in arguments real', cm.output)

    def test_out_annotation_name_match(self):
        self.fs.create_file(
            '/fake/path/out_annotation.f90',
            contents='''\
    module out_annotation_module
    contains
    !!*
    ! A function with @out annotation
    ! @out res: real
    !*!
    function test_out_annotation(x, y) result(res)
        real, intent(in) :: x
        real, intent(out) :: y
        real :: res
        res = x + y
    end function test_out_annotation
    end module out_annotation_module
                            ''',
        )
        result = process_modules(['/fake/path/out_annotation.f90'])

        module = result[0]
        function = module['functions']['test_out_annotation']
        inputs = function['details']['in']
        self.assertEqual(inputs['x'], {'type': 'real', 'description': '', 'dimension': ''})
        outputs = function['details']['out']
        self.assertEqual(outputs['y'], {'type': 'real', 'description': '', 'dimension': ''})

    def test_out_annotation_name_mismatch(self):
        self.fs.create_file(
            '/fake/path/out_annotation.f90',
            contents='''\
    module out_annotation_module
    contains
    !!*
    ! A function with @out annotation
    ! @out z: real
    !*!
    function test_out_annotation(x, y) result(res)
        real, intent(in) :: x, y
        real :: res
        res = x + y
    end function test_out_annotation
    end module out_annotation_module
                            ''',
        )
        result = process_modules(['/fake/path/out_annotation.f90'])

        module = result[0]
        function = module['functions']['test_out_annotation']
        inputs = function['details']['in']
        self.assertEqual(inputs['x'], {'type': 'real', 'description': '', 'dimension': ''})
        self.assertNotIn('z', inputs)
        #TODO
        # with self.assertLogs('root', level='WARNING') as cm:
        #     process_modules(['/fake/path/out_annotation.f90'])
        # self.assertIn('WARNING:root:Warning: out annotation x type integer does not match value in arguments real', cm.output)

    def test_out_annotation_type_match(self):
        self.fs.create_file(
            '/fake/path/out_annotation.f90',
            contents='''\
    module out_annotation_module
    contains
    !!*
    ! A function with @out annotation
    ! @out y: real
    !*!
    function test_out_annotation(x, y) result(res)
        real, intent(in) :: x
        real, intent(out) :: y
        real :: res
        res = x + y
    end function test_out_annotation
    end module out_annotation_module
                            ''',
        )
        result = process_modules(['/fake/path/out_annotation.f90'])

        module = result[0]
        function = module['functions']['test_out_annotation']
        outputs = function['details']['out']
        self.assertEqual(outputs['y'], {'type': 'real', 'description': '', 'dimension': ''})

    def test_out_annotation_type_mismatch(self):
        self.fs.create_file(
            '/fake/path/out_annotation.f90',
            contents='''\
    module out_annotation_module
    contains
    !!*
    ! A function with @out annotation
    ! @out y: integer
    !*!
    function test_out_annotation(x, y) result(res)
        real, intent(in) :: x
        real, intent(out) :: y
        real :: res
        res = x + y
    end function test_out_annotation
    end module out_annotation_module
                            ''',
        )
        result = process_modules(['/fake/path/out_annotation.f90'])

        module = result[0]
        function = module['functions']['test_out_annotation']
        outputs = function['details']['out']
        self.assertEqual(outputs['y'], {'type': 'real', 'description': '', 'dimension': ''})
        #TODO
        # with self.assertLogs('root', level='WARNING') as cm:
        #     process_modules(['/fake/path/in_annotation.f90'])
        # self.assertIn('WARNING:root:Warning: in annotation x type integer does not match value in arguments real', cm.output)

    def test_inout_annotation_name_match(self):
        self.fs.create_file(
            '/fake/path/inout_annotation.f90',
            contents='''\
    module inout_annotation_module
    contains
    !!*
    ! A function with @inout annotation
    ! @inout x: real
    !*!
    function test_inout_annotation(x, y) result(res)
        real, intent(inout) :: x
        real, intent(in) :: y
        real :: res
        res = x + y
    end function test_inout_annotation
    end module inout_annotation_module
                            ''',
        )
        result = process_modules(['/fake/path/inout_annotation.f90'])

        module = result[0]
        function = module['functions']['test_inout_annotation']
        inputs = function['details']['in']
        outputs = function['details']['out']
        self.assertEqual(inputs['x'], {'type': 'real', 'description': '', 'dimension': ''})
        self.assertEqual(outputs['x'], {'type': 'real', 'description': '', 'dimension': ''})

    def test_inout_annotation_name_mismatch(self):
        self.fs.create_file(
            '/fake/path/inout_annotation.f90',
            contents='''\
    module inout_annotation_module
    contains
    !!*
    ! A function with @inout annotation
    ! @inout z: real
    !*!
    function test_inout_annotation(x, y) result(res)
        real, intent(inout) :: x
        real, intent(inout) :: y
        real :: res
        res = x + y
    end function test_inout_annotation
    end module inout_annotation_module
                            ''',
        )
        result = process_modules(['/fake/path/inout_annotation.f90'])

        module = result[0]
        function = module['functions']['test_inout_annotation']
        inputs = function['details']['in']
        outputs = function['details']['out']
        self.assertEqual(inputs['x'], {'type': 'real', 'description': '', 'dimension': ''})
        self.assertEqual(inputs['y'], {'type': 'real', 'description': '', 'dimension': ''})
        self.assertNotIn('z', inputs)
        self.assertEqual(outputs['x'], {'type': 'real', 'description': '', 'dimension': ''})
        self.assertEqual(outputs['y'], {'type': 'real', 'description': '', 'dimension': ''})
        self.assertNotIn('z', outputs)
    #TODO
    #     with self.assertLogs('root', level='WARNING') as cm:
    #         process_modules(['/fake/path/inout_annotation.f90'])
    #     self.assertIn('WARNING:root:Warning: inout annotation z found that is not present in arguments [dict_keys([\'x\'])]', cm.output)

    def test_inout_annotation_type_match(self):
        self.fs.create_file(
            '/fake/path/inout_annotation.f90',
            contents='''\
    module inout_annotation_module
    contains
    !!*
    ! A function with @inout annotation
    ! @inout x: real
    !*!
    function test_inout_annotation(x, y) result(res)
        real, intent(inout) :: x
        real, intent(in) :: y
        real :: res
        res = x + y
    end function test_inout_annotation
    end module inout_annotation_module
                            ''',
        )
        result = process_modules(['/fake/path/inout_annotation.f90'])

        module = result[0]
        function = module['functions']['test_inout_annotation']
        inputs = function['details']['in']
        outputs = function['details']['out']
        self.assertEqual(inputs['x'], {'type': 'real', 'description': '', 'dimension': ''})
        self.assertEqual(outputs['x'], {'type': 'real', 'description': '', 'dimension': ''})

    def test_inout_annotation_type_mismatch(self):
        self.fs.create_file(
            '/fake/path/inout_annotation.f90',
            contents='''\
    module inout_annotation_module
    contains
    !!*
    ! A function with @inout annotation
    ! @inout x: integer
    !*!
    function test_inout_annotation(x, y) result(res)
        real, intent(inout) :: x
        integer, intent(in) :: y
        real :: res
        res = x + y
    end function test_inout_annotation
    end module inout_annotation_module
                            ''',
        )
        result = process_modules(['/fake/path/inout_annotation.f90'])

        module = result[0]
        function = module['functions']['test_inout_annotation']
        inputs = function['details']['in']
        outputs = function['details']['out']
        self.assertEqual(inputs['x'], {'type': 'real', 'description': '', 'dimension': ''})
        self.assertEqual(inputs['y'], {'type': 'integer', 'description': '', 'dimension': ''})
        self.assertEqual(outputs['x'], {'type': 'real', 'description': '', 'dimension': ''})

        #TODO
    #     with self.assertLogs('root', level='WARNING') as cm:
    #         process_modules(['/fake/path/inout_annotation.f90'])
    #     self.assertIn('WARNING:root:Warning: inout annotation x type integer does not match value in arguments real', cm.output)

    def test_return_type(self):
        self.fs.create_file(
            '/fake/path/return_type.f90',
            contents='''\
    module return_type_module
    contains
    !!*
    ! A function with return type
    !*!
    function test_return_type(x, y) result(res)
        real, intent(in) :: x, y
        integer :: res
        res = int(x + y)
    end function test_return_type
    end module return_type_module
                            ''',
        )
        result = process_modules(['/fake/path/return_type.f90'])

        module = result[0]
        function = module['functions']['test_return_type']
        results = function['details']['return']
        self.assertEqual(results['res'], {'type': 'integer', 'description': '', 'dimension': ''})


    def test_in_annotation_name_mismatch_with_description(self):
        self.fs.create_file(
            '/fake/path/in_annotation.f90',
            contents='''\
    module in_annotation_module
    contains
    !!*
    ! A function with @in annotation
    ! @in z: real The first variable
    !*!
    function test_in_annotation(x, y) result(res)
        real, intent(in) :: x, y
        real :: res
        res = x + y
    end function test_in_annotation
    end module in_annotation_module
                            ''',
        )
        result = process_modules(['/fake/path/in_annotation.f90'])

        module = result[0]
        function = module['functions']['test_in_annotation']
        inputs = function['details']['in']
        self.assertEqual(inputs['x'], {'type': 'real', 'description': '', 'dimension': ''})
        self.assertNotIn('z', inputs)
        #TODO
        # with self.assertLogs('root', level='WARNING') as cm:
        #     process_modules(['/fake/path/in_annotation.f90'])
        # self.assertIn('WARNING:root:Warning: in annotation z found that is not present in arguments [dict_keys([\'x\', \'y\'])]', cm.output)

    def test_in_annotation_type_match_with_description(self):
        self.fs.create_file(
            '/fake/path/in_annotation.f90',
            contents='''\
    module in_annotation_module
    contains
    !!*
    ! A function with @in annotation
    ! @in x: real The first variable
    !*!
    function test_in_annotation(x, y) result(res)
        real, intent(in) :: x, y
        real :: res
        res = x + y
    end function test_in_annotation
    end module in_annotation_module
                            ''',
        )
        result = process_modules(['/fake/path/in_annotation.f90'])

        module = result[0]
        function = module['functions']['test_in_annotation']
        inputs = function['details']['in']
        self.assertEqual(inputs['x'], {'type': 'real', 'description': 'The first variable', 'dimension': ''})

    def test_in_annotation_type_mismatch_with_description(self):
        self.fs.create_file(
            '/fake/path/in_annotation.f90',
            contents='''\
    module in_annotation_module
    contains
    !!*
    ! A function with @in annotation
    ! @in x: integer The first variable
    !*!
    function test_in_annotation(x, y) result(res)
        real, intent(in) :: x, y
        real :: res
        res = x + y
    end function test_in_annotation
    end module in_annotation_module
                            ''',
        )
        result = process_modules(['/fake/path/in_annotation.f90'])

        module = result[0]
        function = module['functions']['test_in_annotation']
        inputs = function['details']['in']
        self.assertEqual(inputs['x'], {'type': 'real', 'description': 'The first variable', 'dimension': ''})
        #TODO
        # with self.assertLogs('root', level='WARNING') as cm:
        #     process_modules(['/fake/path/in_annotation.f90'])
        # self.assertIn('WARNING:root:Warning: in annotation x type integer does not match value in arguments real', cm.output)

    def test_out_annotation_name_match_with_description(self):
        self.fs.create_file(
            '/fake/path/out_annotation.f90',
            contents='''\
    module out_annotation_module
    contains
    !!*
    ! A function with @out annotation
    ! @in x: real The first variable
    ! @out y: real The second variable
    !*!
    function test_out_annotation(x, y) result(res)
        real, intent(in) :: x
        real, intent(out) :: y
        real :: res
        res = x + y
    end function test_out_annotation
    end module out_annotation_module
                            ''',
        )
        result = process_modules(['/fake/path/out_annotation.f90'])

        module = result[0]
        function = module['functions']['test_out_annotation']
        inputs = function['details']['in']
        self.assertEqual(inputs['x'], {'type': 'real', 'description': 'The first variable', 'dimension': ''})
        outputs = function['details']['out']
        self.assertEqual(outputs['y'], {'type': 'real', 'description': 'The second variable', 'dimension': ''})

    def test_out_annotation_name_mismatch_with_description(self):
        self.fs.create_file(
            '/fake/path/out_annotation.f90',
            contents='''\
    module out_annotation_module
    contains
    !!*
    ! A function with @out annotation
    ! @out z: real The first variable
    !*!
    function test_out_annotation(x, y) result(res)
        real, intent(in) :: x, y
        real :: res
        res = x + y
    end function test_out_annotation
    end module out_annotation_module
                            ''',
        )
        result = process_modules(['/fake/path/out_annotation.f90'])

        module = result[0]
        function = module['functions']['test_out_annotation']
        inputs = function['details']['in']
        self.assertEqual(inputs['x'], {'type': 'real', 'description': '', 'dimension': ''})
        self.assertEqual(inputs['y'], {'type': 'real', 'description': '', 'dimension': ''})
        self.assertNotIn('z', inputs)
        #TODO
        # with self.assertLogs('root', level='WARNING') as cm:
        #     process_modules(['/fake/path/out_annotation.f90'])
        # self.assertIn('WARNING:root:Warning: out annotation x type integer does not match value in arguments real', cm.output)

    def test_out_annotation_type_match_with_description(self):
        self.fs.create_file(
            '/fake/path/out_annotation.f90',
            contents='''\
    module out_annotation_module
    contains
    !!*
    ! A function with @out annotation
    ! @in x: real The first variable
    ! @out y: real The second variable
    !*!
    function test_out_annotation(x, y) result(res)
        real, intent(in) :: x
        real, intent(out) :: y
        real :: res
        res = x + y
    end function test_out_annotation
    end module out_annotation_module
                            ''',
        )
        result = process_modules(['/fake/path/out_annotation.f90'])

        module = result[0]
        function = module['functions']['test_out_annotation']
        inputs = function['details']['in']
        self.assertEqual(inputs['x'], {'type': 'real', 'description': 'The first variable', 'dimension': ''})
        outputs = function['details']['out']
        self.assertEqual(outputs['y'], {'type': 'real', 'description': 'The second variable', 'dimension': ''})

    def test_out_annotation_type_mismatch_with_description(self):
        self.fs.create_file(
            '/fake/path/out_annotation.f90',
            contents='''\
    module out_annotation_module
    contains
    !!*
    ! A function with @out annotation
    ! @in x: real The first variable
    ! @out y: integer The second variable
    !*!
    function test_out_annotation(x, y) result(res)
        real, intent(in) :: x
        real, intent(out) :: y
        real :: res
        res = x + y
    end function test_out_annotation
    end module out_annotation_module
                            ''',
        )
        result = process_modules(['/fake/path/out_annotation.f90'])

        module = result[0]
        function = module['functions']['test_out_annotation']
        outputs = function['details']['out']
        self.assertEqual(outputs['y'], {'type': 'real', 'description': 'The second variable', 'dimension': ''})
        #TODO
        # with self.assertLogs('root', level='WARNING') as cm:
        #     process_modules(['/fake/path/in_annotation.f90'])
        # self.assertIn('WARNING:root:Warning: in annotation x type integer does not match value in arguments real', cm.output)

    def test_inout_annotation_name_match_with_description(self):
        self.fs.create_file(
            '/fake/path/inout_annotation.f90',
            contents='''\
    module inout_annotation_module
    contains
    !!*
    ! A function with @inout annotation
    ! @inout x: real The first variable
    ! @in y: real The second variable
    !*!
    function test_inout_annotation(x, y) result(res)
        real, intent(inout) :: x
        real, intent(in) :: y
        real :: res
        res = x + y
    end function test_inout_annotation
    end module inout_annotation_module
                            ''',
        )
        result = process_modules(['/fake/path/inout_annotation.f90'])

        module = result[0]
        function = module['functions']['test_inout_annotation']
        inputs = function['details']['in']
        outputs = function['details']['out']
        self.assertEqual(inputs['x'], {'type': 'real', 'description': 'The first variable', 'dimension': ''})
        self.assertEqual(outputs['x'], {'type': 'real', 'description': 'The first variable', 'dimension': ''})
        self.assertEqual(inputs['y'], {'type': 'real', 'description': 'The second variable', 'dimension': ''})

    def test_inout_annotation_name_mismatch_with_description(self):
        self.fs.create_file(
            '/fake/path/inout_annotation.f90',
            contents='''\
    module inout_annotation_module
    contains
    !!*
    ! A function with @inout annotation
    ! @inout z: real The first variable
    !*!
    function test_inout_annotation(x, y) result(res)
        real, intent(inout) :: x
        real, intent(inout) :: y
        real :: res
        res = x + y
    end function test_inout_annotation
    end module inout_annotation_module
                            ''',
        )
        result = process_modules(['/fake/path/inout_annotation.f90'])

        module = result[0]
        function = module['functions']['test_inout_annotation']
        inputs = function['details']['in']
        outputs = function['details']['out']
        self.assertEqual(inputs['x'], {'type': 'real', 'description': '', 'dimension': ''})
        self.assertEqual(inputs['y'], {'type': 'real', 'description': '', 'dimension': ''})
        self.assertNotIn('z', inputs)
        self.assertEqual(outputs['x'], {'type': 'real', 'description': '', 'dimension': ''})
        self.assertEqual(outputs['y'], {'type': 'real', 'description': '', 'dimension': ''})
        self.assertNotIn('z', outputs)
    #TODO
    #     with self.assertLogs('root', level='WARNING') as cm:
    #         process_modules(['/fake/path/inout_annotation.f90'])
    #     self.assertIn('WARNING:root:Warning: inout annotation z found that is not present in arguments [dict_keys([\'x\'])]', cm.output)

    def test_inout_annotation_type_match_with_description(self):
        self.fs.create_file(
            '/fake/path/inout_annotation.f90',
            contents='''\
    module inout_annotation_module
    contains
    !!*
    ! A function with @inout annotation
    ! @inout x: real The first variable
    ! @inout y: real The second variable
    !*!
    function test_inout_annotation(x, y) result(res)
        real, intent(inout) :: x
        real, intent(in) :: y
        real :: res
        res = x + y
    end function test_inout_annotation
    end module inout_annotation_module
                            ''',
        )
        result = process_modules(['/fake/path/inout_annotation.f90'])

        module = result[0]
        function = module['functions']['test_inout_annotation']
        inputs = function['details']['in']
        outputs = function['details']['out']
        self.assertEqual(inputs['x'], {'type': 'real', 'description': 'The first variable', 'dimension': ''})
        self.assertEqual(outputs['x'], {'type': 'real', 'description': 'The first variable', 'dimension': ''})
        self.assertEqual(inputs['y'], {'type': 'real', 'description': 'The second variable', 'dimension': ''})

    def test_inout_annotation_type_mismatch_with_description(self):
        self.fs.create_file(
            '/fake/path/inout_annotation.f90',
            contents='''\
    module inout_annotation_module
    contains
    !!*
    ! A function with @inout annotation
    ! @inout x: integer The first variable
    ! @in y: integer The second variable
    !*!
    function test_inout_annotation(x, y) result(res)
        real, intent(inout) :: x
        integer, intent(in) :: y
        real :: res
        res = x + y
    end function test_inout_annotation
    end module inout_annotation_module
                            ''',
        )
        result = process_modules(['/fake/path/inout_annotation.f90'])

        module = result[0]
        function = module['functions']['test_inout_annotation']
        inputs = function['details']['in']
        outputs = function['details']['out']
        self.assertEqual(inputs['x'], {'type': 'real', 'description': 'The first variable', 'dimension': ''})
        self.assertEqual(inputs['y'], {'type': 'integer', 'description': 'The second variable', 'dimension': ''})
        self.assertEqual(outputs['x'], {'type': 'real', 'description': 'The first variable', 'dimension': ''})

        #TODO
    #     with self.assertLogs('root', level='WARNING') as cm:
    #         process_modules(['/fake/path/inout_annotation.f90'])
    #     self.assertIn('WARNING:root:Warning: inout annotation x type integer does not match value in arguments real', cm.output)

    def test_return_type_with_description(self):
        self.fs.create_file(
            '/fake/path/return_type.f90',
            contents='''\
    module return_type_module
    contains
    !!*
    ! A function with return type
    ! @return The result
    !*!
    function test_return_type(x, y) result(res)
        real, intent(in) :: x, y
        integer :: res
        res = int(x + y)
    end function test_return_type
    end module return_type_module
                            ''',
        )
        result = process_modules(['/fake/path/return_type.f90'])

        module = result[0]
        function = module['functions']['test_return_type']
        results = function['details']['return']
        self.assertEqual(results['res'], {'type': 'integer', 'description': 'The result', 'dimension': ''})

if __name__ == '__main__':
    unittest.main()
