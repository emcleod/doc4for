import unittest
from pathlib import Path
from pyfakefs.fake_filesystem_unittest import TestCase
from doc4for.f90.generate_module_tree import extract_module_data

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
    ! @in x
    ! @in y
    !*!
    function test_in_annotation(x, y) result(res)
        real, intent(in) :: x, y
        real :: res
        res = x + y
    end function test_in_annotation
    end module in_annotation_module
                            ''',
        )
        result = extract_module_data([Path('/fake/path/in_annotation.f90')])

        module = result[0]
        function = module['functions']['test_in_annotation']
        inputs = function['in']
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
    ! @in z
    !*!
    function test_in_annotation(x, y) result(res)
        real, intent(in) :: x, y
        real :: res
        res = x + y
    end function test_in_annotation
    end module in_annotation_module
                            ''',
        )
        result = extract_module_data([Path('/fake/path/in_annotation.f90')])

        module = result[0]
        function = module['functions']['test_in_annotation']
        inputs = function['in']
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
    ! @in x
    !*!
    function test_in_annotation(x, y) result(res)
        real, intent(in) :: x, y
        real :: res
        res = x + y
    end function test_in_annotation
    end module in_annotation_module
                            ''',
        )
        result = extract_module_data([Path('/fake/path/in_annotation.f90')])

        module = result[0]
        function = module['functions']['test_in_annotation']
        inputs = function['in']
        self.assertEqual(inputs['x'], {'type': 'real', 'description': '', 'dimension': ''})

    def test_in_annotation_type_mismatch(self):
        self.fs.create_file(
            '/fake/path/in_annotation.f90',
            contents='''\
    module in_annotation_module
    contains
    !!*
    ! A function with @in annotation
    ! @in x
    !*!
    function test_in_annotation(x, y) result(res)
        real, intent(in) :: x, y
        real :: res
        res = x + y
    end function test_in_annotation
    end module in_annotation_module
                            ''',
        )
        result = extract_module_data([Path('/fake/path/in_annotation.f90')])

        module = result[0]
        function = module['functions']['test_in_annotation']
        inputs = function['in']
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
    ! @out res
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
        result = extract_module_data([Path('/fake/path/out_annotation.f90')])

        module = result[0]
        function = module['functions']['test_out_annotation']
        inputs = function['in']
        self.assertEqual(inputs['x'], {'type': 'real', 'description': '', 'dimension': ''})
        outputs = function['out']
        self.assertEqual(outputs['y'], {'type': 'real', 'description': '', 'dimension': ''})

    def test_out_annotation_name_mismatch(self):
        self.fs.create_file(
            '/fake/path/out_annotation.f90',
            contents='''\
    module out_annotation_module
    contains
    !!*
    ! A function with @out annotation
    ! @out z
    !*!
    function test_out_annotation(x, y) result(res)
        real, intent(in) :: x, y
        real :: res
        res = x + y
    end function test_out_annotation
    end module out_annotation_module
                            ''',
        )
        result = extract_module_data([Path('/fake/path/out_annotation.f90')])

        module = result[0]
        function = module['functions']['test_out_annotation']
        inputs = function['in']
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
    ! @out y
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
        result = extract_module_data([Path('/fake/path/out_annotation.f90')])

        module = result[0]
        function = module['functions']['test_out_annotation']
        outputs = function['out']
        self.assertEqual(outputs['y'], {'type': 'real', 'description': '', 'dimension': ''})

    def test_out_annotation_type_mismatch(self):
        self.fs.create_file(
            '/fake/path/out_annotation.f90',
            contents='''\
    module out_annotation_module
    contains
    !!*
    ! A function with @out annotation
    ! @out y
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
        result = extract_module_data([Path('/fake/path/out_annotation.f90')])

        module = result[0]
        function = module['functions']['test_out_annotation']
        outputs = function['out']
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
    ! @inout x
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
        result = extract_module_data([Path('/fake/path/inout_annotation.f90')])

        module = result[0]
        function = module['functions']['test_inout_annotation']
        inputs = function['in']
        outputs = function['out']
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
    ! @inout z
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
        result = extract_module_data([Path('/fake/path/inout_annotation.f90')])

        module = result[0]
        function = module['functions']['test_inout_annotation']
        inputs = function['in']
        outputs = function['out']
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
    ! @inout x
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
        result = extract_module_data([Path('/fake/path/inout_annotation.f90')])

        module = result[0]
        function = module['functions']['test_inout_annotation']
        inputs = function['in']
        outputs = function['out']
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
    ! @inout x
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
        result = extract_module_data([Path('/fake/path/inout_annotation.f90')])

        module = result[0]
        function = module['functions']['test_inout_annotation']
        inputs = function['in']
        outputs = function['out']
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
        result = extract_module_data([Path('/fake/path/return_type.f90')])

        module = result[0]
        function = module['functions']['test_return_type']
        results = function['return']
        self.assertEqual(results['res'], {'type': 'integer', 'description': '', 'dimension': ''})

    def test_in_annotation_name_mismatch_with_description(self):
        self.fs.create_file(
            '/fake/path/in_annotation.f90',
            contents='''\
    module in_annotation_module
    contains
    !!*
    ! A function with @in annotation
    ! @in z The first variable
    !*!
    function test_in_annotation(x, y) result(res)
        real, intent(in) :: x, y
        real :: res
        res = x + y
    end function test_in_annotation
    end module in_annotation_module
                            ''',
        )
        result = extract_module_data([Path('/fake/path/in_annotation.f90')])

        module = result[0]
        function = module['functions']['test_in_annotation']
        inputs = function['in']
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
    ! @in x The first variable
    !*!
    function test_in_annotation(x, y) result(res)
        real, intent(in) :: x, y
        real :: res
        res = x + y
    end function test_in_annotation
    end module in_annotation_module
                            ''',
        )
        result = extract_module_data([Path('/fake/path/in_annotation.f90')])

        module = result[0]
        function = module['functions']['test_in_annotation']
        inputs = function['in']
        self.assertEqual(inputs['x'], {'type': 'real', 'description': 'The first variable', 'dimension': ''})

    def test_in_annotation_type_mismatch_with_description(self):
        self.fs.create_file(
            '/fake/path/in_annotation.f90',
            contents='''\
    module in_annotation_module
    contains
    !!*
    ! A function with @in annotation
    ! @in x The first variable
    !*!
    function test_in_annotation(x, y) result(res)
        real, intent(in) :: x, y
        real :: res
        res = x + y
    end function test_in_annotation
    end module in_annotation_module
                            ''',
        )
        result = extract_module_data([Path('/fake/path/in_annotation.f90')])

        module = result[0]
        function = module['functions']['test_in_annotation']
        inputs = function['in']
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
    ! @in x The first variable
    ! @out y The second variable
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
        result = extract_module_data([Path('/fake/path/out_annotation.f90')])

        module = result[0]
        function = module['functions']['test_out_annotation']
        inputs = function['in']
        self.assertEqual(inputs['x'], {'type': 'real', 'description': 'The first variable', 'dimension': ''})
        outputs = function['out']
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
        result = extract_module_data([Path('/fake/path/out_annotation.f90')])

        module = result[0]
        function = module['functions']['test_out_annotation']
        inputs = function['in']
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
    ! @in x The first variable
    ! @out y The second variable
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
        result = extract_module_data([Path('/fake/path/out_annotation.f90')])

        module = result[0]
        function = module['functions']['test_out_annotation']
        inputs = function['in']
        self.assertEqual(inputs['x'], {'type': 'real', 'description': 'The first variable', 'dimension': ''})
        outputs = function['out']
        self.assertEqual(outputs['y'], {'type': 'real', 'description': 'The second variable', 'dimension': ''})

    def test_out_annotation_type_mismatch_with_description(self):
        self.fs.create_file(
            '/fake/path/out_annotation.f90',
            contents='''\
    module out_annotation_module
    contains
    !!*
    ! A function with @out annotation
    ! @in x The first variable
    ! @out y The second variable
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
        result = extract_module_data([Path('/fake/path/out_annotation.f90')])

        module = result[0]
        function = module['functions']['test_out_annotation']
        outputs = function['out']
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
    ! @inout x The first variable
    ! @in y The second variable
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
        result = extract_module_data([Path('/fake/path/inout_annotation.f90')])

        module = result[0]
        function = module['functions']['test_inout_annotation']
        inputs = function['in']
        outputs = function['out']
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
    ! @inout z The first variable
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
        result = extract_module_data([Path('/fake/path/inout_annotation.f90')])

        module = result[0]
        function = module['functions']['test_inout_annotation']
        inputs = function['in']
        outputs = function['out']
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
    ! @inout x The first variable
    ! @inout y The second variable
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
        result = extract_module_data([Path('/fake/path/inout_annotation.f90')])

        module = result[0]
        function = module['functions']['test_inout_annotation']
        inputs = function['in']
        outputs = function['out']
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
    ! @inout x The first variable
    ! @in y The second variable
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
        result = extract_module_data([Path('/fake/path/inout_annotation.f90')])

        module = result[0]
        function = module['functions']['test_inout_annotation']
        inputs = function['in']
        outputs = function['out']
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
    ! @return  The result
    !*!
    function test_return_type(x, y) result(res)
        real, intent(in) :: x, y
        integer :: res
        res = int(x + y)
    end function test_return_type
    end module return_type_module
                            ''',
        )
        result = extract_module_data([Path('/fake/path/return_type.f90')])

        module = result[0]
        function = module['functions']['test_return_type']
        results = function['return']
        self.assertEqual(results['res'], {'type': 'integer', 'description': 'The result', 'dimension': ''})

if __name__ == '__main__':
    unittest.main()
