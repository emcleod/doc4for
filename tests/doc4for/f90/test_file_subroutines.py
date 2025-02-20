import unittest
from pathlib import Path
from pyfakefs.fake_filesystem_unittest import TestCase
from doc4for.f90.generate_file_tree import extract_file_data
from doc4for.models.common import BindingTypeEnum

class TestFunctionSignatures(TestCase):
    maxDiff = None

    def setUp(self):
        self.setUpPyfakefs()

    def test_simple_function_no_args(self):
        self.fs.create_file(
            "/fake/path/subroutines.f90",
            contents="""\
program test_prog
    implicit none
end program test_prog

!!*
! Does nothing.
!*!
SUBROUTINE simple()
    INTEGER :: dummy
    dummy = 42
END SUBROUTINE simple

! Regular comment, not for documentation
SUBROUTINE simple_no_doc()
    INTEGER :: dummy
    dummy = 24  ! Another regular comment
END SUBROUTINE simple_no_doc

subroutine dummy()
    ! This should be ignored
end subroutine dummy

module test_mod
    ! This should also be ignored
end module test_mod
"""
        )
        result = extract_file_data([Path("/fake/path/subroutines.f90")])
        self.assertEqual(len(result), 1)
        file_data = result[0]
        self.assertEqual(len(file_data["subroutines"]), 3)

        # Documented subroutine
        sub_doc = file_data["subroutines"]["simple"]
        expected_doc = {
            "attributes": [],
            "description": "\nDoes nothing.\n\n",
            "arguments": [],
            "out": {},
            "in": {},
            "argument_interfaces": {},
            "binding_type": { "type": BindingTypeEnum.DEFAULT, "name": None}
        }
        self.assertEqual(sub_doc, expected_doc)

        # Undocumented subroutine
        func_no_doc = file_data["subroutines"]["simple_no_doc"]
        expected_no_doc = {
            "attributes": [],
            "description": "",
            "arguments": [],
            "out": {},
            "in": {},
            "argument_interfaces": {},
            "binding_type": { "type": BindingTypeEnum.DEFAULT, "name": None}
        }
        self.assertEqual(func_no_doc, expected_no_doc)

    def test_subroutine_with_arguments(self):
        self.fs.create_file(
            "/fake/path/subroutines.f90",
            contents="""\
    program test_prog
        implicit none
    end program test_prog

    !!*
    ! Calculates the area of a rectangle
    ! @in length  The length of the rectangle
    ! @in width   The width of the rectangle
    ! @in scale The scale, not used
    ! @out area The area of the rectangle
    !*!
    SUBROUTINE rectangle_area(length, width, scale, area)
        REAL, INTENT(IN) :: length, width, scale
        REAL, INTENT(OUT) :: area
        area = length * width
    END SUBROUTINE rectangle_area

    ! Calculates rectangle area without documentation
    SUBROUTINE rectangle_area_no_doc(length, width, area)
        REAL, INTENT(IN) :: length, width  ! Regular inline comment
        REAL, INTENT(OUT) :: area ! Another comment
        area = length * width
    END SUBROUTINE rectangle_area_no_doc

    module geometry
        ! Module contents ignored
    end module geometry
    """
        )
        result = extract_file_data([Path("/fake/path/subroutines.f90")])
        self.assertEqual(len(result), 1)
        file_data = result[0]
        self.assertEqual(len(file_data["subroutines"]), 2)

        # Documented function
        sub_doc = file_data["subroutines"]["rectangle_area"]
        expected_doc = {
            "attributes": [],
            "description": "\nCalculates the area of a rectangle\n\n",
            "arguments": ["length", "width", "scale", "area"],
            "in": {
                "length": {"type": "real", "description": "The length of the rectangle", "dimension": ""},
                "width": {"type": "real", "description": "The width of the rectangle", "dimension": ""},
                "scale": {"type": "real", "description": "The scale, not used", "dimension": ""}
            },
            "out": {
                "area": {"type": "real", "description": "The area of the rectangle", "dimension": ""}
            },
            "argument_interfaces": {},
            "binding_type": { "type": BindingTypeEnum.DEFAULT, "name": None}
        }
        self.assertEqual(sub_doc, expected_doc)

        # Undocumented function
        sub_no_doc = file_data["subroutines"]["rectangle_area_no_doc"]
        expected_no_doc = {
            "attributes": [],
            "description": "",
            "arguments": ["length", "width", "area"],
            "in": {
                "length": {"type": "real", "description": "", "dimension": ""},
                "width": {"type": "real", "description": "", "dimension": ""}
            },
            "out": {
                "area": {"type": "real", "description": "", "dimension": ""},
            },
            "argument_interfaces": {},
            "binding_type": { "type": BindingTypeEnum.DEFAULT, "name": None}
        }
        self.assertEqual(sub_no_doc, expected_no_doc)

    def test_subroutine_with_various_argument_annotations(self):
        self.fs.create_file(
            "/fake/path/subroutines.f90",
            contents="""\
    program test_prog
        implicit none
    end program test_prog

    !!*
    ! Test subroutine with various annotation styles
    ! @in arg1     Input argument 1
    ! @out arg2        Output argument 2
    ! @inout arg3 Argument 3 for both input and output
    ! @in arg4       Argument 4 with no space after colon
    !*!
    subroutine test_annotations(arg1, arg2, arg3, arg4) 
        integer, intent(in) :: arg1
        real, intent(out) :: arg2
        character, intent(inout) :: arg3
        logical, intent(in) :: arg4
        real :: res
    end subroutine test_annotations
    """
        )
        result = extract_file_data([Path("/fake/path/subroutines.f90")])
        file_data = result[0]
        self.assertEqual(len(file_data["subroutines"]), 1)

        # Test various annotations
        sub_annotations = file_data["subroutines"]["test_annotations"]
        expected_annotations = {
            "attributes": [],
            "description": "\nTest subroutine with various annotation styles\n\n",
            "arguments": ["arg1", "arg2", "arg3", "arg4"],
            "in": {
                "arg1": {"type": "integer", "description": "Input argument 1", "dimension": ""},
                "arg3": {"type": "character", "description": "Argument 3 for both input and output", "dimension": ""},
                "arg4": {"type": "logical", "description": "Argument 4 with no space after colon", "dimension": ""}
            },
            "out": {
                "arg2": {"type": "real", "description": "Output argument 2", "dimension": ""},
                "arg3": {"type": "character", "description": "Argument 3 for both input and output", "dimension": ""},
            },
            "argument_interfaces": {},
            "binding_type": { "type": BindingTypeEnum.DEFAULT, "name": None}
        }
        self.assertEqual(sub_annotations, expected_annotations)

    def test_subroutine_with_array_arguments(self):
        self.fs.create_file(
            "/fake/path/subroutines.f90",
            contents="""\
            
    program test
    end program test

    !!*
    ! Subroutine with array arguments
    ! @in matrix  The input matrix
    ! @out vector  The output vector
    !*!
    subroutine process_matrix(matrix, vector)
        real, intent(in) :: matrix(100, 100)
        real, intent(out) :: vector(100)
    end subroutine process_matrix
    """
        )
        result = extract_file_data([Path("/fake/path/subroutines.f90")])
        file_data = result[0]
        
        sub_array = file_data["subroutines"]["process_matrix"]
        expected_array = {
            "attributes": [],
            "description": "\nSubroutine with array arguments\n\n",
            "arguments": ["matrix", "vector"],
            "in": {
                "matrix": {"type": "real", "description": "The input matrix", "dimension": "1:100 &times; 1:100"}
            },
            "out": {
                "vector": {"type": "real", "description": "The output vector", "dimension": "1:100"}
            },
            "argument_interfaces": {},
            "binding_type": { "type": BindingTypeEnum.DEFAULT, "name": None}
        }
        self.assertEqual(sub_array, expected_array)

    def test_various_argument_annotation_styles(self):
        self.fs.create_file(
            "/fake/path/arguments.f90",
            contents="""\
            
    program main
    end program main

    !!*
    ! Subroutine to test many arguments
    ! @in a        Standard input annotation
    ! @in b         No space after variable name
    ! @in c         Space before type
    ! @in d          No spaces around colon
    ! @in e    Array input
    ! @out f          Standard output annotation
    ! @out g           No space after variable name
    ! @out h           Space before type
    ! @out i            No spaces around colon
    ! @out j       Array output
    ! @inout k     Standard inout annotation
    ! @inout l      No space after variable name
    ! @inout m      Space before type
    ! @inout n       No spaces around colon
    ! @inout o  Array inout
    !*!
    subroutine test_many_args(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) 
        integer, intent(in) :: a, b, c, d, e(10)
        real, intent(out) :: f, g, h, i, j(5)
        logical, intent(inout) :: k, l, m, n, o(3)
        character :: res
    end subroutine test_many_args
    """
        )
        result = extract_file_data([Path("/fake/path/arguments.f90")])
        file_data = result[0]
        
        sub_styles = file_data["subroutines"]["test_many_args"]
        
        # Check input arguments
        self.assertEqual(sub_styles['in'], {
            'a': {'type': 'integer', 'description': 'Standard input annotation', 'dimension': ''},
            'b': {'type': 'integer', 'description': 'No space after variable name', 'dimension': ''},
            'c': {'type': 'integer', 'description': 'Space before type', 'dimension': ''},
            'd': {'type': 'integer', 'description': 'No spaces around colon', 'dimension': ''},
            'e': {'type': 'integer', 'description': 'Array input', 'dimension': '1:10'},
            'k': {'type': 'logical', 'description': 'Standard inout annotation', 'dimension': ''},
            'l': {'type': 'logical', 'description': 'No space after variable name', 'dimension': ''},
            'm': {'type': 'logical', 'description': 'Space before type', 'dimension': ''},
            'n': {'type': 'logical', 'description': 'No spaces around colon', 'dimension': ''},
            'o': {'type': 'logical', 'description': 'Array inout', 'dimension': '1:3'}
        })
        
        # Check output arguments
        self.assertEqual(sub_styles['out'], {
            'f': {'type': 'real', 'description': 'Standard output annotation', 'dimension': ''},
            'g': {'type': 'real', 'description': 'No space after variable name', 'dimension': ''},
            'h': {'type': 'real', 'description': 'Space before type', 'dimension': ''},
            'i': {'type': 'real', 'description': 'No spaces around colon', 'dimension': ''},
            'j': {'type': 'real', 'description': 'Array output', 'dimension': '1:5'},
            'k': {'type': 'logical', 'description': 'Standard inout annotation', 'dimension': ''},
            'l': {'type': 'logical', 'description': 'No space after variable name', 'dimension': ''},
            'm': {'type': 'logical', 'description': 'Space before type', 'dimension': ''},
            'n': {'type': 'logical', 'description': 'No spaces around colon', 'dimension': ''},
            'o': {'type': 'logical', 'description': 'Array inout', 'dimension': '1:3'}
        })
        
    def test_argument_annotations_with_complex_descriptions(self):
        self.fs.create_file(
            "/fake/path/complex_args.f90",
            contents="""\
            
    program main
    end program main

    !!*
    ! Subroutine with complex descriptions 
    ! @in matrix                   The input matrix, 
    !                              used for primary calculations
    ! @out stats                   Output statistics array:
    !                              1: mean
    !                              2: median
    !                              3-5: quartiles
    !                              6-10: reserved for future use
    ! @inout flag                  Processing flag:
    !                              0 = not started
    !                              1 = in progress
    !                              2 = completed
    !                              negative = error code
    !*!
    subroutine complex_args(matrix, stats, flag)
        real, intent(in) :: matrix(100, 100)
        real, intent(out) :: stats(10)
        integer, intent(inout) :: flag
    end subroutine complex_args
    """
        )
        result = extract_file_data([Path("/fake/path/complex_args.f90")])
        file_data = result[0]
        
        sub_complex = file_data["subroutines"]["complex_args"]
        
        # Check complex descriptions
        self.assertEqual(sub_complex['in']['matrix'], {
            'type': 'real',
            'description': 'The input matrix, used for primary calculations',
            'dimension': '1:100 &times; 1:100'
        })
        
        expected_stats_desc = ('Output statistics array: '
                            '1: mean '
                            '2: median '
                            '3-5: quartiles '
                            '6-10: reserved for future use')
        self.assertEqual(sub_complex['out']['stats'], {
            'type': 'real',
            'description': expected_stats_desc,
            'dimension': '1:10'
        })
        
        expected_flag_desc = ('Processing flag: '
                            '0 = not started '
                            '1 = in progress '
                            '2 = completed '
                            'negative = error code')
        self.assertEqual(sub_complex['in']['flag'], {
            'type': 'integer',
            'description': expected_flag_desc,
            'dimension': ''
        })
        self.assertEqual(sub_complex['out']['flag'], {
            'type': 'integer',
            'description': expected_flag_desc,
            'dimension': ''
        })
        
    def test_unnecessary_return_annotation(self):
        self.fs.create_file(
            "/fake/path/complex_args.f90",
            contents="""\
            
    program main
    end program main

    !!*
    ! Subroutine with complex argument descriptions
    ! @in matrix                   The input matrix, 
    !                              used for primary calculations
    ! @out stats                   Output statistics array:
    !                              1: mean
    !                              2: median
    !                              3-5: quartiles
    !                              6-10: reserved for future use
    ! @inout flag                  Processing flag:
    !                              0 = not started
    !                              1 = in progress
    !                              2 = completed
    !                              negative = error code
    ! @return                      True if processing was successful,
    !                              False otherwise
    !*!
    subroutine complex_args(matrix, stats, flag)
        real, intent(in) :: matrix(100, 100)
        real, intent(out) :: stats(10)
        integer, intent(inout) :: flag
    end subroutine complex_args
    """
        )
        result = extract_file_data([Path("/fake/path/complex_args.f90")])
        file_data = result[0]
        
        sub_complex = file_data["subroutines"]["complex_args"]
        
        # Check complex descriptions
        self.assertEqual(sub_complex['in']['matrix'], {
            'type': 'real',
            'description': 'The input matrix, used for primary calculations',
            'dimension': '1:100 &times; 1:100'
        })
        
        expected_stats_desc = ('Output statistics array: '
                            '1: mean '
                            '2: median '
                            '3-5: quartiles '
                            '6-10: reserved for future use')
        self.assertEqual(sub_complex['out']['stats'], {
            'type': 'real',
            'description': expected_stats_desc,
            'dimension': '1:10'
        })
        
        expected_flag_desc = ('Processing flag: '
                            '0 = not started '
                            '1 = in progress '
                            '2 = completed '
                            'negative = error code')
        self.assertEqual(sub_complex['in']['flag'], {
            'type': 'integer',
            'description': expected_flag_desc,
            'dimension': ''
        })
        self.assertEqual(sub_complex['out']['flag'], {
            'type': 'integer',
            'description': expected_flag_desc,
            'dimension': ''
        })
        

if __name__ == '__main__':
    unittest.main()
