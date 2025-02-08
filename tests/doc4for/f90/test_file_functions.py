import unittest
from pathlib import Path
from pyfakefs.fake_filesystem_unittest import TestCase
from doc4for.f90.generate_file_tree import extract_file_data


class TestFunctionSignatures(TestCase):
    maxDiff = None

    def setUp(self):
        self.setUpPyfakefs()

    def test_simple_function_no_args(self):
        self.fs.create_file(
            "/fake/path/functions.f90",
            contents="""\
program test_prog
    implicit none
end program test_prog

!!*
! Returns a constant value
! @return Always returns 42
!*!
FUNCTION simple()
    INTEGER :: simple
    simple = 42
END FUNCTION simple

! Regular comment, not for documentation
FUNCTION simple_no_doc()
    INTEGER :: simple_no_doc
    simple_no_doc = 24  ! Another regular comment
END FUNCTION simple_no_doc

subroutine dummy()
    ! This should be ignored
end subroutine dummy

module test_mod
    ! This should also be ignored
end module test_mod
"""
        )
        result = extract_file_data([Path("/fake/path/functions.f90")])
        self.assertEqual(len(result), 1)
        file_data = result[0]
        self.assertEqual(len(file_data["functions"]), 2)

        # Documented function
        func_doc = file_data["functions"]["simple"]
        expected_doc = {
            "attributes": [],
            "description": "\nReturns a constant value\n\n",
            "arguments": [],
            "out": {},
            "in": {},
            "return": {"simple": {"description": "Always returns 42", "dimension": "", "type": "integer"}},
            "binding_type": "",
            "interface": ""
        }
        self.assertEqual(func_doc, expected_doc)

        # Undocumented function
        func_no_doc = file_data["functions"]["simple_no_doc"]
        expected_no_doc = {
            "attributes": [],
            "description": "",
            "arguments": [],
            "out": {},
            "in": {},
            "return": {"simple_no_doc": {"description": "", "dimension": "", "type": "integer"}},
            "binding_type": "",
            "interface": ""
        }
        self.assertEqual(func_no_doc, expected_no_doc)

    def test_function_with_arguments(self):
        self.fs.create_file(
            "/fake/path/functions.f90",
            contents="""\
    program test_prog
        implicit none
    end program test_prog

    !!*
    ! Calculates the area of a rectangle
    ! @in length   : real      The length of the rectangle
    ! @in width: real   The width of the rectangle
    ! @in scale :real The scale, not used
    ! @return  Area of the rectangle
    !*!
    REAL FUNCTION rectangle_area(length, width, scale)
        REAL, INTENT(IN) :: length, width, scale
        rectangle_area = length * width
    END FUNCTION rectangle_area

    ! Calculates rectangle area without documentation
    REAL FUNCTION rectangle_area_no_doc(length, width)
        REAL, INTENT(IN) :: length, width  ! Regular inline comment
        rectangle_area_no_doc = length * width
    END FUNCTION rectangle_area_no_doc

    module geometry
        ! Module contents ignored
    end module geometry
    """
        )
        result = extract_file_data([Path("/fake/path/functions.f90")])
        self.assertEqual(len(result), 1)
        file_data = result[0]
        self.assertEqual(len(file_data["functions"]), 2)

        # Documented function
        func_doc = file_data["functions"]["rectangle_area"]
        expected_doc = {
            "attributes": [],
            "description": "\nCalculates the area of a rectangle\n\n",
            "arguments": ["length", "width", "scale"],
            "in": {
                "length": {"type": "real", "description": "The length of the rectangle", "dimension": ""},
                "width": {"type": "real", "description": "The width of the rectangle", "dimension": ""},
                "scale": {"type": "real", "description": "The scale, not used", "dimension": ""}
            },
            "out": {},
            "return": {"rectangle_area": {"description": "Area of the rectangle", "dimension": "", "type": "real"}},
            "binding_type": "",
            "interface": ""
        }
        self.assertEqual(func_doc, expected_doc)

        # Undocumented function
        func_no_doc = file_data["functions"]["rectangle_area_no_doc"]
        expected_no_doc = {
            "attributes": [],
            "description": "",
            "arguments": ["length", "width"],
            "in": {
                "length": {"type": "real", "description": "", "dimension": ""},
                "width": {"type": "real", "description": "", "dimension": ""}
            },
            "out": {},
            "return": {"rectangle_area_no_doc": {"description": "", "dimension": "", "type": "real"}},
            "binding_type": "",
            "interface": ""
        }
        self.assertEqual(func_no_doc, expected_no_doc)

    def test_function_with_various_argument_annotations(self):
        self.fs.create_file(
            "/fake/path/functions.f90",
            contents="""\
    program test_prog
        implicit none
    end program test_prog

    !!*
    ! Test function with various annotation styles
    ! @in arg1 : integer     Input argument 1
    ! @out arg2: real        Output argument 2
    ! @inout arg3 :character Argument 3 for both input and output
    ! @in arg4:logical       Argument 4 with no space after colon
    ! @return res : real     The result of the calculation
    !*!
    function test_annotations(arg1, arg2, arg3, arg4) result(res)
        integer, intent(in) :: arg1
        real, intent(out) :: arg2
        character, intent(inout) :: arg3
        logical, intent(in) :: arg4
        real :: res
    end function test_annotations

    !!*
    ! Function with unnamed return
    ! @return: integer  Always returns 42
    !*!
    function unnamed_return()
        integer :: unnamed_return
        unnamed_return = 42
    end function unnamed_return
    """
        )
        result = extract_file_data([Path("/fake/path/functions.f90")])
        file_data = result[0]
        self.assertEqual(len(file_data["functions"]), 2)

        func_annotations = file_data["functions"]["test_annotations"]
        expected_annotations = {
            "attributes": [],
            "description": "\nTest function with various annotation styles\n\n",
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
            "return": {"res": {"description": "The result of the calculation", "dimension": "", "type": "real"}},
            "binding_type": "",
            "interface": ""
        }
        self.assertEqual(func_annotations, expected_annotations)

        func_unnamed = file_data["functions"]["unnamed_return"]
        expected_unnamed = {
            "attributes": [],
            "description": "Function with unnamed return",
            "arguments": [],
            "in": {},
            "out": {},
            "return": {"unnamed_return": {"description": "Always returns 42", "dimension": "", "type": "integer"}},
            "binding_type": "",
            "interface": ""
        }
        self.assertEqual(func_unnamed, expected_unnamed)

    def test_function_with_array_arguments(self):
        self.fs.create_file(
            "/fake/path/functions.f90",
            contents="""\
            
    program test
    end program test

    !!*
    ! Function with array arguments
    ! @in matrix : real  The input matrix
    ! @out vector: real  The output vector
    ! @return:logical    Whether the operation was successful
    !*!
    function process_matrix(matrix, vector)
        real, intent(in) :: matrix(100, 100)
        real, intent(out) :: vector(100)
        logical :: process_matrix
    end function process_matrix
    """
        )
        result = extract_file_data([Path("/fake/path/functions.f90")])
        file_data = result[0]

        func_array = file_data["functions"]["process_matrix"]
        expected_array = {
            "attributes": [],
            "description": "\nFunction with array arguments\n\n",
            "arguments": ["matrix", "vector"],
            "in": {
                "matrix": {"type": "real", "description": "The input matrix", "dimension": "1:100 &times; 1:100"}
            },
            "out": {
                "vector": {"type": "real", "description": "The output vector", "dimension": "1:100"}
            },
            "return": {"process_matrix": {"description": "Whether the operation was successful", "dimension": "", "type": "logical"}},
            "binding_type": "",
            "interface": ""
        }
        self.assertEqual(func_array, expected_array)

    def test_various_return_annotation_styles(self):
        self.fs.create_file(
            "/fake/path/returns.f90",
            contents="""\
    program test_prog
        implicit none
    end program test_prog

    !!*
    ! Function with colon-attached return type
    ! @return:real Description for colon-attached return type
    !*!
    function attached_type()
        real :: attached_type
    end function attached_type

    !!*
    ! Function with spaced return type
    ! @return : integer  Description for spaced return type
    !*!
    function spaced_type()
        integer :: spaced_type
    end function spaced_type

    !!*
    ! Function with colon-attached spaced return type
    ! @return :logical   Description for colon-attached spaced return type
    !*!
    function attached_spaced_type()
        logical :: attached_spaced_type
    end function attached_spaced_type

    !!*
    ! Named return with colon-attached type
    ! @return result:character Named return with attached type description
    !*!
    function named_attached(x) result(result)
        real, intent(in) :: x
        character :: result
    end function named_attached

    !!*
    ! Named return with spaced type
    ! @return result : complex  Named return with spaced type description
    !*!
    function named_spaced(y) result(result)
        real, intent(in) :: y
        complex :: result
    end function named_spaced
    """
        )
        result = extract_file_data([Path("/fake/path/returns.f90")])
        file_data = result[0]
        self.assertEqual(len(file_data["functions"]), 5)

        # Colon-attached return type
        func_attached = file_data["functions"]["attached_type"]
        expected_attached = {
            "attributes": [],
            "description": "\nFunction with colon-attached return type\n\n",
            "arguments": [],
            "in": {},
            "out": {},
            "return": {"attached_type": {"description": "Description for colon-attached return type",
                                         "dimension": "", "type": "real"}},
            "binding_type": "",
            "interface": ""
        }
        self.assertEqual(func_attached, expected_attached)

        # Spaced return type
        func_spaced = file_data["functions"]["spaced_type"]
        expected_spaced = {
            "attributes": [],
            "description": "\nFunction with spaced return type\n\n",
            "arguments": [],
            "in": {},
            "out": {},
            "return": {"spaced_type": {"description": "Description for spaced return type",
                                       "dimension": "", "type": "integer"}},
            "binding_type": "",
            "interface": ""
        }
        self.assertEqual(func_spaced, expected_spaced)

        # Colon-attached spaced return type
        func_attached_spaced = file_data["functions"]["attached_spaced_type"]
        expected_attached_spaced = {
            "attributes": [],
            "description": "\nFunction with colon-attached spaced return type\n\n",
            "arguments": [],
            "in": {},
            "out": {},
            "return": {"attached_spaced_type": {"description": "Description for colon-attached spaced return type",
                                                "dimension": "", "type": "logical"}},
            "binding_type": "",
            "interface": ""
        }
        self.assertEqual(func_attached_spaced, expected_attached_spaced)

        # Named return with attached type
        func_named_attached = file_data["functions"]["named_attached"]
        expected_named_attached = {
            "attributes": [],
            "description": "\nNamed return with colon-attached type\n\n",
            "arguments": ["x"],
            "in": {"x": {"type": "real", "description": "", "dimension": ""}},
            "out": {},
            "return": {"result": {"description": "Named return with attached type description",
                                  "dimension": "", "type": "character"}},
            "binding_type": "",
            "interface": ""
        }
        self.assertEqual(func_named_attached, expected_named_attached)

        # Named return with spaced type
        func_named_spaced = file_data["functions"]["named_spaced"]
        expected_named_spaced = {
            "attributes": [],
            "description": "\nNamed return with spaced type\n\n",
            "arguments": ["y"],
            "in": {"y": {"type": "real", "description": "", "dimension": ""}},
            "out": {},
            "return": {"result": {"description": "Named return with spaced type description",
                                  "dimension": "", "type": "complex"}},
            "binding_type": "",
            "interface": ""
        }
        self.assertEqual(func_named_spaced, expected_named_spaced)

    def test_various_argument_annotation_styles(self):
        self.fs.create_file(
            "/fake/path/arguments.f90",
            contents="""\
            
    program main
    end program main

    !!*
    ! Function to test all argument annotation styles
    ! @in a : integer        Standard input annotation
    ! @in b: integer         No space after variable name
    ! @in c :integer         Space before type
    ! @in d:integer          No spaces around colon
    ! @in e : integer(10)    Array input
    ! @out f : real          Standard output annotation
    ! @out g: real           No space after variable name
    ! @out h :real           Space before type
    ! @out i:real            No spaces around colon
    ! @out j : real(5)       Array output
    ! @inout k : logical     Standard inout annotation
    ! @inout l: logical      No space after variable name
    ! @inout m :logical      Space before type
    ! @inout n:logical       No spaces around colon
    ! @inout o : logical(3)  Array inout
    ! @inout p: integer(4)   Array input and output
    ! @inout q :integer(2)   Array inout
    ! @return : character    Return type
    !*!
    function test_arg_styles(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q) result(res)
        integer, intent(in) :: a, b, c, d, e(10)
        real, intent(out) :: f, g, h, i, j(5)
        logical, intent(inout) :: k, l, m, n, o(3)
        integer, intent(inout) :: p(4), q(2)
        character :: res
    end function test_arg_styles
    """
        )
        result = extract_file_data([Path("/fake/path/arguments.f90")])
        file_data = result[0]

        func_styles = file_data["functions"]["test_arg_styles"]
        self.assertEqual(func_styles["description"], "\nFunction to test all argument annotation styles\n\n")
        # Check input arguments
        self.assertEqual(func_styles['in'], {
            'a': {'type': 'integer', 'description': 'Standard input annotation', 'dimension': ''},
            'b': {'type': 'integer', 'description': 'No space after variable name', 'dimension': ''},
            'c': {'type': 'integer', 'description': 'Space before type', 'dimension': ''},
            'd': {'type': 'integer', 'description': 'No spaces around colon', 'dimension': ''},
            'e': {'type': 'integer', 'description': 'Array input', 'dimension': '1:10'},
            'k': {'type': 'logical', 'description': 'Standard inout annotation', 'dimension': ''},
            'l': {'type': 'logical', 'description': 'No space after variable name', 'dimension': ''},
            'm': {'type': 'logical', 'description': 'Space before type', 'dimension': ''},
            'n': {'type': 'logical', 'description': 'No spaces around colon', 'dimension': ''},
            'o': {'type': 'logical', 'description': 'Array inout', 'dimension': '1:3'},
            'p': {'type': 'integer', 'description': 'Array input and output', 'dimension': '1:4'},
            'q': {'type': 'integer', 'description': 'Array inout', 'dimension': '1:2'}
        })

        # Check output arguments
        self.assertEqual(func_styles['out'], {
            'f': {'type': 'real', 'description': 'Standard output annotation', 'dimension': ''},
            'g': {'type': 'real', 'description': 'No space after variable name', 'dimension': ''},
            'h': {'type': 'real', 'description': 'Space before type', 'dimension': ''},
            'i': {'type': 'real', 'description': 'No spaces around colon', 'dimension': ''},
            'j': {'type': 'real', 'description': 'Array output', 'dimension': '1:5'},
            'k': {'type': 'logical', 'description': 'Standard inout annotation', 'dimension': ''},
            'l': {'type': 'logical', 'description': 'No space after variable name', 'dimension': ''},
            'm': {'type': 'logical', 'description': 'Space before type', 'dimension': ''},
            'n': {'type': 'logical', 'description': 'No spaces around colon', 'dimension': ''},
            'o': {'type': 'logical', 'description': 'Array inout', 'dimension': '1:3'},
            'p': {'type': 'integer', 'description': 'Array input and output', 'dimension': '1:4'},
            'q': {'type': 'integer', 'description': 'Array inout', 'dimension': '1:2'}
        })

        # Check return
        self.assertEqual(func_styles['return'], {
            'res': {'type': 'character', 'description': 'Return type', 'dimension': ''}
        })

    def test_argument_annotations_with_complex_descriptions(self):
        self.fs.create_file(
            "/fake/path/complex_args.f90",
            contents="""\
            
    program main
    end program main

    !!*
    ! Function with complex argument descriptions
    ! @in matrix : real            The input matrix, 
    !                              used for primary calculations
    ! @out stats: real             Output statistics array:
    !                              1: mean
    !                              2: median
    !                              3-5: quartiles
    !                              6-10: reserved for future use
    ! @inout flag : integer        Processing flag:
    !                              0 = not started
    !                              1 = in progress
    !                              2 = completed
    !                              negative = error code
    ! @return : logical            True if processing was successful,
    !                              False otherwise
    !*!
    function complex_args(matrix, stats, flag)
        real, intent(in) :: matrix(100, 100)
        real, intent(out) :: stats(10)
        integer, intent(inout) :: flag
        logical :: complex_args
    end function complex_args
    """
        )
        result = extract_file_data([Path("/fake/path/complex_args.f90")])
        file_data = result[0]

        func_complex = file_data["functions"]["complex_args"]

        # Check complex descriptions
        self.assertEqual(func_complex['in']['matrix'], {
            'type': 'real',
            'description': 'The input matrix, used for primary calculations',
            'dimension': '1:100 &times; 1:100'
        })

        expected_stats_desc = ('Output statistics array: '
                               '1: mean '
                               '2: median '
                               '3-5: quartiles '
                               '6-10: reserved for future use')
        self.assertEqual(func_complex['out']['stats'], {
            'type': 'real',
            'description': expected_stats_desc,
            'dimension': '1:10'
        })

        expected_flag_desc = ('Processing flag: '
                              '0 = not started '
                              '1 = in progress '
                              '2 = completed '
                              'negative = error code')
        self.assertEqual(func_complex['in']['flag'], {
            'type': 'integer',
            'description': expected_flag_desc,
            'dimension': ''
        })
        self.assertEqual(func_complex['out']['flag'], {
            'type': 'integer',
            'description': expected_flag_desc,
            'dimension': ''
        })

        # Check return description
        expected_return_desc = ('True if processing was successful, '
                                'False otherwise')
        self.assertEqual(func_complex['return']['complex_args'], {
            'type': 'logical',
            'description': expected_return_desc,
            'dimension': ''
        })

    def test_return_with_arrays(self):
        self.fs.create_file(
            "/fake/path/array_returns.f90",
            contents="""\
        program test_prog
            implicit none
        end program test_prog

        !!*
        ! Function returning an array
        ! @return: real(10)      Returns a vector of results
        !*!
        function vector_return()
            real :: vector_return(10)
        end function vector_return

        !!*
        ! Function with named array return
        ! @return result: integer(5,5)  Returns a matrix of counts
        !*!
        function matrix_return() result(result)
            integer :: result(5,5)
        end function matrix_return

        !!*
        ! Function with different spacing in array spec
        ! @return res : complex ( 3 )  Returns complex values
        !*!
        function spaced_array() result(res)
            complex :: res(3)
        end function spaced_array

        !!*
        ! Function with no spaces around array spec
        ! @return output:real(100)  Returns many values
        !*!
        function dense_array() result(output)
            real :: output(100)
        end function dense_array
        """
        )
        result = extract_file_data([Path("/fake/path/array_returns.f90")])
        file_data = result[0]
        self.assertEqual(len(file_data["functions"]), 4)

        # Unnamed array return
        func_vector = file_data["functions"]["vector_return"]
        expected_vector = {
            "attributes": [],
            "description": "\nFunction returning an array\n\n",
            "arguments": [],
            "in": {},
            "out": {},
            "return": {"vector_return": {
                "description": "Returns a vector of results",
                "dimension": "1:10",
                "type": "real"
            }},
            "binding_type": "",
            "interface": ""
        }
        self.assertEqual(func_vector, expected_vector)

        # Named matrix return
        func_matrix = file_data["functions"]["matrix_return"]
        expected_matrix = {
            "attributes": [],
            "description": "\nFunction with named array return\n\n",
            "arguments": [],
            "in": {},
            "out": {},
            "return": {"result": {
                "description": "Returns a matrix of counts",
                "dimension": "1:5 &times; 1:5",
                "type": "integer"
            }},
            "binding_type": "",
            "interface": ""
        }
        self.assertEqual(func_matrix, expected_matrix)

        # Spaced array spec
        func_spaced = file_data["functions"]["spaced_array"]
        expected_spaced = {
            "attributes": [],
            "description": "\nFunction with different spacing in array spec\n\n",
            "arguments": [],
            "in": {},
            "out": {},
            "return": {"res": {
                "description": "Returns complex values",
                "dimension": "1:3",
                "type": "complex"
            }},
            "binding_type": "",
            "interface": ""
        }
        self.assertEqual(func_spaced, expected_spaced)

        # Dense array spec
        func_dense = file_data["functions"]["dense_array"]
        expected_dense = {
            "attributes": [],
            "description": "\nFunction with no spaces around array spec\n\n",
            "arguments": [],
            "in": {},
            "out": {},
            "return": {"output": {
                "description": "Returns many values",
                "dimension": "1:100",
                "type": "real"
            }},
            "binding_type": "",
            "interface": ""
        }
        self.assertEqual(func_dense, expected_dense)

    def test_coarray_annotations(self):
        self.fs.create_file(
            "/fake/path/coarrays.f90",
            contents="""\
        program test_prog
            implicit none
        end program test_prog

        !!*
        ! Function with coarray return
        ! @return: real[*]      Returns a coarray distributed across images
        !*!
        function coarray_return()
            real :: coarray_return[*]
        end function coarray_return

        !!*
        ! Function with array and coarray specs
        ! @return result: integer(10)[2,*]  Returns a distributed array of counts
        !*!
        function array_coarray() result(result)
            integer :: result(10)[2,*]
        end function array_coarray

        !!*
        ! Function with coarray arguments
        ! @in  data : real[*]         Input coarray
        ! @out res  : real(3)[3,*]    Output array coarray
        ! @return   : logical         Success status
        !*!
        function process_coarrays(data, res)
            real, intent(in) :: data[*]
            real, intent(out) :: res(3)[3,*]
            logical :: process_coarrays
        end function process_coarrays

        !!*
        ! Function with different spacing in coarray specs
        ! @in  vec : complex ( 3 ) [ 2, * ]  Input vector
        ! @out mat : real(2,2)[*]            Output matrix
        ! @return result : integer[*]         Distributed status codes
        !*!
        function spaced_specs(vec, mat) result(result)
            complex, intent(in) :: vec(3)[2,*]
            real, intent(out) :: mat(2,2)[*]
            integer :: result[*]
        end function spaced_specs
        """
        )
        result = extract_file_data([Path("/fake/path/coarrays.f90")])
        file_data = result[0]
        self.assertEqual(len(file_data["functions"]), 4)

        # Simple coarray return
        func_coarray = file_data["functions"]["coarray_return"]
        expected_coarray = {
            "attributes": [],
            "description": "\nFunction with coarray return\n\n",
            "arguments": [],
            "in": {},
            "out": {},
            "return": {"coarray_return": {
                "description": "Returns a coarray distributed across images",
                "dimension": "* (assumed-size)",
                "type": "real"
            }},
            "binding_type": "",
            "interface": ""
        }
        self.assertEqual(func_coarray, expected_coarray)

        # Array with coarray return
        func_array_coarray = file_data["functions"]["array_coarray"]
        expected_array_coarray = {
            "attributes": [],
            "description": "\nFunction with array and coarray specs\n\n",
            "arguments": [],
            "in": {},
            "out": {},
            "return": {"result": {
                "description": "Returns a distributed array of counts",
                "dimension": "1:10 &times; 1:2 &times; * (assumed-size)",
                "type": "integer"
            }},
            "binding_type": "",
            "interface": ""
        }
        self.assertEqual(func_array_coarray, expected_array_coarray)

        # Function with coarray arguments
        func_process = file_data["functions"]["process_coarrays"]
        expected_process = {
            "attributes": [],
            "description": "\nFunction with coarray arguments\n\n",
            "arguments": ["data", "res"],
            "in": {
                "data": {
                    "type": "real",
                    "description": "Input coarray",
                    "dimension": "* (assumed-size)"
                }
            },
            "out": {
                "res": {
                    "type": "real",
                    "description": "Output array coarray",
                    "dimension": "1:3 &times; 1:3 &times; * (assumed-size)"
                }
            },
            "return": {"process_coarrays": {
                "description": "Success status",
                "dimension": "",
                "type": "logical"
            }},
            "binding_type": "",
            "interface": ""
        }
        self.assertEqual(func_process, expected_process)

        # Function with varied spacing in specs
        func_spaced = file_data["functions"]["spaced_specs"]
        expected_spaced = {
            "attributes": [],
            "description": "\nFunction with different spacing in coarray specs\n\n",
            "arguments": ["vec", "mat"],
            "in": {
                "vec": {
                    "type": "complex",
                    "description": "Input vector",
                    "dimension": "1:3 &times; 1:2 &times; * (assumed-size)"
                }
            },
            "out": {
                "mat": {
                    "type": "real",
                    "description": "Output matrix",
                    "dimension": "1:2 &times; 1:2 &times; * (assumed-size)"
                }
            },
            "return": {"result": {
                "description": "Distributed status codes",
                "dimension": "* (assumed-size)",
                "type": "integer"
            }},
            "binding_type": "",
            "interface": ""
        }
        self.assertEqual(func_spaced, expected_spaced)


if __name__ == '__main__':
    unittest.main()
