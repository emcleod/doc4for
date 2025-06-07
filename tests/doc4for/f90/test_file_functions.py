import unittest
from pathlib import Path
from pyfakefs.fake_filesystem_unittest import TestCase
from doc4for.f90.generate_file_tree import extract_file_data
from doc4for.models.common import BindingTypeEnum
from doc4for.models.dimension_models import ArrayBound, BoundType
from doc4for.models.common import Expression, ExpressionType
from doc4for.models.variable_models import PolymorphismType

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
            "return": {"description": "Always returns 42", "dimension": None, "type": "INTEGER",
                       "attributes": [], "default_value": None, "kind": None, "length": None, "interface_name": None,
                       "enum_type": None, "polymorphism_type": PolymorphismType.NONE},
            "argument_interfaces": {},
            "binding_type": None
        }
        self.assertEqual(func_doc, expected_doc)

        # Undocumented function
        func_no_doc = file_data["functions"]["simple_no_doc"]
        expected_no_doc = {
            "attributes": [],
            "description": "\n",
            "arguments": [],
            "out": {},
            "in": {},
            "return": {"description": "", "dimension": None, "type": "INTEGER",
                       "attributes": [], "default_value": None, "kind": None, "length": None, "interface_name": None,
                       "enum_type": None, "polymorphism_type": PolymorphismType.NONE},
            "argument_interfaces": {},
            "binding_type": None
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
    ! @in length         The length of the rectangle
    ! @in width    The width of the rectangle
    ! @in scale  The scale, not used
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
                "length": {"description": "The length of the rectangle", "dimension": None, "type": "REAL",
                       "attributes": [], "default_value": None, "kind": None, "length": None, "interface_name": None,
                       "enum_type": None, "polymorphism_type": PolymorphismType.NONE},
                "width": {"description": "The width of the rectangle", "dimension": None, "type": "REAL",
                       "attributes": [], "default_value": None, "kind": None, "length": None, "interface_name": None,
                       "enum_type": None, "polymorphism_type": PolymorphismType.NONE},
                "scale": {"description": "The scale, not used", "dimension": None, "type": "REAL",
                       "attributes": [], "default_value": None, "kind": None, "length": None, "interface_name": None,
                       "enum_type": None, "polymorphism_type": PolymorphismType.NONE},
            },
            "out": {},
            "return": {"description": "Area of the rectangle", "dimension": None, "type": "REAL",
                       "attributes": [], "default_value": None, "kind": None, "length": None, "interface_name": None,
                       "enum_type": None, "polymorphism_type": PolymorphismType.NONE},
            "argument_interfaces": {},
            "binding_type": None
        }
        self.assertEqual(func_doc, expected_doc)

        # Undocumented function
        func_no_doc = file_data["functions"]["rectangle_area_no_doc"]
        expected_no_doc = {
            "attributes": [],
            "description": "",
            "arguments": ["length", "width", "scale"],
            "in": {
                "length": {"description": "", "dimension": None, "type": "REAL",
                       "attributes": [], "default_value": None, "kind": None, "length": None, "interface_name": None,
                       "enum_type": None, "polymorphism_type": PolymorphismType.NONE},
                "width": {"description": "", "dimension": None, "type": "REAL",
                       "attributes": [], "default_value": None, "kind": None, "length": None, "interface_name": None,
                       "enum_type": None, "polymorphism_type": PolymorphismType.NONE},
                "scale": {"description": "", "dimension": None, "type": "REAL",
                       "attributes": [], "default_value": None, "kind": None, "length": None, "interface_name": None,
                       "enum_type": None, "polymorphism_type": PolymorphismType.NONE},
            },
            "out": {},
            "return": {"description": "", "dimension": None, "type": "REAL",
                       "attributes": [], "default_value": None, "kind": None, "length": None, "interface_name": None,
                       "enum_type": None, "polymorphism_type": PolymorphismType.NONE},
            "argument_interfaces": {},
            "binding_type": None
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
    ! @in    arg1  Input argument 1
    ! @out   arg2  Output argument 2
    ! @inout arg3  Argument 3 for both input and output
    ! @in    arg4  Argument 4 with no space after colon
    ! @return      The result of the calculation
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
    ! @return  Always returns 42
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
        self.assertEqual(func_annotations["in"]["arg1"]["description"], "Input argument 1")
        self.assertEqual(func_annotations["in"]["arg3"]["description"], "Argument 3 for both input and output")
        self.assertEqual(func_annotations["out"]["arg2"]["description"], "Output argument 2")
        self.assertEqual(func_annotations["out"]["arg3"]["description"], "Argument 3 for both input and output")
        self.assertEqual(func_annotations["return"]["description"], "The result of the calculation")

        func_unnamed = file_data["functions"]["unnamed_return"]
        self.assertEqual(func_unnamed["return"]["description"], "Always returns 42")

    def test_function_with_array_arguments(self):
        self.fs.create_file(
            "/fake/path/functions.f90",
            contents="""\
            
    program test
    end program test

    !!*
    ! Function with array arguments
    ! @in matrix   The input matrix
    ! @out vector  The output vector
    ! @return    Whether the operation was successful
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
                "matrix": {"description": "The input matrix", 
                       "dimension": {"dimensions": [
                           ArrayBound(BoundType.FIXED, Expression(ExpressionType.LITERAL, "1"), Expression(ExpressionType.LITERAL, "100")),
                           ArrayBound(BoundType.FIXED, Expression(ExpressionType.LITERAL, "1"), Expression(ExpressionType.LITERAL, "100"))]}, 
                       "type": "REAL", "attributes": [], "default_value": None, "kind": None, "length": None, "interface_name": None,
                       "enum_type": None, "polymorphism_type": PolymorphismType.NONE},
            },
            "out": {
                "vector": {"description": "The output vector", 
                       "dimension": {"dimensions": [ArrayBound(BoundType.FIXED, Expression(ExpressionType.LITERAL, "1"), Expression(ExpressionType.LITERAL, "100"))]}, 
                       "type": "REAL", "attributes": [], "default_value": None, "kind": None, "length": None, "interface_name": None,
                       "enum_type": None, "polymorphism_type": PolymorphismType.NONE},
            },
            "return": {"description": "Whether the operation was successful", 
                       "dimension": None, 
                       "type": "LOGICAL", "attributes": [], "default_value": None, "kind": None, "length": None, "interface_name": None,
                       "enum_type": None, "polymorphism_type": PolymorphismType.NONE},
            "argument_interfaces": {},
            "binding_type": None
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
    ! Unnamed return
    ! @return Description for unnamed return
    !*!
    function unnamed_return()
        real :: unnamed_return
    end function unnamed_return

    !!*
    ! Named return 
    ! @return Named return with attached type description
    !*!
    function named_return(x) result(result)
        real, intent(in) :: x
        character :: result
    end function named_return
"""
        )
        result = extract_file_data([Path("/fake/path/returns.f90")])
        file_data = result[0]
        self.assertEqual(len(file_data["functions"]), 2)

        func_attached = file_data["functions"]["unnamed_return"]
        expected_attached = {
            "attributes": [],
            "description": "\nUnnamed return\n\n",
            "arguments": [],
            "in": {},
            "out": {},
            "return": {"description": "Description for unnamed return", "dimension": None, "type": "REAL",
                       "attributes": [], "default_value": None, "kind": None, "length": None, "interface_name": None,
                       "enum_type": None, "polymorphism_type": PolymorphismType.NONE},
            "argument_interfaces": {},
            "binding_type": None
        }
        self.assertEqual(func_attached, expected_attached)


    def test_many_arguments(self):
        self.fs.create_file(
            "/fake/path/arguments.f90",
            contents="""\
            
    program main
    end program main

    !!*
    ! Function to test all argument annotation styles
    ! @in a         Standard input annotation
    ! @in b         No space after variable name
    ! @in c         Space before type
    ! @in d         No spaces around colon
    ! @in e         Array input
    ! @out f        Standard output annotation
    ! @out g        No space after variable name
    ! @out h        Space before type
    ! @out i        No spaces around colon
    ! @out j        Array output
    ! @inout k      Standard inout annotation
    ! @inout l      No space after variable name
    ! @inout m      Space before type
    ! @inout n      No spaces around colon
    ! @inout o      Array inout
    ! @inout p      Array input and output
    ! @inout q      Array inout
    ! @return       Return type
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
        self.assertEqual(len(func_styles["in"]), 12)

        self.assertEqual(func_styles["in"]["a"]["type"], "INTEGER")
        self.assertEqual(func_styles["in"]["a"]["description"], "Standard input annotation")
        self.assertIsNone(func_styles["in"]["a"]["dimension"])
        self.assertEqual(func_styles["in"]["b"]["type"], "INTEGER")
        self.assertEqual(func_styles["in"]["b"]["description"], "No space after variable name")
        self.assertIsNone(func_styles["in"]["b"]["dimension"])
        self.assertEqual(func_styles["in"]["c"]["type"], "INTEGER")
        self.assertEqual(func_styles["in"]["c"]["description"], "Space before type")
        self.assertIsNone(func_styles["in"]["c"]["dimension"])
        self.assertEqual(func_styles["in"]["d"]["type"], "INTEGER")
        self.assertEqual(func_styles["in"]["d"]["description"], "No spaces around colon")
        self.assertIsNone(func_styles["in"]["d"]["dimension"])
        self.assertEqual(func_styles["in"]["e"]["type"], "INTEGER")
        self.assertEqual(func_styles["in"]["e"]["description"], "Array input")
        self.assertEqual(func_styles["in"]["e"]["dimension"], 
                         {"dimensions": [ArrayBound(BoundType.FIXED, 
                                                    Expression(ExpressionType.LITERAL, "1"), 
                                                    Expression(ExpressionType.LITERAL, "10"))]})
        self.assertEqual(func_styles["in"]["k"]["type"], "LOGICAL")
        self.assertEqual(func_styles["in"]["k"]["description"], "Standard inout annotation")
        self.assertIsNone(func_styles["in"]["k"]["dimension"])
        self.assertEqual(func_styles["in"]["l"]["type"], "LOGICAL")
        self.assertEqual(func_styles["in"]["l"]["description"], "No space after variable name")
        self.assertIsNone(func_styles["in"]["l"]["dimension"])
        self.assertEqual(func_styles["in"]["m"]["type"], "LOGICAL")
        self.assertEqual(func_styles["in"]["m"]["description"], "Space before type")
        self.assertIsNone(func_styles["in"]["m"]["dimension"])
        self.assertEqual(func_styles["in"]["n"]["type"], "LOGICAL")
        self.assertEqual(func_styles["in"]["n"]["description"], "No spaces around colon")
        self.assertIsNone(func_styles["in"]["n"]["dimension"])
        self.assertEqual(func_styles["in"]["o"]["type"], "LOGICAL")
        self.assertEqual(func_styles["in"]["o"]["description"], "Array inout")
        self.assertEqual(func_styles["in"]["o"]["dimension"], 
                         {"dimensions": [ArrayBound(BoundType.FIXED, 
                                                    Expression(ExpressionType.LITERAL, "1"), 
                                                    Expression(ExpressionType.LITERAL, "3"))]})
        self.assertEqual(func_styles["in"]["p"]["type"], "INTEGER")
        self.assertEqual(func_styles["in"]["p"]["description"], "Array input and output")
        self.assertEqual(func_styles["in"]["p"]["dimension"], 
                         {"dimensions": [ArrayBound(BoundType.FIXED, 
                                                    Expression(ExpressionType.LITERAL, "1"), 
                                                    Expression(ExpressionType.LITERAL, "4"))]})
        self.assertEqual(func_styles["in"]["q"]["type"], "INTEGER")
        self.assertEqual(func_styles["in"]["q"]["description"], "Array inout")
        self.assertEqual(func_styles["in"]["q"]["dimension"], 
                         {"dimensions": [ArrayBound(BoundType.FIXED, 
                                                    Expression(ExpressionType.LITERAL, "1"), 
                                                    Expression(ExpressionType.LITERAL, "2"))]})

        # Check output arguments
        self.assertEqual(len(func_styles["out"]), 12)

        self.assertEqual(func_styles["out"]["f"]["type"], "REAL")
        self.assertEqual(func_styles["out"]["f"]["description"], "Standard output annotation")
        self.assertIsNone(func_styles["out"]["f"]["dimension"])
        self.assertEqual(func_styles["out"]["g"]["type"], "REAL")
        self.assertEqual(func_styles["out"]["g"]["description"], "No space after variable name")
        self.assertIsNone(func_styles["out"]["g"]["dimension"])
        self.assertEqual(func_styles["out"]["h"]["type"], "REAL")
        self.assertEqual(func_styles["out"]["h"]["description"], "Space before type")
        self.assertIsNone(func_styles["out"]["h"]["dimension"])
        self.assertEqual(func_styles["out"]["i"]["type"], "REAL")
        self.assertEqual(func_styles["out"]["i"]["description"], "No spaces around colon")
        self.assertIsNone(func_styles["out"]["i"]["dimension"])
        self.assertEqual(func_styles["out"]["j"]["type"], "REAL")
        self.assertEqual(func_styles["out"]["j"]["description"], "Array output")
        self.assertEqual(func_styles["out"]["j"]["dimension"], 
                         {"dimensions": [ArrayBound(BoundType.FIXED, 
                                                    Expression(ExpressionType.LITERAL, "1"), 
                                                    Expression(ExpressionType.LITERAL, "5"))]})
        self.assertEqual(func_styles["out"]["k"]["type"], "LOGICAL")
        self.assertEqual(func_styles["out"]["k"]["description"], "Standard inout annotation")
        self.assertIsNone(func_styles["out"]["k"]["dimension"])
        self.assertEqual(func_styles["out"]["l"]["type"], "LOGICAL")
        self.assertEqual(func_styles["out"]["l"]["description"], "No space after variable name")
        self.assertIsNone(func_styles["out"]["l"]["dimension"])
        self.assertEqual(func_styles["out"]["m"]["type"], "LOGICAL")
        self.assertEqual(func_styles["out"]["m"]["description"], "Space before type")
        self.assertIsNone(func_styles["out"]["m"]["dimension"])
        self.assertEqual(func_styles["out"]["n"]["type"], "LOGICAL")
        self.assertEqual(func_styles["out"]["n"]["description"], "No spaces around colon")
        self.assertIsNone(func_styles["out"]["n"]["dimension"])
        self.assertEqual(func_styles["out"]["o"]["type"], "LOGICAL")
        self.assertEqual(func_styles["out"]["o"]["description"], "Array inout")
        self.assertEqual(func_styles["out"]["o"]["dimension"], 
                         {"dimensions": [ArrayBound(BoundType.FIXED, 
                                                    Expression(ExpressionType.LITERAL, "1"), 
                                                    Expression(ExpressionType.LITERAL, "3"))]})
        self.assertEqual(func_styles["out"]["p"]["type"], "INTEGER")
        self.assertEqual(func_styles["out"]["p"]["description"], "Array input and output")
        self.assertEqual(func_styles["out"]["p"]["dimension"], 
                         {"dimensions": [ArrayBound(BoundType.FIXED, 
                                                    Expression(ExpressionType.LITERAL, "1"), 
                                                    Expression(ExpressionType.LITERAL, "4"))]})
        self.assertEqual(func_styles["out"]["q"]["type"], "INTEGER")
        self.assertEqual(func_styles["out"]["q"]["description"], "Array inout")
        self.assertEqual(func_styles["out"]["q"]["dimension"], 
                         {"dimensions": [ArrayBound(BoundType.FIXED, 
                                                    Expression(ExpressionType.LITERAL, "1"), 
                                                    Expression(ExpressionType.LITERAL, "2"))]})

        # Check return
        self.assertEqual(func_styles["return"], {
            "type": "CHARACTER", 
            "description": "Return type", 
            "dimension": None,
            "attributes": [],
            "enum_type": None,
            "interface_name": None,
            "kind": None,
            "length": "1",
            "polymorphism_type": PolymorphismType.NONE,
            "default_value": None
        })

    def test_argument_annotations_with_complex_descriptions(self):
        self.fs.create_file(
            "/fake/path/complex_args.f90",
            contents="""\
            
    program main
    end program main

    !!*
    ! Function with complex argument descriptions
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
        matrix = func_complex["in"]["matrix"]
        self.assertEqual(matrix["type"], "REAL")
        self.assertEqual(matrix["description"], "The input matrix, used for primary calculations")
        self.assertEqual(matrix["dimension"], {"dimensions":
                                               [ArrayBound(BoundType.FIXED,
                                                           Expression(ExpressionType.LITERAL, "1"),
                                                           Expression(ExpressionType.LITERAL, "100")),
                                                ArrayBound(BoundType.FIXED,
                                                           Expression(ExpressionType.LITERAL, "1"),
                                                           Expression(ExpressionType.LITERAL, "100"))
                                                ]})

        expected_stats_desc = ("Output statistics array: "
                               "1: mean "
                               "2: median "
                               "3-5: quartiles "
                               "6-10: reserved for future use")
        stats = func_complex["out"]["stats"]
        self.assertEqual(stats["type"], "REAL")
        self.assertEqual(stats["description"], expected_stats_desc)
        self.assertEqual(stats["dimension"], {"dimensions":
                                               [ArrayBound(BoundType.FIXED,
                                                           Expression(ExpressionType.LITERAL, "1"),
                                                           Expression(ExpressionType.LITERAL, "10"))
                                                ]})

        expected_flag_desc = ("Processing flag: "
                              "0 = not started "
                              "1 = in progress "
                              "2 = completed "
                              "negative = error code")
        flag = func_complex["in"]["flag"]
        self.assertEqual(flag["type"], "INTEGER")
        self.assertEqual(flag["description"], expected_flag_desc)
        self.assertIsNone(flag["dimension"])

        flag = func_complex["out"]["flag"]
        self.assertEqual(flag["type"], "INTEGER")
        self.assertEqual(flag["description"], expected_flag_desc)
        self.assertIsNone(flag["dimension"])

        # Check return description
        expected_return_desc = ("True if processing was successful, "
                                "False otherwise")
        self.assertEqual(func_complex["return"]["type"], "LOGICAL")
        self.assertEqual(func_complex["return"]["description"], expected_return_desc)
        self.assertIsNone(func_complex["return"]["dimension"])

    def test_return_with_arrays(self):
        self.fs.create_file(
            "/fake/path/array_returns.f90",
            contents="""\
        program test_prog
            implicit none
        end program test_prog

        !!*
        ! Function returning an array
        ! @return Returns a vector of results
        !*!
        function vector_return()
            real :: vector_return(10)
        end function vector_return

        !!*
        ! Function with named array return
        ! @return Returns a matrix of counts
        !*!
        function matrix_return() result(result)
            integer :: result(5,5)
        end function matrix_return

        !!*
        ! Function with different spacing in array spec
        ! @return  Returns complex values
        !*!
        function spaced_array() result(res)
            complex :: res(3)
        end function spaced_array

        !!*
        ! Function with no spaces around array spec
        ! @return  Returns many values
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
            "return": {"description": "Returns a vector of results", 
                       "dimension": {"dimensions": [ArrayBound(BoundType.FIXED, Expression(ExpressionType.LITERAL, "1"), Expression(ExpressionType.LITERAL, "10"))]}, 
                       "type": "REAL", "attributes": [], "default_value": None, "kind": None, "length": None, "interface_name": None,
                       "enum_type": None, "polymorphism_type": PolymorphismType.NONE},
            "argument_interfaces": {},
            "binding_type": None
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
            "return": {
                "description": "Returns a matrix of counts",
                "dimension": {"dimensions": [
                    ArrayBound(BoundType.FIXED, Expression(ExpressionType.LITERAL, "1"), Expression(ExpressionType.LITERAL, "5")),
                    ArrayBound(BoundType.FIXED, Expression(ExpressionType.LITERAL, "1"), Expression(ExpressionType.LITERAL, "5"))]}, 
                "type": "INTEGER", "attributes": [], "default_value": None, "kind": None, "length": None, "interface_name": None,
                "enum_type": None, "polymorphism_type": PolymorphismType.NONE},
            "argument_interfaces": {},
            "binding_type": None
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
            "return": {
                "description": "Returns complex values",
                "dimension": {"dimensions": [ArrayBound(BoundType.FIXED, Expression(ExpressionType.LITERAL, "1"), Expression(ExpressionType.LITERAL, "3"))]}, 
                "type": "COMPLEX", "attributes": [], "default_value": None, "kind": None, "length": None, "interface_name": None,
                "enum_type": None, "polymorphism_type": PolymorphismType.NONE},
            "argument_interfaces": {},
            "binding_type": None
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
            "return": {
                "description": "Returns many values",
                "dimension": {"dimensions": [ArrayBound(BoundType.FIXED, Expression(ExpressionType.LITERAL, "1"), Expression(ExpressionType.LITERAL, "100"))]}, 
                "type": "REAL", "attributes": [], "default_value": None, "kind": None, "length": None, "interface_name": None,
                "enum_type": None, "polymorphism_type": PolymorphismType.NONE},            
            "argument_interfaces": {},
            "binding_type": None
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
        ! @return      Returns a coarray distributed across images
        !*!
        function coarray_return()
            real :: coarray_return[*]
        end function coarray_return

        !!*
        ! Function with array and coarray specs
        ! @return   Returns a distributed array of counts
        !*!
        function array_coarray() result(result)
            integer :: result(10)[2,*]
        end function array_coarray

        !!*
        ! Function with coarray arguments
        ! @in  data         Input coarray
        ! @out res          Output array coarray
        ! @return           Success status
        !*!
        function process_coarrays(data, res)
            real, intent(in) :: data[*]
            real, intent(out) :: res(3)[3,*]
            logical :: process_coarrays
        end function process_coarrays

        !!*
        ! Function with different spacing in coarray specs
        ! @in  vec  Input vector
        ! @out mat  Output matrix
        ! @return   Distributed status codes
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
            "argument_interfaces": {},
            "binding_type": { "type": BindingTypeEnum.DEFAULT, "name": None}
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
            "argument_interfaces": {},
            "binding_type": { "type": BindingTypeEnum.DEFAULT, "name": None}
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
            "argument_interfaces": {},
            "binding_type": { "type": BindingTypeEnum.DEFAULT, "name": None}
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
            "argument_interfaces": {},
            "binding_type": { "type": BindingTypeEnum.DEFAULT, "name": None}
        }
        self.assertEqual(func_spaced, expected_spaced)


if __name__ == "__main__":
    unittest.main()
