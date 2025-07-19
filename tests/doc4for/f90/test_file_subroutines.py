import unittest
from pathlib import Path
from pyfakefs.fake_filesystem_unittest import TestCase
from doc4for.f90.generate_file_tree import extract_file_data
from doc4for.models.dimension_models import ArrayBound, BoundType
from doc4for.models.common import Expression, ExpressionType
from doc4for.models.variable_models import PolymorphismType

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
            "description": "Does nothing.\n",
            "arguments": [],
            "out": {},
            "in": {},
            "argument_interfaces": {},
            "binding_type": None,
            "uses": {}
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
            "binding_type": None,
            "uses": {}
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
            "description": "Calculates the area of a rectangle\n\n",
            "arguments": ["length", "width", "scale", "area"],
            "in": {
                "length": {"type": "REAL", 
                           "description": "The length of the rectangle",
                           "dimension": None,
                           "attributes": [],
                           "default_value": None,
                           "enum_type": None,
                           "interface_name": None,
                           "kind": None,
                           "length": None,
                           "polymorphism_type": PolymorphismType.NONE,
                           "type_params": None},
                "width": {"type": "REAL", 
                           "description": "The width of the rectangle",
                           "dimension": None,
                           "attributes": [],
                           "default_value": None,
                           "enum_type": None,
                           "interface_name": None,
                           "kind": None,
                           "length": None,
                           "polymorphism_type": PolymorphismType.NONE,
                           "type_params": None},
                "scale": {"type": "REAL", 
                           "description": "The scale, not used",
                           "dimension": None,
                           "attributes": [],
                           "default_value": None,
                           "enum_type": None,
                           "interface_name": None,
                           "kind": None,
                           "length": None,
                           "polymorphism_type": PolymorphismType.NONE,
                           "type_params": None}
            },
            "out": {
                "area": {"type": "REAL", 
                           "description": "The area of the rectangle",
                           "dimension": None,
                           "attributes": [],
                           "default_value": None,
                           "enum_type": None,
                           "interface_name": None,
                           "kind": None,
                           "length": None,
                           "polymorphism_type": PolymorphismType.NONE,
                           "type_params": None}
            },
            "argument_interfaces": {},
            "binding_type": None,
            "uses": {}
        }
        self.assertEqual(sub_doc, expected_doc)

        # Undocumented function
        sub_no_doc = file_data["subroutines"]["rectangle_area_no_doc"]
        expected_no_doc = {
            "attributes": [],
            "description": "",
            "arguments": ["length", "width", "area"],
            "in": {
                "length": {"type": "REAL", 
                           "description": "",
                           "dimension": None,
                           "attributes": [],
                           "default_value": None,
                           "enum_type": None,
                           "interface_name": None,
                           "kind": None,
                           "length": None,
                           "polymorphism_type": PolymorphismType.NONE,
                           "type_params": None},
                "width": {"type": "REAL", 
                           "description": "",
                           "dimension": None,
                           "attributes": [],
                           "default_value": None,
                           "enum_type": None,
                           "interface_name": None,
                           "kind": None,
                           "length": None,
                           "polymorphism_type": PolymorphismType.NONE,
                           "type_params": None},
            },
            "out": {
                "area": {"type": "REAL", 
                           "description": "",
                           "dimension": None,
                           "attributes": [],
                           "default_value": None,
                           "enum_type": None,
                           "interface_name": None,
                           "kind": None,
                           "length": None,
                           "polymorphism_type": PolymorphismType.NONE,
                           "type_params": None},
            },
            "argument_interfaces": {},
            "binding_type": None,
            "uses": {}
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
    ! @in arg4       
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
        self.assertEqual(sub_annotations["description"], "Test subroutine with various annotation styles\n\n")
        self.assertEqual(sub_annotations["arguments"], ["arg1", "arg2", "arg3", "arg4"])
        self.assertEqual(sub_annotations["in"]["arg1"]["description"], "Input argument 1")
        self.assertEqual(sub_annotations["in"]["arg3"]["description"], "Argument 3 for both input and output")
        self.assertEqual(sub_annotations["in"]["arg4"]["description"], "")
        self.assertEqual(sub_annotations["out"]["arg2"]["description"], "Output argument 2")
        self.assertEqual(sub_annotations["out"]["arg3"]["description"], "Argument 3 for both input and output")

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
            "description": "Subroutine with array arguments\n\n",
            "arguments": ["matrix", "vector"],
            "in": {
                "matrix": {"type": "REAL", 
                           "description": "The input matrix",
                           "dimension": {"dimensions": [
                               ArrayBound(BoundType.FIXED, Expression(ExpressionType.LITERAL, "1"), Expression(ExpressionType.LITERAL, "100")),
                               ArrayBound(BoundType.FIXED, Expression(ExpressionType.LITERAL, "1"), Expression(ExpressionType.LITERAL, "100")),
                           ]},
                           "attributes": [],
                           "default_value": None,
                           "enum_type": None,
                           "interface_name": None,
                           "kind": None,
                           "length": None,
                           "polymorphism_type": PolymorphismType.NONE,
                           "type_params": None}
            },
            "out": {
                "vector": {"type": "REAL", 
                           "description": "The output vector",
                           "dimension": {"dimensions": [
                               ArrayBound(BoundType.FIXED, Expression(ExpressionType.LITERAL, "1"), Expression(ExpressionType.LITERAL, "100")),
                           ]},
                           "attributes": [],
                           "default_value": None,
                           "enum_type": None,
                           "interface_name": None,
                           "kind": None,
                           "length": None,
                           "polymorphism_type": PolymorphismType.NONE,
                           "type_params": None}
            },
            "argument_interfaces": {},
            "binding_type": None,
            "uses": {}
        }
        self.assertEqual(sub_array, expected_array)
        
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
        self.assertEqual(sub_complex["in"]["matrix"]["type"], "REAL")
        self.assertEqual(sub_complex["in"]["matrix"]["description"], "The input matrix, used for primary calculations")
        self.assertEqual(sub_complex["in"]["matrix"]["dimension"], { "dimensions":
            [ArrayBound(BoundType.FIXED, Expression(ExpressionType.LITERAL, "1"), Expression(ExpressionType.LITERAL, "100")),
            ArrayBound(BoundType.FIXED, Expression(ExpressionType.LITERAL, "1"), Expression(ExpressionType.LITERAL, "100"))
        ]})
        
        expected_stats_desc = ("Output statistics array: "
                            "1: mean "
                            "2: median "
                            "3-5: quartiles "
                            "6-10: reserved for future use")
        self.assertEqual(sub_complex["out"]["stats"]["type"], "REAL")
        self.assertEqual(sub_complex["out"]["stats"]["description"], expected_stats_desc)
        self.assertEqual(sub_complex["out"]["stats"]["dimension"], { "dimensions":
            [ArrayBound(BoundType.FIXED, Expression(ExpressionType.LITERAL, "1"), Expression(ExpressionType.LITERAL, "10")),
        ]})
        
        expected_flag_desc = ("Processing flag: "
                            "0 = not started "
                            "1 = in progress "
                            "2 = completed "
                            "negative = error code")
        self.assertEqual(sub_complex["in"]["flag"]["type"], "INTEGER")
        self.assertEqual(sub_complex["in"]["flag"]["description"], expected_flag_desc)
        self.assertIsNone(sub_complex["in"]["flag"]["dimension"])

        self.assertEqual(sub_complex["out"]["flag"]["type"], "INTEGER")
        self.assertEqual(sub_complex["out"]["flag"]["description"], expected_flag_desc)
        self.assertIsNone(sub_complex["out"]["flag"]["dimension"])
        
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
        self.assertEqual(sub_complex["in"]["matrix"]["type"], "REAL")
        self.assertEqual(sub_complex["in"]["matrix"]["description"], "The input matrix, used for primary calculations")
        self.assertEqual(sub_complex["in"]["matrix"]["dimension"]["dimensions"], [
            ArrayBound(BoundType.FIXED, Expression(ExpressionType.LITERAL, "1"), Expression(ExpressionType.LITERAL, "100")),
            ArrayBound(BoundType.FIXED, Expression(ExpressionType.LITERAL, "1"), Expression(ExpressionType.LITERAL, "100")),
        ])
        
        expected_stats_desc = ("Output statistics array: "
                            "1: mean "
                            "2: median "
                            "3-5: quartiles "
                            "6-10: reserved for future use")
        self.assertEqual(sub_complex["out"]["stats"]["type"], "REAL")
        self.assertEqual(sub_complex["out"]["stats"]["description"], expected_stats_desc)
        self.assertEqual(sub_complex["out"]["stats"]["dimension"]["dimensions"],
                          [ArrayBound(BoundType.FIXED, Expression(ExpressionType.LITERAL, "1"), Expression(ExpressionType.LITERAL, "10"))])
        
        expected_flag_desc = ("Processing flag: "
                            "0 = not started "
                            "1 = in progress "
                            "2 = completed "
                            "negative = error code")
        self.assertEqual(sub_complex["in"]["flag"]["type"], "INTEGER")
        self.assertEqual(sub_complex["in"]["flag"]["description"], expected_flag_desc)
        self.assertIsNone(sub_complex["in"]["flag"]["dimension"])
        
        self.assertEqual(sub_complex["out"]["flag"]["type"], "INTEGER")
        self.assertEqual(sub_complex["out"]["flag"]["description"], expected_flag_desc)
        self.assertIsNone(sub_complex["out"]["flag"]["dimension"])
        

if __name__ == "__main__":
    unittest.main()
