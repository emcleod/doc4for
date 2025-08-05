import unittest
from pathlib import Path
from pyfakefs.fake_filesystem_unittest import TestCase
from doc4for.f90.generate_module_tree import extract_module_data
from doc4for.models.dimension_models import ArrayBound, BoundType
from doc4for.models.common import Expression, ExpressionType

class TestDataComponents(TestCase):
    maxDiff=None

    def setUp(self):
        self.setUpPyfakefs()

    def test_public_data_components_no_comments(self):
        self.fs.create_file(
            "/fake/path/types.f90",
            contents="""\
 module simple_module
   implicit none
   private

   public :: simple_type

   type :: simple_type
     real, public :: x
     integer, public :: y
   contains
     procedure, public :: init
     procedure, public :: get_x
     procedure, public :: get_y
   end type simple_type
end module simple_module
 """,
        )
        result = extract_module_data([Path("/fake/path/types.f90")])
        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(module["module_name"], "simple_module")
        types = module["types"]
        self.assertEqual(len(types), 1)
        type = types["simple_type"]
        self.assertEqual(type["type_name"], "simple_type")
        self.assertEqual(type["attributes"], ["PUBLIC"])
        procedures = type["procedures"]
        self.assertEqual(procedures["init"]["name"], "init")
        self.assertEqual(procedures["init"]["description"], "")
        data_components = type["data_components"]
        self.assertEqual(len(data_components), 2)
        self.assertEqual(data_components["x"]["name"], "x")
        self.assertEqual(data_components["x"]["type"], "REAL")
        self.assertEqual(data_components["x"]["description"], "")
        self.assertEqual(data_components["x"]["dimension"], None)
        self.assertEqual(data_components["x"]["initial_value"], None)
        self.assertEqual(data_components["x"]["attributes"], ["PUBLIC"])
        self.assertEqual(data_components["y"]["name"], "y")
        self.assertEqual(data_components["y"]["type"], "INTEGER")
        self.assertEqual(data_components["y"]["description"], "")
        self.assertEqual(data_components["y"]["dimension"], None)
        self.assertEqual(data_components["y"]["initial_value"], None)
        self.assertEqual(data_components["y"]["attributes"], ["PUBLIC"])
        self.assertEqual(type["description"], "")
        self.assertIsNone(type["extends"])
        self.assertEqual(type["generic_interfaces"], {})

    def test_public_data_components_with_comments(self):
        self.fs.create_file(
            "/fake/path/types.f90",
            contents="""\
 module simple_module
   implicit none
   private

   public :: simple_type

   type :: simple_type
     !!* declares x *!
     real, public :: x
     !!* declares y *!
     integer, public :: y
   contains
     procedure, public :: init
     procedure, public :: get_x
     procedure, public :: get_y
   end type simple_type
end module simple_module
 """,
        )
        result = extract_module_data([Path("/fake/path/types.f90")])
        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(module["module_name"], "simple_module")
        types = module["types"]
        self.assertEqual(len(types), 1)
        type = types["simple_type"]
        self.assertEqual(type["type_name"], "simple_type")
        self.assertEqual(type["attributes"], ["PUBLIC"])
        procedures = type["procedures"]
        self.assertEqual(procedures["init"]["name"], "init")
        self.assertEqual(procedures["init"]["description"], "")
        data_components = type["data_components"]
        self.assertEqual(len(data_components), 2)
        self.assertEqual(data_components["x"]["name"], "x")
        self.assertEqual(data_components["x"]["type"], "REAL")
        self.assertEqual(data_components["x"]["description"], "declares x\n")
        self.assertEqual(data_components["x"]["dimension"], None)
        self.assertEqual(data_components["x"]["initial_value"], None)
        self.assertEqual(data_components["x"]["attributes"], ["PUBLIC"])
        self.assertEqual(data_components["y"]["name"], "y")
        self.assertEqual(data_components["y"]["type"], "INTEGER")
        self.assertEqual(data_components["y"]["description"], "declares y\n")
        self.assertEqual(data_components["y"]["dimension"], None)
        self.assertEqual(data_components["y"]["initial_value"], None)
        self.assertEqual(data_components["y"]["attributes"], ["PUBLIC"])
        self.assertEqual(type["description"], "")
        self.assertIsNone(type["extends"])
        self.assertEqual(type["generic_interfaces"], {})

    def test_data_components_comment_edge_cases(self):
        self.fs.create_file(
            "/fake/path/types.f90",
            contents="""\
    module edge_case_module
      implicit none
      private

      public :: edge_case_type

      type :: edge_case_type
        !!* This is a multi-line comment
        ! that describes the x component
        ! across several lines
        !*!
        real, public :: x
        
        ! This is just a regular comment, not doc4for
        !!* This should be for y *!
        integer, public :: y = 5
        
        !!* This comment is for both a and b *!
        real, public :: a, b
        
        integer, public :: z = 10  ! This trailing comment should not be captured
        !!* This is for w *!
        real, public :: w
        
        !!* Orphaned comment with no component after it *!
        ! Another regular comment
        
      contains
        procedure, public :: init
      end type edge_case_type
    end module edge_case_module
    """,
        )
        result = extract_module_data([Path("/fake/path/types.f90")])
        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(module["module_name"], "edge_case_module")
        types = module["types"]
        self.assertEqual(len(types), 1)
        type = types["edge_case_type"]
        
        data_components = type["data_components"]
        
        # Multi-line comment case
        self.assertEqual(data_components["x"]["description"], 
                        "This is a multi-line comment\nthat describes the x component\nacross several lines\n")
        
        # Regular comment should be ignored, only doc4for comment captured
        self.assertEqual(data_components["y"]["description"], "This should be for y\n")
        
        # Multiple components on one line - both should get the same description
        self.assertEqual(data_components["a"]["description"], "This comment is for both a and b\n")
        self.assertEqual(data_components["b"]["description"], "This comment is for both a and b\n")
        
        # Trailing comment should not be captured
        self.assertEqual(data_components["z"]["description"], "")
        
        # Component after regular comments should still get its doc4for comment
        self.assertEqual(data_components["w"]["description"], "This is for w\n")

    def test_initialised_public_data_components_no_comments(self):
        self.fs.create_file(
            "/fake/path/types.f90",
            contents="""\
 module simple_module
   implicit none
   private

   public :: simple_type

   type :: simple_type
     real, public :: x = 10.0
     integer, public :: y
   contains
     procedure, public :: init
     procedure, public :: get_x
     procedure, public :: get_y
   end type simple_type
end module simple_module
 """,
        )
        result = extract_module_data([Path("/fake/path/types.f90")])
        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(module["module_name"], "simple_module")
        types = module["types"]
        self.assertEqual(len(types), 1)
        type = types["simple_type"]
        self.assertEqual(type["type_name"], "simple_type")
        self.assertEqual(type["attributes"], ["PUBLIC"])
        procedures = type["procedures"]
        self.assertEqual(procedures["init"]["name"], "init")
        self.assertEqual(procedures["init"]["description"], "")
        data_components = type["data_components"]
        self.assertEqual(len(data_components), 2)
        self.assertEqual(data_components["x"]["name"], "x")
        self.assertEqual(data_components["x"]["type"], "REAL")
        self.assertEqual(data_components["x"]["description"], "")
        self.assertEqual(data_components["x"]["dimension"], None)
        self.assertEqual(data_components["x"]["initial_value"], "10.0")
        self.assertEqual(data_components["x"]["attributes"], ["PUBLIC"])
        self.assertEqual(data_components["y"]["name"], "y")
        self.assertEqual(data_components["y"]["type"], "INTEGER")
        self.assertEqual(data_components["y"]["description"], "")
        self.assertEqual(data_components["y"]["dimension"], None)
        self.assertEqual(data_components["y"]["initial_value"], None)
        self.assertEqual(data_components["y"]["attributes"], ["PUBLIC"])
        self.assertEqual(type["description"], "")
        self.assertIsNone(type["extends"])
        self.assertEqual(type["generic_interfaces"], {})

    def test_initialised_public_data_components_with_comments(self):
        self.fs.create_file(
            "/fake/path/types.f90",
            contents="""\
 module simple_module
   implicit none
   private

   public :: simple_type

   type :: simple_type
     !!* x is 10 *!
     real, public :: x = 10.0
     !!* y is not initialised *!
     integer, public :: y
   contains
     procedure, public :: init
     procedure, public :: get_x
     procedure, public :: get_y
   end type simple_type
end module simple_module
 """,
        )
        result = extract_module_data([Path("/fake/path/types.f90")])
        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(module["module_name"], "simple_module")
        types = module["types"]
        self.assertEqual(len(types), 1)
        type = types["simple_type"]
        self.assertEqual(type["type_name"], "simple_type")
        self.assertEqual(type["attributes"], ["PUBLIC"])
        procedures = type["procedures"]
        self.assertEqual(procedures["init"]["name"], "init")
        self.assertEqual(procedures["init"]["description"], "")
        data_components = type["data_components"]
        self.assertEqual(len(data_components), 2)
        self.assertEqual(data_components["x"]["name"], "x")
        self.assertEqual(data_components["x"]["type"], "REAL")
        self.assertEqual(data_components["x"]["description"], "x is 10\n")
        self.assertEqual(data_components["x"]["dimension"], None)
        self.assertEqual(data_components["x"]["initial_value"], "10.0")
        self.assertEqual(data_components["x"]["attributes"], ["PUBLIC"])
        self.assertEqual(data_components["y"]["name"], "y")
        self.assertEqual(data_components["y"]["type"], "INTEGER")
        self.assertEqual(data_components["y"]["description"], "y is not initialised\n")
        self.assertEqual(data_components["y"]["dimension"], None)
        self.assertEqual(data_components["y"]["initial_value"], None)
        self.assertEqual(data_components["y"]["attributes"], ["PUBLIC"])
        self.assertEqual(type["description"], "")
        self.assertIsNone(type["extends"])
        self.assertEqual(type["generic_interfaces"], {})

    def test_public_and_private_data_components_no_comments(self):
        self.fs.create_file(
            "/fake/path/types.f90",
            contents="""\
 module simple_module
   implicit none
   private

   public :: simple_type

   type :: simple_type
     real, public :: x
     integer, private :: y
   contains
     procedure, public :: init
     procedure, public :: get_x
     procedure, public :: get_y
   end type simple_type
end module simple_module
 """,
        )
        result = extract_module_data([Path("/fake/path/types.f90")])
        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(module["module_name"], "simple_module")
        types = module["types"]
        self.assertEqual(len(types), 1)
        type = types["simple_type"]
        self.assertEqual(type["type_name"], "simple_type")
        self.assertEqual(type["attributes"], ["PUBLIC"])
        procedures = type["procedures"]
        self.assertEqual(procedures["init"]["name"], "init")
        self.assertEqual(procedures["init"]["description"], "")
        data_components = type["data_components"]
        self.assertEqual(len(data_components), 2)
        self.assertEqual(len(data_components), 2)
        self.assertEqual(data_components["x"]["name"], "x")
        self.assertEqual(data_components["x"]["type"], "REAL")
        self.assertEqual(data_components["x"]["description"], "")
        self.assertEqual(data_components["x"]["dimension"], None)
        self.assertEqual(data_components["x"]["initial_value"], None)
        self.assertEqual(data_components["x"]["attributes"], ["PUBLIC"])
        self.assertEqual(data_components["y"]["name"], "y")
        self.assertEqual(data_components["y"]["type"], "INTEGER")
        self.assertEqual(data_components["y"]["description"], "")
        self.assertEqual(data_components["y"]["dimension"], None)
        self.assertEqual(data_components["y"]["initial_value"], None)
        self.assertEqual(data_components["y"]["attributes"], ["PRIVATE"])
        self.assertEqual(type["description"], "")
        self.assertIsNone(type["extends"])
        self.assertEqual(type["generic_interfaces"], {})

    def test_public_and_private_data_components_with_comments(self):
        self.fs.create_file(
            "/fake/path/types.f90",
            contents="""\
 module simple_module
   implicit none
   private

   public :: simple_type

   type :: simple_type
     !!* declares x *!
     real, public :: x
     !!* declares y *!
     integer, private :: y
   contains
     procedure, public :: init
     procedure, public :: get_x
     procedure, public :: get_y
   end type simple_type
end module simple_module
 """,
        )
        result = extract_module_data([Path("/fake/path/types.f90")])
        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(module["module_name"], "simple_module")
        types = module["types"]
        self.assertEqual(len(types), 1)
        type = types["simple_type"]
        self.assertEqual(type["type_name"], "simple_type")
        self.assertEqual(type["attributes"], ["PUBLIC"])
        procedures = type["procedures"]
        self.assertEqual(procedures["init"]["name"], "init")
        self.assertEqual(procedures["init"]["description"], "")
        data_components = type["data_components"]
        self.assertEqual(len(data_components), 2)
        self.assertEqual(len(data_components), 2)
        self.assertEqual(data_components["x"]["name"], "x")
        self.assertEqual(data_components["x"]["type"], "REAL")
        self.assertEqual(data_components["x"]["description"], "declares x\n")
        self.assertEqual(data_components["x"]["dimension"], None)
        self.assertEqual(data_components["x"]["initial_value"], None)
        self.assertEqual(data_components["x"]["attributes"], ["PUBLIC"])
        self.assertEqual(data_components["y"]["name"], "y")
        self.assertEqual(data_components["y"]["type"], "INTEGER")
        self.assertEqual(data_components["y"]["description"], "declares y\n")
        self.assertEqual(data_components["y"]["dimension"], None)
        self.assertEqual(data_components["y"]["initial_value"], None)
        self.assertEqual(data_components["y"]["attributes"], ["PRIVATE"])
        self.assertEqual(type["description"], "")
        self.assertIsNone(type["extends"])
        self.assertEqual(type["generic_interfaces"], {})

    def test_array_data_components_no_comments(self):
        self.fs.create_file(
            "/fake/path/types.f90",
            contents="""\
module array_module
implicit none
private

public :: array_type

type :: array_type
    real, dimension(3,3), public :: matrix
    integer, dimension(:), allocatable, public :: vector
    complex, dimension(2,2,2), public :: tensor
contains
    procedure, public :: init
end type array_type
end module array_module
    """,
        )
        result = extract_module_data([Path("/fake/path/types.f90")])
        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(module["module_name"], "array_module")
        types = module["types"]
        self.assertEqual(len(types), 1)
        type = types["array_type"]
        self.assertEqual(type["type_name"], "array_type")
        self.assertEqual(type["attributes"], ["PUBLIC"])        
        data_components = type["data_components"]
        self.assertEqual(len(data_components), 3)
        self.assertEqual(data_components["matrix"]["name"], "matrix")
        self.assertEqual(data_components["matrix"]["type"], "REAL")
        self.assertEqual(data_components["matrix"]["dimension"], 
                        { "dimensions": [ArrayBound(BoundType.FIXED, Expression(ExpressionType.LITERAL, "1"), Expression(ExpressionType.LITERAL, "3")),
                                         ArrayBound(BoundType.FIXED, Expression(ExpressionType.LITERAL, "1"), Expression(ExpressionType.LITERAL, "3"))]})
        self.assertEqual(data_components["matrix"]["attributes"], ["PUBLIC"])
        self.assertEqual(data_components["matrix"]["description"], "")
        self.assertEqual(data_components["vector"]["name"], "vector")
        self.assertEqual(data_components["vector"]["type"], "INTEGER")
        self.assertEqual(data_components["vector"]["dimension"], 
                         { "dimensions": [ArrayBound(BoundType.DEFERRED)] })
        self.assertEqual(data_components["vector"]["attributes"], ["ALLOCATABLE", "PUBLIC"])
        self.assertEqual(data_components["vector"]["description"], "")
        self.assertEqual(data_components["tensor"]["name"], "tensor")
        self.assertEqual(data_components["tensor"]["type"], "COMPLEX")
        self.assertEqual(data_components["tensor"]["dimension"], 
                         { "dimensions": [ArrayBound(BoundType.FIXED, Expression(ExpressionType.LITERAL, "1"), Expression(ExpressionType.LITERAL, "2")),
                                          ArrayBound(BoundType.FIXED, Expression(ExpressionType.LITERAL, "1"), Expression(ExpressionType.LITERAL, "2")),
                                          ArrayBound(BoundType.FIXED, Expression(ExpressionType.LITERAL, "1"), Expression(ExpressionType.LITERAL, "2"))]})
        self.assertEqual(data_components["tensor"]["attributes"], ["PUBLIC"])
        self.assertEqual(data_components["tensor"]["description"], "")
        procedures = type["procedures"]
        self.assertEqual(len(procedures), 1)
        self.assertEqual(procedures["init"]["name"], "init")

    def test_array_data_components_with_comments(self):
        self.fs.create_file(
            "/fake/path/types.f90",
            contents="""\
module array_module
implicit none
private

public :: array_type

type :: array_type
!!* a matrix *!
    real, dimension(3,3), public :: matrix
!!*
! a vector 
!*!
    integer, dimension(:), allocatable, public :: vector
!!* a tensor
!*!
    complex, dimension(2,2,2), public :: tensor
contains
    procedure, public :: init
end type array_type
end module array_module
    """,
        )
        result = extract_module_data([Path("/fake/path/types.f90")])
        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(module["module_name"], "array_module")
        types = module["types"]
        self.assertEqual(len(types), 1)
        type = types["array_type"]
        self.assertEqual(type["type_name"], "array_type")
        self.assertEqual(type["attributes"], ["PUBLIC"])        
        data_components = type["data_components"]
        self.assertEqual(len(data_components), 3)
        self.assertEqual(data_components["matrix"]["name"], "matrix")
        self.assertEqual(data_components["matrix"]["type"], "REAL")
        self.assertEqual(data_components["matrix"]["dimension"], 
                        { "dimensions": [ArrayBound(BoundType.FIXED, Expression(ExpressionType.LITERAL, "1"), Expression(ExpressionType.LITERAL, "3")),
                                         ArrayBound(BoundType.FIXED, Expression(ExpressionType.LITERAL, "1"), Expression(ExpressionType.LITERAL, "3"))]})
        self.assertEqual(data_components["matrix"]["attributes"], ["PUBLIC"])
        self.assertEqual(data_components["matrix"]["description"], "a matrix\n")
        self.assertEqual(data_components["vector"]["name"], "vector")
        self.assertEqual(data_components["vector"]["type"], "INTEGER")
        self.assertEqual(data_components["vector"]["dimension"], 
                         { "dimensions": [ArrayBound(BoundType.DEFERRED)] })
        self.assertEqual(data_components["vector"]["attributes"], ["ALLOCATABLE", "PUBLIC"])
        self.assertEqual(data_components["vector"]["description"], "a vector\n")
        self.assertEqual(data_components["tensor"]["name"], "tensor")
        self.assertEqual(data_components["tensor"]["type"], "COMPLEX")
        self.assertEqual(data_components["tensor"]["dimension"], 
                         { "dimensions": [ArrayBound(BoundType.FIXED, Expression(ExpressionType.LITERAL, "1"), Expression(ExpressionType.LITERAL, "2")),
                                          ArrayBound(BoundType.FIXED, Expression(ExpressionType.LITERAL, "1"), Expression(ExpressionType.LITERAL, "2")),
                                          ArrayBound(BoundType.FIXED, Expression(ExpressionType.LITERAL, "1"), Expression(ExpressionType.LITERAL, "2"))]})
        self.assertEqual(data_components["tensor"]["attributes"], ["PUBLIC"])
        self.assertEqual(data_components["tensor"]["description"], "a tensor\n")
        procedures = type["procedures"]
        self.assertEqual(len(procedures), 1)
        self.assertEqual(procedures["init"]["name"], "init")

    def test_character_data_components_no_comments(self):
        self.fs.create_file(
            "/fake/path/types.f90",
            contents="""\
    module character_module
    implicit none
    private

    public :: character_type

    type :: character_type
        character(len=20), public :: fixed_length_string
        character(len=:), allocatable, public :: variable_length_string
        character(kind=selected_char_kind("ISO_10646")), public :: unicode_char
        character(len=10, kind=selected_char_kind("ASCII")), public :: ascii_string
    contains
        procedure, public :: init
    end type character_type
    end module character_module
    """,
        )
        result = extract_module_data([Path("/fake/path/types.f90")])
        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(module["module_name"], "character_module")
        types = module["types"]
        self.assertEqual(len(types), 1)
        type = types["character_type"]
        self.assertEqual(type["type_name"], "character_type")
        self.assertEqual(type["attributes"], ["PUBLIC"])
        data_components = type["data_components"]
        self.assertEqual(len(data_components), 4)
        self.assertEqual(data_components["fixed_length_string"]["name"], "fixed_length_string")
        self.assertEqual(data_components["fixed_length_string"]["type"], "CHARACTER")
        self.assertEqual(data_components["fixed_length_string"]["len"], "20")
        self.assertEqual(data_components["fixed_length_string"]["attributes"], ["PUBLIC"])
        self.assertEqual(data_components["variable_length_string"]["name"], "variable_length_string")
        self.assertEqual(data_components["variable_length_string"]["type"], "CHARACTER")
        self.assertEqual(data_components["variable_length_string"]["attributes"], ["ALLOCATABLE", "PUBLIC"])
        self.assertEqual(data_components["variable_length_string"]["len"], ":")
        self.assertEqual(data_components["unicode_char"]["name"], "unicode_char")
        self.assertEqual(data_components["unicode_char"]["type"], "CHARACTER")
        self.assertEqual(data_components["unicode_char"]["attributes"], ["PUBLIC"])
        self.assertEqual(data_components["unicode_char"]["len"], "1") # by default characters have length 1 if not specified
        self.assertEqual(data_components["ascii_string"]["name"], "ascii_string")
        self.assertEqual(data_components["ascii_string"]["type"], "CHARACTER")
        self.assertEqual(data_components["ascii_string"]["attributes"], ["PUBLIC"])
        self.assertEqual(data_components["ascii_string"]["len"], "10")
        procedures = type["procedures"]
        self.assertEqual(len(procedures), 1)
        self.assertEqual(procedures["init"]["name"], "init")

    def test_character_data_components_with_comments(self):
        self.fs.create_file(
            "/fake/path/types.f90",
            contents="""\
    module character_module
    implicit none
    private

    public :: character_type

    type :: character_type
        !!* The first one *!
        character(len=20), public :: fixed_length_string
        !!* The second one *!
        character(len=:), allocatable, public :: variable_length_string
        !!* The third one *!
        character(kind=selected_char_kind("ISO_10646")), public :: unicode_char
        !!* The fourth one *!
        character(len=10, kind=selected_char_kind("ASCII")), public :: ascii_string
        !!* The fifth one *!
        character(kind=selected_char_kind("ASCII"), len=30), public :: other_ascii_string
    contains
        procedure, public :: init
    end type character_type
    end module character_module
    """,
        )
        result = extract_module_data([Path("/fake/path/types.f90")])
        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(module["module_name"], "character_module")
        types = module["types"]
        self.assertEqual(len(types), 1)
        type = types["character_type"]
        self.assertEqual(type["type_name"], "character_type")
        self.assertEqual(type["attributes"], ["PUBLIC"])
        data_components = type["data_components"]
        self.assertEqual(len(data_components), 5)
        self.assertEqual(data_components["fixed_length_string"]["name"], "fixed_length_string")
        self.assertEqual(data_components["fixed_length_string"]["type"], "CHARACTER")
        self.assertEqual(data_components["fixed_length_string"]["len"], "20")
        self.assertEqual(data_components["fixed_length_string"]["attributes"], ["PUBLIC"])
        self.assertEqual(data_components["fixed_length_string"]["description"], "The first one\n")
        self.assertEqual(data_components["variable_length_string"]["name"], "variable_length_string")
        self.assertEqual(data_components["variable_length_string"]["type"], "CHARACTER")
        self.assertEqual(data_components["variable_length_string"]["attributes"], ["ALLOCATABLE", "PUBLIC"])
        self.assertEqual(data_components["variable_length_string"]["description"], "The second one\n")
        self.assertEqual(data_components["unicode_char"]["name"], "unicode_char")
        self.assertEqual(data_components["unicode_char"]["type"], "CHARACTER")
        self.assertEqual(data_components["unicode_char"]["attributes"], ["PUBLIC"])
        self.assertEqual(data_components["unicode_char"]["description"], "The third one\n")        
        self.assertEqual(data_components["unicode_char"]["len"], "1")
        self.assertEqual(data_components["ascii_string"]["name"], "ascii_string")
        self.assertEqual(data_components["ascii_string"]["type"], "CHARACTER")
        self.assertEqual(data_components["ascii_string"]["attributes"], ["PUBLIC"])
        self.assertEqual(data_components["ascii_string"]["description"], "The fourth one\n")
        self.assertEqual(data_components["ascii_string"]["len"], "10")
        self.assertEqual(data_components["other_ascii_string"]["name"], "other_ascii_string")
        self.assertEqual(data_components["other_ascii_string"]["type"], "CHARACTER")
        self.assertEqual(data_components["other_ascii_string"]["attributes"], ["PUBLIC"])
        self.assertEqual(data_components["other_ascii_string"]["description"], "The fifth one\n")
        self.assertEqual(data_components["other_ascii_string"]["len"], "30")
        procedures = type["procedures"]
        self.assertEqual(len(procedures), 1)
        self.assertEqual(procedures["init"]["name"], "init")

    def test_kind_specification_components_no_comments(self):
        self.fs.create_file(
            "/fake/path/types.f90",
            contents="""\
    module kind_module
    use, intrinsic :: iso_fortran_env, only: sp => real32, dp => real64, int64
    implicit none
    private

    public :: kind_type

    type :: kind_type
        integer(kind=int64), public :: big_int
        real(kind=sp), public :: single_precision
        real(kind=dp), public :: double_precision
        complex, public :: complex_dp
        logical(kind=1), public :: small_logical
    contains
        procedure, public :: init
    end type kind_type
    end module kind_module
    """,
        )
        result = extract_module_data([Path("/fake/path/types.f90")])
        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(module["module_name"], "kind_module")
        types = module["types"]
        self.assertEqual(len(types), 1)
        type = types["kind_type"]
        self.assertEqual(type["type_name"], "kind_type")
        self.assertEqual(type["attributes"], ["PUBLIC"])
        data_components = type["data_components"]
        self.assertEqual(len(data_components), 5)
        self.assertEqual(data_components["big_int"]["name"], "big_int")
        self.assertEqual(data_components["big_int"]["type"], "INTEGER")
        self.assertEqual(data_components["big_int"]["kind"], "int64")
        self.assertEqual(data_components["big_int"]["attributes"], ["PUBLIC"])
        self.assertEqual(data_components["single_precision"]["name"], "single_precision")
        self.assertEqual(data_components["single_precision"]["type"], "REAL")
        self.assertEqual(data_components["single_precision"]["kind"], "sp")
        self.assertEqual(data_components["single_precision"]["attributes"], ["PUBLIC"])
        self.assertEqual(data_components["double_precision"]["name"], "double_precision")
        self.assertEqual(data_components["double_precision"]["type"], "REAL")
        self.assertEqual(data_components["double_precision"]["kind"], "dp")
        self.assertEqual(data_components["double_precision"]["attributes"], ["PUBLIC"])
        self.assertEqual(data_components["complex_dp"]["name"], "complex_dp")
        self.assertEqual(data_components["complex_dp"]["type"], "COMPLEX")
        self.assertIsNone(data_components["complex_dp"]["kind"])
        self.assertEqual(data_components["complex_dp"]["attributes"], ["PUBLIC"])
        self.assertEqual(data_components["small_logical"]["name"], "small_logical")
        self.assertEqual(data_components["small_logical"]["type"], "LOGICAL")
        self.assertEqual(data_components["small_logical"]["kind"], "1")
        self.assertEqual(data_components["small_logical"]["attributes"], ["PUBLIC"])
        procedures = type["procedures"]
        self.assertEqual(len(procedures), 1)
        self.assertEqual(procedures["init"]["name"], "init")

    def test_kind_specification_components_with_comments(self):
        self.fs.create_file(
            "/fake/path/types.f90",
            contents="""\
    module kind_module
    use, intrinsic :: iso_fortran_env, only: sp => real32, dp => real64, int64
    implicit none
    private

    public :: kind_type

    type :: kind_type
    !!* one *!
        integer(kind=int64), public :: big_int
    !!* two *!
        real(kind=sp), public :: single_precision
    !!* three *!
        real(kind=dp), public :: double_precision
    !!* four *!
        complex, public :: complex_dp
    !!* five *!
        logical(kind=1), public :: small_logical
    contains
        procedure, public :: init
    end type kind_type
    end module kind_module
    """,
        )
        result = extract_module_data([Path("/fake/path/types.f90")])
        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(module["module_name"], "kind_module")
        types = module["types"]
        self.assertEqual(len(types), 1)
        type = types["kind_type"]
        self.assertEqual(type["type_name"], "kind_type")
        self.assertEqual(type["attributes"], ["PUBLIC"])
        data_components = type["data_components"]
        self.assertEqual(len(data_components), 5)
        self.assertEqual(data_components["big_int"]["name"], "big_int")
        self.assertEqual(data_components["big_int"]["type"], "INTEGER")
        self.assertEqual(data_components["big_int"]["kind"], "int64")
        self.assertEqual(data_components["big_int"]["attributes"], ["PUBLIC"])
        self.assertEqual(data_components["big_int"]["description"], "one\n")
        self.assertEqual(data_components["single_precision"]["name"], "single_precision")
        self.assertEqual(data_components["single_precision"]["type"], "REAL")
        self.assertEqual(data_components["single_precision"]["kind"], "sp")
        self.assertEqual(data_components["single_precision"]["attributes"], ["PUBLIC"])
        self.assertEqual(data_components["single_precision"]["description"], "two\n")
        self.assertEqual(data_components["double_precision"]["name"], "double_precision")
        self.assertEqual(data_components["double_precision"]["type"], "REAL")
        self.assertEqual(data_components["double_precision"]["kind"], "dp")
        self.assertEqual(data_components["double_precision"]["attributes"], ["PUBLIC"])
        self.assertEqual(data_components["double_precision"]["description"], "three\n")
        self.assertEqual(data_components["complex_dp"]["name"], "complex_dp")
        self.assertEqual(data_components["complex_dp"]["type"], "COMPLEX")
        self.assertIsNone(data_components["complex_dp"]["kind"])
        self.assertEqual(data_components["complex_dp"]["attributes"], ["PUBLIC"])
        self.assertEqual(data_components["complex_dp"]["description"], "four\n")
        self.assertEqual(data_components["small_logical"]["name"], "small_logical")
        self.assertEqual(data_components["small_logical"]["type"], "LOGICAL")
        self.assertEqual(data_components["small_logical"]["kind"], "1")
        self.assertEqual(data_components["small_logical"]["attributes"], ["PUBLIC"])
        self.assertEqual(data_components["small_logical"]["description"], "five\n")        
        procedures = type["procedures"]
        self.assertEqual(len(procedures), 1)
        self.assertEqual(procedures["init"]["name"], "init")

    def test_array_with_initialization(self):
        self.fs.create_file(
            "/fake/path/types.f90",
            contents="""\
    module array_init_module
    implicit none
    private

    public :: array_init_type

    type :: array_init_type
        integer, dimension(2), public :: int_arr = [1, 2]
        real, dimension(3), public :: real_arr = [1.0, 2.0, 3.0]
        logical, dimension(2,2), public :: log_arr = reshape([.true., .false., .false., .true.], [2,2])
        character(len=5), dimension(2), public :: char_arr = ["Hello", "World"]
    contains
        procedure, public :: init
    end type array_init_type
    end module array_init_module
    """,
        )
        result = extract_module_data([Path("/fake/path/types.f90")])
        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(module["module_name"], "array_init_module")
        types = module["types"]
        self.assertEqual(len(types), 1)
        type = types["array_init_type"]
        self.assertEqual(type["type_name"], "array_init_type")
        self.assertEqual(type["attributes"], ["PUBLIC"])
        
        data_components = type["data_components"]
        self.assertEqual(len(data_components), 4)
        self.assertEqual(data_components["int_arr"]["name"], "int_arr")
        self.assertEqual(data_components["int_arr"]["type"], "INTEGER")
        self.assertEqual(data_components["int_arr"]["dimension"], 
                         {"dimensions": [ArrayBound(BoundType.FIXED, Expression(ExpressionType.LITERAL, "1"), Expression(ExpressionType.LITERAL, "2"))]})
        self.assertEqual(data_components["int_arr"]["initial_value"], "[1, 2]")
        self.assertEqual(data_components["int_arr"]["attributes"], ["PUBLIC"])
        self.assertEqual(data_components["real_arr"]["name"], "real_arr")
        self.assertEqual(data_components["real_arr"]["type"], "REAL")
        self.assertEqual(data_components["real_arr"]["dimension"], 
                         {"dimensions": [ArrayBound(BoundType.FIXED, Expression(ExpressionType.LITERAL, "1"), Expression(ExpressionType.LITERAL, "3"))]})
        self.assertEqual(data_components["real_arr"]["initial_value"], "[1.0, 2.0, 3.0]")
        self.assertEqual(data_components["real_arr"]["attributes"], ["PUBLIC"])
        self.assertEqual(data_components["log_arr"]["name"], "log_arr")
        self.assertEqual(data_components["log_arr"]["type"], "LOGICAL")
        self.assertEqual(data_components["log_arr"]["dimension"], 
                         {"dimensions": [ArrayBound(BoundType.FIXED, Expression(ExpressionType.LITERAL, "1"), Expression(ExpressionType.LITERAL, "2")),
                                         ArrayBound(BoundType.FIXED, Expression(ExpressionType.LITERAL, "1"), Expression(ExpressionType.LITERAL, "2"))]})
        self.assertEqual(data_components["log_arr"]["initial_value"], "RESHAPE([.TRUE., .FALSE., .FALSE., .TRUE.], [2, 2])")
        self.assertEqual(data_components["log_arr"]["attributes"], ["PUBLIC"])
        self.assertEqual(data_components["char_arr"]["name"], "char_arr")
        self.assertEqual(data_components["char_arr"]["type"], "CHARACTER")
        self.assertEqual(data_components["char_arr"]["dimension"], 
                         {"dimensions": [ArrayBound(BoundType.FIXED, Expression(ExpressionType.LITERAL, "1"), Expression(ExpressionType.LITERAL, "2"))]})
        self.assertEqual(data_components["char_arr"]["initial_value"], '["Hello", "World"]')
        self.assertEqual(data_components["char_arr"]["attributes"], ["PUBLIC"])    
        procedures = type["procedures"]
        self.assertEqual(len(procedures), 1)
        self.assertEqual(procedures["init"]["name"], "init")

    def test_allocatable_arrays(self):
        self.fs.create_file(
            "/fake/path/types.f90",
            contents="""\
    module allocatable_module
    implicit none
    private

    public :: allocatable_type

    type :: allocatable_type
        real, allocatable, public :: alloc_x(:)
        integer, allocatable, public :: alloc_matrix(:,:)
        character(len=:), allocatable, public :: alloc_string
        complex, allocatable, public :: alloc_complex(:,:,:)
        logical, allocatable, public :: alloc_logical(:)
    contains
        procedure, public :: init
    end type allocatable_type
    end module allocatable_module
    """,
        )
        result = extract_module_data([Path("/fake/path/types.f90")])
        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(module["module_name"], "allocatable_module")
        types = module["types"]
        self.assertEqual(len(types), 1)
        type = types["allocatable_type"]
        self.assertEqual(type["type_name"], "allocatable_type")
        self.assertEqual(type["attributes"], ["PUBLIC"])
        data_components = type["data_components"]
        self.assertEqual(len(data_components), 5)
        self.assertEqual(data_components["alloc_x"]["name"], "alloc_x")
        self.assertEqual(data_components["alloc_x"]["type"], "REAL")
        self.assertEqual(data_components["alloc_x"]["dimension"], 
                         {"dimensions": [ArrayBound(BoundType.DEFERRED)]})
        self.assertEqual(data_components["alloc_x"]["attributes"], ["ALLOCATABLE", "PUBLIC"])
        self.assertEqual(data_components["alloc_matrix"]["name"], "alloc_matrix")
        self.assertEqual(data_components["alloc_matrix"]["type"], "INTEGER")
        self.assertEqual(data_components["alloc_matrix"]["dimension"], 
                         {"dimensions": [ArrayBound(BoundType.DEFERRED), ArrayBound(BoundType.DEFERRED)]})
        self.assertEqual(data_components["alloc_matrix"]["attributes"], ["ALLOCATABLE", "PUBLIC"])
        self.assertEqual(data_components["alloc_string"]["name"], "alloc_string")
        self.assertEqual(data_components["alloc_string"]["type"], "CHARACTER")
        self.assertEqual(data_components["alloc_string"]["attributes"], ["ALLOCATABLE", "PUBLIC"])
        self.assertEqual(data_components["alloc_complex"]["name"], "alloc_complex")
        self.assertEqual(data_components["alloc_complex"]["type"], "COMPLEX")
        self.assertEqual(data_components["alloc_complex"]["dimension"], 
                         {"dimensions": [ArrayBound(BoundType.DEFERRED), ArrayBound(BoundType.DEFERRED), ArrayBound(BoundType.DEFERRED)]})
        self.assertEqual(data_components["alloc_complex"]["attributes"], ["ALLOCATABLE", "PUBLIC"])
        self.assertEqual(data_components["alloc_logical"]["name"], "alloc_logical")
        self.assertEqual(data_components["alloc_logical"]["type"], "LOGICAL")
        self.assertEqual(data_components["alloc_logical"]["dimension"], 
                         {"dimensions": [ArrayBound(BoundType.DEFERRED)]})
        self.assertEqual(data_components["alloc_logical"]["attributes"], ["ALLOCATABLE", "PUBLIC"])
        procedures = type["procedures"]
        self.assertEqual(len(procedures), 1)
        self.assertEqual(procedures["init"]["name"], "init")
            
    def test_pointer_components(self):
        self.fs.create_file(
            "/fake/path/types.f90",
            contents="""\
    module pointer_module
    implicit none
    private

    public :: pointer_type

    type :: pointer_type
        !!* A pointer to a scalar real value *!
        real, pointer, public :: scalar_ptr => null()
        
        integer, pointer, dimension(:,:), public :: array_ptr
        
        !!* A pointer to a variable-length string
        ! This can be used for dynamic string handling
        !*!
        character(len=:), pointer, public :: string_ptr
        
        !!* Fixed-length string pointer *!
        character(len=50), pointer, public :: fixed_string_ptr => null()
        
        complex, pointer, dimension(:), public :: complex_array_ptr
        
        !!* Logical pointer with initialization *!
        logical, pointer, public :: flag_ptr => null()
    contains
        procedure, public :: init
        procedure, public :: cleanup
    end type pointer_type
    end module pointer_module
    """,
        )
        result = extract_module_data([Path("/fake/path/types.f90")])
        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(module["module_name"], "pointer_module")
        types = module["types"]
        self.assertEqual(len(types), 1)
        type = types["pointer_type"]
        self.assertEqual(type["type_name"], "pointer_type")
        self.assertEqual(type["attributes"], ["PUBLIC"])
        
        data_components = type["data_components"]
        self.assertEqual(len(data_components), 6)
        
        # Scalar pointer with null initialization and documentation
        self.assertEqual(data_components["scalar_ptr"]["name"], "scalar_ptr")
        self.assertEqual(data_components["scalar_ptr"]["type"], "REAL")
        self.assertEqual(data_components["scalar_ptr"]["dimension"], None)
        self.assertEqual(data_components["scalar_ptr"]["initial_value"], "null()")
        self.assertEqual(data_components["scalar_ptr"]["attributes"], ["POINTER", "PUBLIC"])
        self.assertEqual(data_components["scalar_ptr"]["description"], "A pointer to a scalar real value\n")
        
        # Array pointer without documentation
        self.assertEqual(data_components["array_ptr"]["name"], "array_ptr")
        self.assertEqual(data_components["array_ptr"]["type"], "INTEGER")
        self.assertEqual(data_components["array_ptr"]["dimension"], 
                        {"dimensions": [ArrayBound(BoundType.DEFERRED), ArrayBound(BoundType.DEFERRED)]})
        self.assertEqual(data_components["array_ptr"]["initial_value"], None)
        self.assertEqual(data_components["array_ptr"]["attributes"], ["POINTER", "PUBLIC"])
        self.assertEqual(data_components["array_ptr"]["description"], "")
        
        # Variable-length string pointer with multi-line documentation
        self.assertEqual(data_components["string_ptr"]["name"], "string_ptr")
        self.assertEqual(data_components["string_ptr"]["type"], "CHARACTER")
        self.assertEqual(data_components["string_ptr"]["len"], ":")
        self.assertEqual(data_components["string_ptr"]["initial_value"], None)
        self.assertEqual(data_components["string_ptr"]["attributes"], ["POINTER", "PUBLIC"])
        self.assertEqual(data_components["string_ptr"]["description"], 
                        "A pointer to a variable-length string\nThis can be used for dynamic string handling\n")
        
        # Fixed-length string pointer with documentation
        self.assertEqual(data_components["fixed_string_ptr"]["name"], "fixed_string_ptr")
        self.assertEqual(data_components["fixed_string_ptr"]["type"], "CHARACTER")
        self.assertEqual(data_components["fixed_string_ptr"]["len"], "50")
        self.assertEqual(data_components["fixed_string_ptr"]["initial_value"], "null()")
        self.assertEqual(data_components["fixed_string_ptr"]["attributes"], ["POINTER", "PUBLIC"])
        self.assertEqual(data_components["fixed_string_ptr"]["description"], "Fixed-length string pointer\n")
        
        # Complex array pointer without documentation
        self.assertEqual(data_components["complex_array_ptr"]["name"], "complex_array_ptr")
        self.assertEqual(data_components["complex_array_ptr"]["type"], "COMPLEX")
        self.assertEqual(data_components["complex_array_ptr"]["dimension"], 
                        {"dimensions": [ArrayBound(BoundType.DEFERRED)]})
        self.assertEqual(data_components["complex_array_ptr"]["initial_value"], None)
        self.assertEqual(data_components["complex_array_ptr"]["attributes"], ["POINTER", "PUBLIC"])
        self.assertEqual(data_components["complex_array_ptr"]["description"], "")
        
        # Logical pointer with documentation
        self.assertEqual(data_components["flag_ptr"]["name"], "flag_ptr")
        self.assertEqual(data_components["flag_ptr"]["type"], "LOGICAL")
        self.assertEqual(data_components["flag_ptr"]["dimension"], None)
        self.assertEqual(data_components["flag_ptr"]["initial_value"], "null()")
        self.assertEqual(data_components["flag_ptr"]["attributes"], ["POINTER", "PUBLIC"])
        self.assertEqual(data_components["flag_ptr"]["description"], "Logical pointer with initialization\n")
        
        procedures = type["procedures"]
        self.assertEqual(len(procedures), 2)
        self.assertEqual(procedures["init"]["name"], "init")
        self.assertEqual(procedures["cleanup"]["name"], "cleanup")

    def test_user_defined_type_components(self):
        self.fs.create_file(
            "/fake/path/types.f90",
            contents="""\
    module nested_module
    implicit none
    private

    public :: simple_type, nested_type

    type :: simple_type
        real, public :: x
        integer, public :: y
    contains
        procedure, public :: init
    end type simple_type

    type :: nested_type
        !!* A single instance of simple_type *!
        type(simple_type), public :: nested
        
        type(simple_type), dimension(3), public :: nested_array
        
        !!* An allocatable instance that can be allocated at runtime *!
        type(simple_type), allocatable, public :: nested_alloc
        
        !!* A 2D array of simple_type objects *!
        type(simple_type), dimension(2,2), public :: nested_matrix
        
        type(simple_type), allocatable, dimension(:), public :: nested_alloc_array
        
        !!* Pointer to a simple_type instance *!
        type(simple_type), pointer, public :: nested_ptr => null()
    contains
        procedure, public :: init
        procedure, public :: cleanup
    end type nested_type
    end module nested_module
    """,
        )
        result = extract_module_data([Path("/fake/path/types.f90")])
        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(module["module_name"], "nested_module")
        types = module["types"]
        self.assertEqual(len(types), 2)
        
        # Check the simple_type first
        simple_type = types["simple_type"]
        self.assertEqual(simple_type["type_name"], "simple_type")
        self.assertEqual(simple_type["attributes"], ["PUBLIC"])
        
        # Check the nested_type
        nested_type = types["nested_type"]
        self.assertEqual(nested_type["type_name"], "nested_type")
        self.assertEqual(nested_type["attributes"], ["PUBLIC"])
        
        data_components = nested_type["data_components"]
        self.assertEqual(len(data_components), 6)
        
        # Single instance with documentation
        self.assertEqual(data_components["nested"]["name"], "nested")
        self.assertEqual(data_components["nested"]["type"], "simple_type")
        self.assertEqual(data_components["nested"]["dimension"], None)
        self.assertEqual(data_components["nested"]["initial_value"], None)
        self.assertEqual(data_components["nested"]["attributes"], ["PUBLIC"])
        self.assertEqual(data_components["nested"]["description"], "A single instance of simple_type\n")
        
        # Fixed array without documentation
        self.assertEqual(data_components["nested_array"]["name"], "nested_array")
        self.assertEqual(data_components["nested_array"]["type"], "simple_type")
        self.assertEqual(data_components["nested_array"]["dimension"], 
                        {"dimensions": [ArrayBound(BoundType.FIXED, Expression(ExpressionType.LITERAL, "1"), Expression(ExpressionType.LITERAL, "3"))]})
        self.assertEqual(data_components["nested_array"]["initial_value"], None)
        self.assertEqual(data_components["nested_array"]["attributes"], ["PUBLIC"])
        self.assertEqual(data_components["nested_array"]["description"], "")
        
        # Allocatable scalar with documentation
        self.assertEqual(data_components["nested_alloc"]["name"], "nested_alloc")
        self.assertEqual(data_components["nested_alloc"]["type"], "simple_type")
        self.assertEqual(data_components["nested_alloc"]["dimension"], None)
        self.assertEqual(data_components["nested_alloc"]["initial_value"], None)
        self.assertEqual(data_components["nested_alloc"]["attributes"], ["ALLOCATABLE", "PUBLIC"])
        self.assertEqual(data_components["nested_alloc"]["description"], "An allocatable instance that can be allocated at runtime\n")
        
        # 2D fixed array with documentation
        self.assertEqual(data_components["nested_matrix"]["name"], "nested_matrix")
        self.assertEqual(data_components["nested_matrix"]["type"], "simple_type")
        self.assertEqual(data_components["nested_matrix"]["dimension"], 
                        {"dimensions": [ArrayBound(BoundType.FIXED, Expression(ExpressionType.LITERAL, "1"), Expression(ExpressionType.LITERAL, "2")),
                                        ArrayBound(BoundType.FIXED, Expression(ExpressionType.LITERAL, "1"), Expression(ExpressionType.LITERAL, "2"))]})
        self.assertEqual(data_components["nested_matrix"]["initial_value"], None)
        self.assertEqual(data_components["nested_matrix"]["attributes"], ["PUBLIC"])
        self.assertEqual(data_components["nested_matrix"]["description"], "A 2D array of simple_type objects\n")
        
        # Allocatable array without documentation
        self.assertEqual(data_components["nested_alloc_array"]["name"], "nested_alloc_array")
        self.assertEqual(data_components["nested_alloc_array"]["type"], "simple_type")
        self.assertEqual(data_components["nested_alloc_array"]["dimension"], 
                        {"dimensions": [ArrayBound(BoundType.DEFERRED)]})
        self.assertEqual(data_components["nested_alloc_array"]["initial_value"], None)
        self.assertEqual(data_components["nested_alloc_array"]["attributes"], ["ALLOCATABLE", "PUBLIC"])
        self.assertEqual(data_components["nested_alloc_array"]["description"], "")
        
        # Pointer with documentation
        self.assertEqual(data_components["nested_ptr"]["name"], "nested_ptr")
        self.assertEqual(data_components["nested_ptr"]["type"], "simple_type")
        self.assertEqual(data_components["nested_ptr"]["dimension"], None)
        self.assertEqual(data_components["nested_ptr"]["initial_value"], "null()")
        self.assertEqual(data_components["nested_ptr"]["attributes"], ["POINTER", "PUBLIC"])
        self.assertEqual(data_components["nested_ptr"]["description"], "Pointer to a simple_type instance\n")
        
        procedures = nested_type["procedures"]
        self.assertEqual(len(procedures), 2)
        self.assertEqual(procedures["init"]["name"], "init")
        self.assertEqual(procedures["cleanup"]["name"], "cleanup")

    @unittest.skip("fparser doesn't handle target, volatile or protected")
    def test_multiple_attributes_components(self):
        self.fs.create_file(
            "/fake/path/types.f90",
            contents="""\
    module multi_attr_module
    implicit none
    private

    public :: multi_attr_type

    type :: multi_attr_type
        !!* Variable that can be both allocated and targeted by pointers *!
        real, allocatable, target, dimension(:), public :: multi_attr_var
        
        integer, pointer, volatile, private :: volatile_ptr
        
        !!* Protected string - can be read but not modified outside the type *!
        character(len=20), protected, public :: protected_string
        
        !!* A constant value defined at compile time *!
        real, public :: pi = 3.14159265
        
        complex, allocatable, target, dimension(:,:), public :: complex_matrix
        
        !!* Volatile array for hardware interfacing
        ! Values may change outside program control
        !*!
        logical, volatile, dimension(8), public :: status_flags
        
        integer, pointer, protected, public :: protected_ptr => null()
        
        !!* Allocatable string with multiple attributes *!
        character(len=:), allocatable, protected, public :: dynamic_label
        
    contains
        procedure, public :: init
        procedure, public :: set_values
    end type multi_attr_type
    end module multi_attr_module
    """,
        )
        result = extract_module_data([Path("/fake/path/types.f90")])
        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(module["module_name"], "multi_attr_module")
        types = module["types"]
        self.assertEqual(len(types), 1)
        
        type = types["multi_attr_type"]
        self.assertEqual(type["type_name"], "multi_attr_type")
        self.assertEqual(type["attributes"], ["PUBLIC"])
        
        data_components = type["data_components"]
        self.assertEqual(len(data_components), 8)
        
        # Allocatable + target + public with documentation
        self.assertEqual(data_components["multi_attr_var"]["name"], "multi_attr_var")
        self.assertEqual(data_components["multi_attr_var"]["type"], "REAL")
        self.assertEqual(data_components["multi_attr_var"]["dimension"], 
                        {"dimensions": [ArrayBound(BoundType.DEFERRED)]})
        self.assertIn("ALLOCATABLE", data_components["multi_attr_var"]["attributes"])
        self.assertIn("TARGET", data_components["multi_attr_var"]["attributes"])
        self.assertIn("PUBLIC", data_components["multi_attr_var"]["attributes"])
        self.assertEqual(data_components["multi_attr_var"]["description"], 
                        "Variable that can be both allocated and targeted by pointers\n")
        
        # Pointer + volatile + private without documentation
        self.assertEqual(data_components["volatile_ptr"]["name"], "volatile_ptr")
        self.assertEqual(data_components["volatile_ptr"]["type"], "INTEGER")
        self.assertIn("POINTER", data_components["volatile_ptr"]["attributes"])
        self.assertIn("VOLATILE", data_components["volatile_ptr"]["attributes"])
        self.assertIn("PRIVATE", data_components["volatile_ptr"]["attributes"])
        self.assertEqual(data_components["volatile_ptr"]["description"], "")
        
        # Protected + public with documentation
        self.assertEqual(data_components["protected_string"]["name"], "protected_string")
        self.assertEqual(data_components["protected_string"]["type"], "CHARACTER")
        self.assertEqual(data_components["protected_string"]["len"], "20")
        self.assertIn("PROTECTED", data_components["protected_string"]["attributes"])
        self.assertIn("PUBLIC", data_components["protected_string"]["attributes"])
        self.assertEqual(data_components["protected_string"]["description"], 
                        "Protected string - can be read but not modified outside the type\n")
        
        # Parameter + public with documentation
        self.assertEqual(data_components["pi"]["name"], "pi")
        self.assertEqual(data_components["pi"]["type"], "REAL")
        self.assertEqual(data_components["pi"]["initial_value"], "3.14159265")
        self.assertIn("PARAMETER", data_components["pi"]["attributes"])
        self.assertIn("PUBLIC", data_components["pi"]["attributes"])
        self.assertEqual(data_components["pi"]["description"], "A constant value defined at compile time\n")
        
        # Allocatable + target + dimension + public without documentation
        self.assertEqual(data_components["complex_matrix"]["name"], "complex_matrix")
        self.assertEqual(data_components["complex_matrix"]["type"], "COMPLEX")
        self.assertEqual(data_components["complex_matrix"]["dimension"], 
                        {"dimensions": [ArrayBound(BoundType.DEFERRED), ArrayBound(BoundType.DEFERRED)]})
        self.assertIn("ALLOCATABLE", data_components["complex_matrix"]["attributes"])
        self.assertIn("TARGET", data_components["complex_matrix"]["attributes"])
        self.assertIn("PUBLIC", data_components["complex_matrix"]["attributes"])
        self.assertEqual(data_components["complex_matrix"]["description"], "")
        
        # Volatile + dimension + public with multi-line documentation
        self.assertEqual(data_components["status_flags"]["name"], "status_flags")
        self.assertEqual(data_components["status_flags"]["type"], "LOGICAL")
        self.assertEqual(data_components["status_flags"]["dimension"], 
                        {"dimensions": [ArrayBound(BoundType.FIXED, Expression(ExpressionType.LITERAL, "1"), 
                                                    Expression(ExpressionType.LITERAL, "8"))]})
        self.assertIn("VOLATILE", data_components["status_flags"]["attributes"])
        self.assertIn("PUBLIC", data_components["status_flags"]["attributes"])
        self.assertEqual(data_components["status_flags"]["description"], 
                        "Volatile array for hardware interfacing\nValues may change outside program control\n")
        
        # Pointer + protected + public without documentation
        self.assertEqual(data_components["protected_ptr"]["name"], "protected_ptr")
        self.assertEqual(data_components["protected_ptr"]["type"], "INTEGER")
        self.assertEqual(data_components["protected_ptr"]["initial_value"], "NULL()")
        self.assertIn("POINTER", data_components["protected_ptr"]["attributes"])
        self.assertIn("PROTECTED", data_components["protected_ptr"]["attributes"])
        self.assertIn("PUBLIC", data_components["protected_ptr"]["attributes"])
        self.assertEqual(data_components["protected_ptr"]["description"], "")
        
        # Allocatable + protected + public with documentation
        self.assertEqual(data_components["dynamic_label"]["name"], "dynamic_label")
        self.assertEqual(data_components["dynamic_label"]["type"], "CHARACTER")
        self.assertEqual(data_components["dynamic_label"]["len"], ":")
        self.assertIn("ALLOCATABLE", data_components["dynamic_label"]["attributes"])
        self.assertIn("PROTECTED", data_components["dynamic_label"]["attributes"])
        self.assertIn("PUBLIC", data_components["dynamic_label"]["attributes"])
        self.assertEqual(data_components["dynamic_label"]["description"], 
                        "Allocatable string with multiple attributes\n")
        
        procedures = type["procedures"]
        self.assertEqual(len(procedures), 2)
        self.assertEqual(procedures["init"]["name"], "init")
        self.assertEqual(procedures["set_values"]["name"], "set_values")

    def test_inline_array_declaration(self):
        self.fs.create_file(
            "/fake/path/types.f90",
            contents="""\
    module inline_array_module
    implicit none
    private

    public :: inline_array_type

    type :: inline_array_type
        !!* A 3x3 matrix declared inline *!
        real, public :: inline_matrix(3,3)
        
        integer, public :: inline_vector(10) = [(i, i=1,10)]
        
        !!* String array with inline declaration
        ! Each string can hold 5 characters
        !*!
        character(5), public :: inline_strings(2) = ["Hello", "World"]
        
        !!* Complex inline arrays *!
        real, public :: coordinates(3) = [1.0, 2.0, 3.0]
        
        logical, public :: flags(4) = [.true., .false., .true., .false.]
        
        !!* 2D integer array *!
        integer, public :: grid(2,3) = reshape([1,2,3,4,5,6], [2,3])
        
        complex, public :: complex_arr(2) = [(1.0, 0.0), (0.0, 1.0)]
        
        !!* Mixed declaration styles - allocatable with inline syntax *!
        real, allocatable, public :: alloc_inline(:)
        
        !!* Character array with longer strings *!
        character(len=10), public :: names(3) = ["Alice     ", "Bob       ", "Charlie   "]
        
    contains
        procedure, public :: init
        procedure, public :: print_arrays
    end type inline_array_type
    end module inline_array_module
    """,
        )
        result = extract_module_data([Path("/fake/path/types.f90")])
        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(module["module_name"], "inline_array_module")
        types = module["types"]
        self.assertEqual(len(types), 1)
        
        type = types["inline_array_type"]
        self.assertEqual(type["type_name"], "inline_array_type")
        self.assertEqual(type["attributes"], ["PUBLIC"])
        
        data_components = type["data_components"]
        self.assertEqual(len(data_components), 9)
        
        # Basic inline matrix with documentation
        self.assertEqual(data_components["inline_matrix"]["name"], "inline_matrix")
        self.assertEqual(data_components["inline_matrix"]["type"], "REAL")
        self.assertEqual(data_components["inline_matrix"]["dimension"], 
                        {"dimensions": [ArrayBound(BoundType.FIXED, Expression(ExpressionType.LITERAL, "1"), Expression(ExpressionType.LITERAL, "3")),
                                        ArrayBound(BoundType.FIXED, Expression(ExpressionType.LITERAL, "1"), Expression(ExpressionType.LITERAL, "3"))]})
        self.assertEqual(data_components["inline_matrix"]["initial_value"], None)
        self.assertEqual(data_components["inline_matrix"]["attributes"], ["PUBLIC"])
        self.assertEqual(data_components["inline_matrix"]["description"], "A 3x3 matrix declared inline\n")
        
        # Inline vector with implied do initialization
        self.assertEqual(data_components["inline_vector"]["name"], "inline_vector")
        self.assertEqual(data_components["inline_vector"]["type"], "INTEGER")
        self.assertEqual(data_components["inline_vector"]["dimension"], 
                        {"dimensions": [ArrayBound(BoundType.FIXED, Expression(ExpressionType.LITERAL, "1"), Expression(ExpressionType.LITERAL, "10"))]})
        self.assertEqual(data_components["inline_vector"]["initial_value"], "[(i, i = 1, 10)]")
        self.assertEqual(data_components["inline_vector"]["attributes"], ["PUBLIC"])
        
        # Character array with inline declaration and multi-line documentation
        self.assertEqual(data_components["inline_strings"]["name"], "inline_strings")
        self.assertEqual(data_components["inline_strings"]["type"], "CHARACTER")
        self.assertEqual(data_components["inline_strings"]["len"], "5")
        self.assertEqual(data_components["inline_strings"]["dimension"], 
                        {"dimensions": [ArrayBound(BoundType.FIXED, Expression(ExpressionType.LITERAL, "1"), Expression(ExpressionType.LITERAL, "2"))]})
        self.assertEqual(data_components["inline_strings"]["initial_value"], '["Hello", "World"]')
        self.assertEqual(data_components["inline_strings"]["attributes"], ["PUBLIC"])
        self.assertEqual(data_components["inline_strings"]["description"], 
                        "String array with inline declaration\nEach string can hold 5 characters\n")
        
        # Real array with literal initialization and documentation
        self.assertEqual(data_components["coordinates"]["name"], "coordinates")
        self.assertEqual(data_components["coordinates"]["type"], "REAL")
        self.assertEqual(data_components["coordinates"]["dimension"], 
                        {"dimensions": [ArrayBound(BoundType.FIXED, Expression(ExpressionType.LITERAL, "1"), Expression(ExpressionType.LITERAL, "3"))]})
        self.assertEqual(data_components["coordinates"]["initial_value"], "[1.0, 2.0, 3.0]")
        self.assertEqual(data_components["coordinates"]["attributes"], ["PUBLIC"])
        self.assertEqual(data_components["coordinates"]["description"], "Complex inline arrays\n")
        
        # Logical array without documentation
        self.assertEqual(data_components["flags"]["name"], "flags")
        self.assertEqual(data_components["flags"]["type"], "LOGICAL")
        self.assertEqual(data_components["flags"]["dimension"], 
                        {"dimensions": [ArrayBound(BoundType.FIXED, Expression(ExpressionType.LITERAL, "1"), Expression(ExpressionType.LITERAL, "4"))]})
        self.assertEqual(data_components["flags"]["initial_value"], "[.TRUE., .FALSE., .TRUE., .FALSE.]")
        self.assertEqual(data_components["flags"]["attributes"], ["PUBLIC"])
        
        # 2D array with reshape initialization and documentation
        self.assertEqual(data_components["grid"]["name"], "grid")
        self.assertEqual(data_components["grid"]["type"], "INTEGER")
        self.assertEqual(data_components["grid"]["dimension"], 
                        {"dimensions": [ArrayBound(BoundType.FIXED, Expression(ExpressionType.LITERAL, "1"), Expression(ExpressionType.LITERAL, "2")),
                                        ArrayBound(BoundType.FIXED, Expression(ExpressionType.LITERAL, "1"), Expression(ExpressionType.LITERAL, "3"))]})
        self.assertEqual(data_components["grid"]["initial_value"], "RESHAPE([1, 2, 3, 4, 5, 6], [2, 3])")
        self.assertEqual(data_components["grid"]["attributes"], ["PUBLIC"])
        self.assertEqual(data_components["grid"]["description"], "2D integer array\n")
        
        # Complex array without documentation
        self.assertEqual(data_components["complex_arr"]["name"], "complex_arr")
        self.assertEqual(data_components["complex_arr"]["type"], "COMPLEX")
        self.assertEqual(data_components["complex_arr"]["dimension"], 
                        {"dimensions": [ArrayBound(BoundType.FIXED, Expression(ExpressionType.LITERAL, "1"), Expression(ExpressionType.LITERAL, "2"))]})
        self.assertEqual(data_components["complex_arr"]["initial_value"], "[(1.0, 0.0), (0.0, 1.0)]")
        self.assertEqual(data_components["complex_arr"]["attributes"], ["PUBLIC"])
        
        # Allocatable with inline syntax and documentation
        self.assertEqual(data_components["alloc_inline"]["name"], "alloc_inline")
        self.assertEqual(data_components["alloc_inline"]["type"], "REAL")
        self.assertEqual(data_components["alloc_inline"]["dimension"], 
                        {"dimensions": [ArrayBound(BoundType.DEFERRED)]})
        self.assertEqual(data_components["alloc_inline"]["attributes"], ["ALLOCATABLE", "PUBLIC"])
        self.assertEqual(data_components["alloc_inline"]["description"], "Mixed declaration styles - allocatable with inline syntax\n")
        
        # Character array with padding and documentation
        self.assertEqual(data_components["names"]["name"], "names")
        self.assertEqual(data_components["names"]["type"], "CHARACTER")
        self.assertEqual(data_components["names"]["len"], "10")
        self.assertEqual(data_components["names"]["dimension"], 
                        {"dimensions": [ArrayBound(BoundType.FIXED, Expression(ExpressionType.LITERAL, "1"), Expression(ExpressionType.LITERAL, "3"))]})
        self.assertEqual(data_components["names"]["initial_value"], '["Alice     ", "Bob       ", "Charlie   "]')
        self.assertEqual(data_components["names"]["attributes"], ["PUBLIC"])
        self.assertEqual(data_components["names"]["description"], "Character array with longer strings\n")
        
        procedures = type["procedures"]
        self.assertEqual(len(procedures), 2)
        self.assertEqual(procedures["init"]["name"], "init")
        self.assertEqual(procedures["print_arrays"]["name"], "print_arrays")

    def test_empty_type(self):
        self.fs.create_file(
            "/fake/path/types.f90",
            contents="""\
    module empty_module
    implicit none
    private

    public :: empty_type, utility_type

    !!* A type with no data components, only procedures
    ! This can be useful for grouping related procedures
    !*!
    type :: empty_type
    contains
        procedure, public :: do_something
        procedure, public :: do_another_thing
        procedure, private :: helper_method
        final :: cleanup
    end type empty_type

    !!* Another empty type with generic interfaces *!
    type :: utility_type
    contains
        procedure, public :: add_real
        procedure, public :: add_int
        generic, public :: add => add_real, add_int
        procedure, public :: print_info
    end type utility_type

    end module empty_module
    """,
        )
        result = extract_module_data([Path("/fake/path/types.f90")])
        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(module["module_name"], "empty_module")
        types = module["types"]
        self.assertEqual(len(types), 2)
        
        # Check the first empty type
        empty_type = types["empty_type"]
        self.assertEqual(empty_type["type_name"], "empty_type")
        self.assertEqual(empty_type["attributes"], ["PUBLIC"])
        self.assertEqual(empty_type["data_components"], {})  # No data components
        self.assertEqual(empty_type["description"], 
                        "A type with no data components, only procedures\nThis can be useful for grouping related procedures\n")
        
        procedures = empty_type["procedures"]
        self.assertEqual(len(procedures), 4)
        self.assertEqual(procedures["do_something"]["name"], "do_something")
        self.assertEqual(procedures["do_another_thing"]["name"], "do_another_thing")
        self.assertEqual(procedures["helper_method"]["name"], "helper_method")
        self.assertEqual(procedures["cleanup"]["name"], "cleanup")
        self.assertEqual(procedures["cleanup"]["is_final"], True)
        
        # Check the utility type
        utility_type = types["utility_type"]
        self.assertEqual(utility_type["type_name"], "utility_type")
        self.assertEqual(utility_type["attributes"], ["PUBLIC"])
        self.assertEqual(utility_type["data_components"], {})  # No data components
        self.assertEqual(utility_type["description"], "Another empty type with generic interfaces\n")
        
        procedures = utility_type["procedures"]
        self.assertEqual(len(procedures), 3)
        self.assertEqual(procedures["add_real"]["name"], "add_real")
        self.assertEqual(procedures["add_int"]["name"], "add_int")
        self.assertEqual(procedures["print_info"]["name"], "print_info")
        
        generic_interfaces = utility_type["generic_interfaces"]
        self.assertEqual(len(generic_interfaces), 1)
        self.assertIn("add", generic_interfaces)
        self.assertEqual(generic_interfaces["add"]["generic_spec"], "add")
        self.assertEqual(set(generic_interfaces["add"]["specific_procedures"]), {"add_real", "add_int"})


    def test_complex_initialization_expressions(self):
        self.fs.create_file(
            "/fake/path/types.f90",
            contents="""\
    module complex_init_module
    use, intrinsic :: iso_fortran_env, only: real64
    implicit none
    private

    public :: complex_init_type

    type :: complex_init_type
        !!* Square root of 2 *!
        real, public :: sqrt_two = sqrt(2.0)
        
        integer, public :: power_of_two = 2**3 + 1
        
        !!* Array with computed values *!
        real, dimension(3), public :: trig_values = [sin(0.0), cos(0.0), tan(0.0)]
        
        !!* String concatenation *!
        character(len=11), public :: combined = "Hello" // " " // "World"
        
        real(real64), public :: pi_squared = 3.14159265358979323846_real64**2
        
        !!* Logical expressions *!
        logical, public :: is_true = (2 > 1) .and. (.not. .false.)
        
        integer, dimension(4), public :: arithmetic = [1+1, 2*3, 10/2, mod(7,3)]
        
        !!* Complex number initialization *!
        complex, public :: unit_imag = (0.0, 1.0)
        
        real, public :: nested_expr = abs(sin(3.14159/4.0) * cos(3.14159/4.0))
        
        !!* Array with mixed operations *!
        integer, dimension(3), public :: mixed_ops = [max(5,3), min(10,7), merge(1,2,.true.)]
        
    contains
        procedure, public :: init
        procedure, public :: show_values
    end type complex_init_type
    end module complex_init_module
    """,
        )
        result = extract_module_data([Path("/fake/path/types.f90")])
        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(module["module_name"], "complex_init_module")
        types = module["types"]
        self.assertEqual(len(types), 1)
        
        type = types["complex_init_type"]
        self.assertEqual(type["type_name"], "complex_init_type")
        self.assertEqual(type["attributes"], ["PUBLIC"])
        
        data_components = type["data_components"]
        self.assertEqual(len(data_components), 10)
        
        # Function call initialization
        self.assertEqual(data_components["sqrt_two"]["name"], "sqrt_two")
        self.assertEqual(data_components["sqrt_two"]["type"], "REAL")
        self.assertEqual(data_components["sqrt_two"]["initial_value"], "SQRT(2.0)")
        self.assertEqual(data_components["sqrt_two"]["description"], "Square root of 2\n")
        
        # Arithmetic expression
        self.assertEqual(data_components["power_of_two"]["name"], "power_of_two")
        self.assertEqual(data_components["power_of_two"]["type"], "INTEGER")
        self.assertEqual(data_components["power_of_two"]["initial_value"], "2 ** 3 + 1")
        
        # Array with function calls
        self.assertEqual(data_components["trig_values"]["name"], "trig_values")
        self.assertEqual(data_components["trig_values"]["type"], "REAL")
        self.assertEqual(data_components["trig_values"]["dimension"], 
                        {"dimensions": [ArrayBound(BoundType.FIXED, Expression(ExpressionType.LITERAL, "1"), 
                                                    Expression(ExpressionType.LITERAL, "3"))]})
        self.assertEqual(data_components["trig_values"]["initial_value"], "[SIN(0.0), COS(0.0), TAN(0.0)]")
        self.assertEqual(data_components["trig_values"]["description"], "Array with computed values\n")
        
        # String concatenation
        self.assertEqual(data_components["combined"]["name"], "combined")
        self.assertEqual(data_components["combined"]["type"], "CHARACTER")
        self.assertEqual(data_components["combined"]["len"], "11")
        self.assertEqual(data_components["combined"]["initial_value"], '"Hello" // " " // "World"')
        self.assertEqual(data_components["combined"]["description"], "String concatenation\n")
        
        # Real with kind and expression
        self.assertEqual(data_components["pi_squared"]["name"], "pi_squared")
        self.assertEqual(data_components["pi_squared"]["type"], "REAL")
        self.assertEqual(data_components["pi_squared"]["kind"], "real64")
        self.assertEqual(data_components["pi_squared"]["initial_value"], "3.14159265358979323846_real64 ** 2")
        
        # Logical expression
        self.assertEqual(data_components["is_true"]["name"], "is_true")
        self.assertEqual(data_components["is_true"]["type"], "LOGICAL")
        self.assertEqual(data_components["is_true"]["initial_value"], "(2 > 1) .AND. (.NOT. .FALSE.)")
        self.assertEqual(data_components["is_true"]["description"], "Logical expressions\n")
        
        # Array with arithmetic
        self.assertEqual(data_components["arithmetic"]["name"], "arithmetic")
        self.assertEqual(data_components["arithmetic"]["type"], "INTEGER")
        self.assertEqual(data_components["arithmetic"]["dimension"], 
                        {"dimensions": [ArrayBound(BoundType.FIXED, Expression(ExpressionType.LITERAL, "1"), 
                                                    Expression(ExpressionType.LITERAL, "4"))]})
        self.assertEqual(data_components["arithmetic"]["initial_value"], "[1 + 1, 2 * 3, 10 / 2, MOD(7, 3)]")
        
        # Complex number
        self.assertEqual(data_components["unit_imag"]["name"], "unit_imag")
        self.assertEqual(data_components["unit_imag"]["type"], "COMPLEX")
        self.assertEqual(data_components["unit_imag"]["initial_value"], "(0.0, 1.0)")
        self.assertEqual(data_components["unit_imag"]["description"], "Complex number initialization\n")
        
        # Nested expression
        self.assertEqual(data_components["nested_expr"]["name"], "nested_expr")
        self.assertEqual(data_components["nested_expr"]["type"], "REAL")
        self.assertEqual(data_components["nested_expr"]["initial_value"], "ABS(SIN(3.14159 / 4.0) * COS(3.14159 / 4.0))")
        
        # Array with intrinsic functions
        self.assertEqual(data_components["mixed_ops"]["name"], "mixed_ops")
        self.assertEqual(data_components["mixed_ops"]["type"], "INTEGER")
        self.assertEqual(data_components["mixed_ops"]["dimension"], 
                        {"dimensions": [ArrayBound(BoundType.FIXED, Expression(ExpressionType.LITERAL, "1"), 
                                                    Expression(ExpressionType.LITERAL, "3"))]})
        self.assertEqual(data_components["mixed_ops"]["initial_value"], "[MAX(5, 3), MIN(10, 7), MERGE(1, 2, .TRUE.)]")
        self.assertEqual(data_components["mixed_ops"]["description"], "Array with mixed operations\n")
        
        procedures = type["procedures"]
        self.assertEqual(len(procedures), 2)
        self.assertEqual(procedures["init"]["name"], "init")
        self.assertEqual(procedures["show_values"]["name"], "show_values")                

#TODO fparser can cope this this fine - check other coarray tests and see if using codimension[*] will fix them
#TODO add codimension support similar to dimensions:
# "codimension": {
#     "codimensions": ["2", "3", "*"]  # for codimension[2,3,*]
# }
# or 
# "codimension": {
#     "fixed_codims": [2, 3],
#     "assumed_final": True  # for the * in the last position
# }

    def test_coarray_components(self):
        self.fs.create_file(
            "/fake/path/types.f90",
            contents="""\
    module coarray_module
    use, intrinsic :: iso_fortran_env, only: int64
    implicit none
    private

    public :: coarray_type

    type :: coarray_type
        !!* Scalar coarray component *!
        real, codimension[*], public :: coarray_scalar
        
        integer, codimension[*], private :: sync_counter
        
        !!* Array with coarray dimension
        ! Each image has its own copy of this array
        !*!
        real, dimension(100), codimension[*], public :: data_array
        
        !!* 2D array that is also a coarray *!
        integer, dimension(10,10), codimension[*], public :: grid
        
        logical, codimension[*], public :: is_ready = .false.
        
        !!* Allocatable coarray vector *!
        integer, dimension(:), codimension[:], allocatable, public :: coarray_vector
        
        !!* Character coarray for image names *!
        character(len=20), codimension[*], public :: image_name
        
        complex, dimension(3,3), codimension[2,*], public :: matrix_2d_coarray
        
        !!* Multi-dimensional coarray *!
        real, dimension(5), codimension[2,2,*], public :: multi_dim_coarray
        
    contains
        procedure, public :: init
        procedure, public :: sync_all_images
        procedure, public :: exchange_data
    end type coarray_type
    end module coarray_module
    """,
        )
        result = extract_module_data([Path("/fake/path/types.f90")])
        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(module["module_name"], "coarray_module")
        types = module["types"]
        self.assertEqual(len(types), 1)
        
        type = types["coarray_type"]
        self.assertEqual(type["type_name"], "coarray_type")
        self.assertEqual(type["attributes"], ["PUBLIC"])
        
        data_components = type["data_components"]
        self.assertEqual(len(data_components), 9)
        
        # Scalar coarray with documentation
        self.assertEqual(data_components["coarray_scalar"]["name"], "coarray_scalar")
        self.assertEqual(data_components["coarray_scalar"]["type"], "REAL")
 #       self.assertEqual(data_components["coarray_scalar"]["codimension"], {"codimensions": ["*"]})
        self.assertEqual(data_components["coarray_scalar"]["attributes"], ["PUBLIC"])
        self.assertEqual(data_components["coarray_scalar"]["description"], "Scalar coarray component\n")
        
        # Private coarray without documentation
        self.assertEqual(data_components["sync_counter"]["name"], "sync_counter")
        self.assertEqual(data_components["sync_counter"]["type"], "INTEGER")
 #       self.assertEqual(data_components["sync_counter"]["codimension"], {"codimensions": ["*"]})
        self.assertEqual(data_components["sync_counter"]["attributes"], ["PRIVATE"])
        
        # Array with coarray dimension and multi-line documentation
        self.assertEqual(data_components["data_array"]["name"], "data_array")
        self.assertEqual(data_components["data_array"]["type"], "REAL")
        self.assertEqual(data_components["data_array"]["dimension"], 
                        {"dimensions": [ArrayBound(BoundType.FIXED, Expression(ExpressionType.LITERAL, "1"), 
                                                    Expression(ExpressionType.LITERAL, "100"))]})
  #      self.assertEqual(data_components["data_array"]["codimension"], {"codimensions": ["*"]})
        self.assertEqual(data_components["data_array"]["attributes"], ["PUBLIC"])
        self.assertEqual(data_components["data_array"]["description"], 
                        "Array with coarray dimension\nEach image has its own copy of this array\n")
        
        # 2D array coarray with documentation
        self.assertEqual(data_components["grid"]["name"], "grid")
        self.assertEqual(data_components["grid"]["type"], "INTEGER")
        self.assertEqual(data_components["grid"]["dimension"], 
                        {"dimensions": [ArrayBound(BoundType.FIXED, Expression(ExpressionType.LITERAL, "1"), 
                                                    Expression(ExpressionType.LITERAL, "10")),
                                        ArrayBound(BoundType.FIXED, Expression(ExpressionType.LITERAL, "1"), 
                                                    Expression(ExpressionType.LITERAL, "10"))]})
  #      self.assertEqual(data_components["grid"]["codimension"], {"codimensions": ["*"]})
        self.assertEqual(data_components["grid"]["attributes"], ["PUBLIC"])
        self.assertEqual(data_components["grid"]["description"], "2D array that is also a coarray\n")
        
        # Coarray with initialization
        self.assertEqual(data_components["is_ready"]["name"], "is_ready")
        self.assertEqual(data_components["is_ready"]["type"], "LOGICAL")
  #      self.assertEqual(data_components["is_ready"]["codimension"], {"codimensions": ["*"]})
        self.assertEqual(data_components["is_ready"]["initial_value"], ".FALSE.")
        self.assertEqual(data_components["is_ready"]["attributes"], ["PUBLIC"])
        
        # Allocatable coarray with deferred dimensions
        self.assertEqual(data_components["coarray_vector"]["name"], "coarray_vector")
        self.assertEqual(data_components["coarray_vector"]["type"], "INTEGER")
        self.assertEqual(data_components["coarray_vector"]["dimension"], 
                        {"dimensions": [ArrayBound(BoundType.DEFERRED)]})
  #      self.assertEqual(data_components["coarray_vector"]["codimension"], {"codimensions": [":"]})
        self.assertEqual(data_components["coarray_vector"]["attributes"], ["ALLOCATABLE", "PUBLIC"])
        self.assertEqual(data_components["coarray_vector"]["description"], "Allocatable coarray vector\n")
        
        # Character coarray
        self.assertEqual(data_components["image_name"]["name"], "image_name")
        self.assertEqual(data_components["image_name"]["type"], "CHARACTER")
        self.assertEqual(data_components["image_name"]["len"], "20")
  #      self.assertEqual(data_components["image_name"]["codimension"], {"codimensions": ["*"]})
        self.assertEqual(data_components["image_name"]["attributes"], ["PUBLIC"])
        self.assertEqual(data_components["image_name"]["description"], "Character coarray for image names\n")
        
        # 2D coarray indexing
        self.assertEqual(data_components["matrix_2d_coarray"]["name"], "matrix_2d_coarray")
        self.assertEqual(data_components["matrix_2d_coarray"]["type"], "COMPLEX")
        self.assertEqual(data_components["matrix_2d_coarray"]["dimension"], 
                        {"dimensions": [ArrayBound(BoundType.FIXED, Expression(ExpressionType.LITERAL, "1"), 
                                                    Expression(ExpressionType.LITERAL, "3")),
                                        ArrayBound(BoundType.FIXED, Expression(ExpressionType.LITERAL, "1"), 
                                                    Expression(ExpressionType.LITERAL, "3"))]})
        #TODO
    #    self.assertEqual(data_components["matrix_2d_coarray"]["codimension"], {"codimensions": ["2", "*"]})
        self.assertEqual(data_components["matrix_2d_coarray"]["attributes"], ["PUBLIC"])
        
        # Multi-dimensional coarray
        self.assertEqual(data_components["multi_dim_coarray"]["name"], "multi_dim_coarray")
        self.assertEqual(data_components["multi_dim_coarray"]["type"], "REAL")
        self.assertEqual(data_components["multi_dim_coarray"]["dimension"], 
                        {"dimensions": [ArrayBound(BoundType.FIXED, Expression(ExpressionType.LITERAL, "1"), 
                                                    Expression(ExpressionType.LITERAL, "5"))]})
    #    self.assertEqual(data_components["multi_dim_coarray"]["codimension"], {"codimensions": ["2", "2", "*"]})
        self.assertEqual(data_components["multi_dim_coarray"]["attributes"], ["PUBLIC"])
        self.assertEqual(data_components["multi_dim_coarray"]["description"], "Multi-dimensional coarray\n")
    #  
        procedures = type["procedures"]
        self.assertEqual(len(procedures), 3)
        self.assertEqual(procedures["init"]["name"], "init")
        self.assertEqual(procedures["sync_all_images"]["name"], "sync_all_images")
        self.assertEqual(procedures["exchange_data"]["name"], "exchange_data")
        
if __name__ == "__main__":
    unittest.main()
