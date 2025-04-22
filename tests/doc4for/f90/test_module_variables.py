import unittest
from typing import cast
from pathlib import Path
from pyfakefs.fake_filesystem_unittest import TestCase
from doc4for.models.common import Expression, ExpressionType
from doc4for.models.dimension_models import ArrayBound, BoundType, Dimension
from doc4for.f90.generate_module_tree import extract_module_data

# Helper function for creating dimension expressions
def create_dimension_expr(lower, upper):
    return ArrayBound(
        bound_type=BoundType.FIXED,
        lower=Expression(expr_type=ExpressionType.LITERAL, value=str(lower), function_name=None, arguments=None),
        upper=Expression(expr_type=ExpressionType.LITERAL, value=str(upper), function_name=None, arguments=None),
        stride=None
    )


class TestVariableDeclarations(TestCase):

    def setUp(self):
        self.setUpPyfakefs()

    def test_simple_declarations(self):
        self.fs.create_file("/fake/path/module.f90",
                            contents="""\
    module test_mod
        implicit none

        ! Basic types with direct initialization
        integer :: i = 1
        real :: r = 1.0
        complex :: c = (1.0, 2.0)
        logical :: l = .true.
        character(len=10) :: str = "Hello"

        ! With kind parameters
        integer(kind=8) :: big_i = 1000000000
        real(kind=8) :: double = 1.0d0
        
    end module test_mod
    """)
        result = extract_module_data([Path("/fake/path/module.f90")])
        
        # Check basic structure
        self.assertEqual(len(result), 1)
        module_data = result[0]
        self.assertEqual(module_data["module_name"], "test_mod")
        
        variables = module_data["variables"]
        self.assertEqual(len(variables), 7)  

        # Check basic type initializations
        self.assertEqual(variables["i"]["type"], "integer")
        self.assertIsNone(variables["i"]["kind"])
        self.assertEqual(variables["i"]["initial_value"], "1")

        self.assertEqual(variables["r"]["type"], "real")
        self.assertIsNone(variables["r"]["kind"])
        self.assertEqual(variables["r"]["initial_value"], "1.0")

        self.assertEqual(variables["c"]["type"], "complex")
        self.assertIsNone(variables["c"]["kind"])
        self.assertEqual(variables["c"]["initial_value"], "(1.0, 2.0)")

        self.assertEqual(variables["l"]["type"], "logical")
        self.assertEqual(variables["l"]["initial_value"], ".true.")

        self.assertEqual(variables["str"]["type"], "character")
        self.assertEqual(variables["str"]["length"], "10")
        self.assertEqual(variables["str"]["initial_value"], '"Hello"')

        # Check kind specifications
        self.assertEqual(variables["big_i"]["type"], "integer")
        self.assertEqual(variables["big_i"]["kind"], "8")
        self.assertEqual(variables["big_i"]["initial_value"], "1000000000")

        self.assertEqual(variables["double"]["type"], "real")
        self.assertEqual(variables["double"]["kind"], "8")
        self.assertEqual(variables["double"]["initial_value"], "1.0d0")


    def test_array_declarations(self):
        self.fs.create_file("/fake/path/module.f90",
                            contents="""\
    module test_mod
        implicit none

        ! Fixed size arrays with different syntaxes
        integer :: arr1(5) = [1, 2, 3, 4, 5]              ! Modern syntax
        integer :: arr2(3) = (/1, 2, 3/)                  ! Old syntax
        real :: arr3(4) = [1.0, 2*2.0, 1.0]              ! With repeat
        
        ! Multi-dimensional arrays
        real :: matrix(2,2) = reshape([1.0, 2.0, 3.0, 4.0], [2,2])
        
        ! Arrays with explicit bounds
        integer :: explicit(-1:1) = [-1, 0, 1]
        
        ! Array constructors with operations
        real :: computed(3) = [1.0, 1.5, 2.0] * 2.0
                
        ! Character arrays
        character(len=5) :: str_arr(2) = ["Hello", "World"]

    end module test_mod
    """)
        result = extract_module_data([Path("/fake/path/module.f90")])
        
        # Check basic structure
        self.assertEqual(len(result), 1)
        module_data = result[0]
        self.assertEqual(module_data["module_name"], "test_mod")
        
        # Check variables
        variables = module_data["variables"]
        self.assertEqual(len(variables), 7)  

        # Check simple array with modern syntax
        self.assertEqual(variables["arr1"]["type"], "integer")
        dimension = cast(Dimension, variables["arr1"]["dimension"])
        self.assertEqual(dimension["dimensions"][0], create_dimension_expr(1, 5))
        self.assertEqual(variables["arr1"]["initial_value"], "1, 2, 3, 4, 5")

        # Check array with old syntax
        self.assertEqual(variables["arr2"]["type"], "integer")
        dimension = cast(Dimension, variables["arr2"]["dimension"])
        self.assertEqual(dimension["dimensions"][0], create_dimension_expr(1, 3))
        self.assertEqual(variables["arr2"]["initial_value"], "1, 2, 3")

        # Check array with repeat syntax
        self.assertEqual(variables["arr3"]["type"], "real")
        dimension = cast(Dimension, variables["arr3"]["dimension"])
        self.assertEqual(dimension["dimensions"][0], create_dimension_expr(1, 4))
        self.assertEqual(variables["arr3"]["initial_value"], "1.0, 2.0, 2.0, 1.0")

        # Check multi-dimensional array
        self.assertEqual(variables["matrix"]["type"], "real")
        dimension = cast(Dimension, variables["matrix"]["dimension"])
        self.assertEqual(dimension["dimensions"][0], create_dimension_expr(1, 2))
        self.assertEqual(dimension["dimensions"][1], create_dimension_expr(1, 2))
        self.assertEqual(variables["matrix"]["initial_value"], "reshape([1.0, 2.0, 3.0, 4.0], [2,2])")

        # Check explicit bounds
        self.assertEqual(variables["explicit"]["type"], "integer")
        dimension = cast(Dimension, variables["explicit"]["dimension"])
        self.assertEqual(dimension["dimensions"][0], create_dimension_expr(-1, 1))
        self.assertEqual(variables["explicit"]["initial_value"], "-1, 0, 1")

        # Check array with operations
        self.assertEqual(variables["computed"]["type"], "real")
        dimension = cast(Dimension, variables["computed"]["dimension"])
        self.assertEqual(dimension["dimensions"][0], create_dimension_expr(1, 3))
        self.assertEqual(variables["computed"]["initial_value"], "[1.0, 1.5, 2.0] * 2.0")

        # Check character array
        self.assertEqual(variables["str_arr"]["type"], "character")
        self.assertEqual(variables["str_arr"]["length"], "5")
        dimension = cast(Dimension, variables["str_arr"]["dimension"])        
        self.assertEqual(dimension["dimensions"][0], create_dimension_expr(1, 2))
        self.assertEqual(variables["str_arr"]["initial_value"], '"Hello", "World"')

    def test_character_array_declarations(self):
        self.fs.create_file("/fake/path/module.f90",
                            contents="""\
    module test_mod
        implicit none
                
        ! Character arrays
        character(len=5) :: str_arr(2) = ["Hello", "World"]
        character*10 names(3) = (/"aaaaaaaaaa", "bbbbbbbbbb", "cccccccccc"/)
    end module test_mod
    """)
        result = extract_module_data([Path("/fake/path/module.f90")])
        
        # Check basic structure
        self.assertEqual(len(result), 1)
        module_data = result[0]
        self.assertEqual(module_data["module_name"], "test_mod")
        
        variables = module_data["variables"]
        self.assertEqual(len(variables), 2)  

        # Check new-style character array
        self.assertEqual(variables["str_arr"]["type"], "character")
        self.assertEqual(variables["str_arr"]["length"], "5")
        dimension = cast(Dimension, variables["str_arr"]["dimension"])
        self.assertEqual(dimension["dimensions"][0], create_dimension_expr(1, 2))
        self.assertEqual(variables["str_arr"]["initial_value"], '"Hello", "World"')

        # Check old-style character array
        self.assertEqual(variables["names"]["type"], "character")
        self.assertEqual(variables["names"]["length"], "10")
        dimension = cast(Dimension, variables["names"]["dimension"])
        self.assertEqual(dimension["dimensions"][0], create_dimension_expr(1, 3))
        self.assertEqual(variables["names"]["initial_value"], '"aaaaaaaaaa", "bbbbbbbbbb", "cccccccccc"')

    def test_derived_type_declarations(self):
        self.fs.create_file("/fake/path/module.f90",
                            contents="""\
    module test_mod
        implicit none

        type :: point
            real :: x, y
        end type point

        type :: circle
            type(point) :: center
            real :: radius
        end type circle

        ! Basic derived type initialization
        type(point) :: p = point(1.0, 2.0)
        
        ! Array of derived types
        type(point) :: points(2) = [point(0.0, 0.0), point(1.0, 1.0)]
        
        ! Nested derived type
        type(circle) :: c = circle(point(0.0, 0.0), 5.0)

    end module test_mod
    """)
        result = extract_module_data([Path("/fake/path/module.f90")])
        
        # Check basic structure
        self.assertEqual(len(result), 1)
        module_data = result[0]
        self.assertEqual(module_data["module_name"], "test_mod")
        
        # Check types are properly parsed
        types = module_data["types"]
        self.assertEqual(len(types), 2)  # point and circle
        
        self.assertIn("point", types)
        self.assertEqual(types["point"]["type_name"], "point")
        self.assertEqual(len(types["point"]["data_components"]), 2)  # x and y
        self.assertEqual(types["point"]["data_components"]["x"]["type"], "real")
        self.assertEqual(types["point"]["data_components"]["y"]["type"], "real")
        
        self.assertIn("circle", types)
        self.assertEqual(types["circle"]["type_name"], "circle")
        self.assertEqual(len(types["circle"]["data_components"]), 2)  # center and radius
        self.assertEqual(types["circle"]["data_components"]["center"]["type"], "point")
        self.assertEqual(types["circle"]["data_components"]["radius"]["type"], "real")

        # Check variables
        variables = module_data["variables"]
        self.assertEqual(len(variables), 3)  # p, points, c

        # Check basic derived type variable
        self.assertEqual(variables["p"]["type"], "point")
        self.assertEqual(variables["p"]["name"], "p")
        self.assertEqual(variables["p"]["kind"], "point")
        self.assertEqual(variables["p"]["initial_value"], "point(1.0, 2.0)")

        # Check array of derived types
        self.assertEqual(variables["points"]["type"], "point")
        self.assertEqual(variables["points"]["name"], "points")
        dimension = cast(Dimension, variables["points"]["dimension"])
        self.assertEqual(dimension["dimensions"][0], create_dimension_expr(1, 2))
        self.assertEqual(variables["points"]["initial_value"], "point(0.0, 0.0), point(1.0, 1.0)")

        # Check nested derived type
        self.assertEqual(variables["c"]["type"], "circle")
        self.assertEqual(variables["c"]["name"], "c")
        self.assertEqual(variables["c"]["initial_value"], "circle(point(0.0, 0.0), 5.0)")

    def test_array_constructor_functions(self):
        self.fs.create_file("/fake/path/module.f90",
                            contents="""\
    module test_mod
        implicit none
        
        ! Using array constructor functions
        real :: seq(5) = [(real(i), i=1,5)]
        real :: seq2(5) = [(2.0*i, i=1,5)]
        
        ! Using intrinsic functions
        real :: zeros(3) = spread(0.0, dim=1, ncopies=3)
        real :: ones(3,3) = reshape([9*1.0], [3,3])
        
        ! Using array sections
        integer :: full(10) = [(i, i=1,10)]
        integer :: part(5) = full(1:10:2)  ! Every second element

    end module test_mod
    """)
        result = extract_module_data([Path("/fake/path/module.f90")])
        
        # Check basic structure
        self.assertEqual(len(result), 1)
        module_data = result[0]
        self.assertEqual(module_data["module_name"], "test_mod")
        
        # Check variables
        variables = module_data["variables"]
        self.assertEqual(len(variables), 6)  # seq, seq2, zeros, ones, full, part

        # Check array constructor function initializations
        self.assertEqual(variables["seq"]["type"], "real")
        dimension = cast(Dimension, variables["seq"]["dimension"])
        self.assertEqual(dimension["dimensions"][0], create_dimension_expr(1, 5))
        self.assertEqual(variables["seq"]["initial_value"], "(real(i), i=1,5)")

        self.assertEqual(variables["seq2"]["type"], "real")
        dimension = cast(Dimension, variables["seq2"]["dimension"])
        self.assertEqual(dimension["dimensions"][0], create_dimension_expr(1, 5))
        self.assertEqual(variables["seq2"]["initial_value"], "(2.0*i, i=1,5)")

        # Check intrinsic function initializations
        self.assertEqual(variables["zeros"]["type"], "real")
        dimension = cast(Dimension, variables["zeros"]["dimension"])
        self.assertEqual(dimension["dimensions"][0], create_dimension_expr(1, 3))
        self.assertEqual(variables["zeros"]["initial_value"], "spread(0.0, dim=1, ncopies=3)")

        self.assertEqual(variables["ones"]["type"], "real")
        dimension = cast(Dimension, variables["ones"]["dimension"])
        self.assertEqual(len(dimension["dimensions"]), 2)
        self.assertEqual(dimension["dimensions"][0], create_dimension_expr(1, 3))
        self.assertEqual(dimension["dimensions"][1], create_dimension_expr(1, 3))
        self.assertEqual(variables["ones"]["initial_value"], "reshape([9*1.0], [3,3])")

        # Check array section initializations
        self.assertEqual(variables["full"]["type"], "integer")
        dimension = cast(Dimension, variables["full"]["dimension"])
        self.assertEqual(dimension["dimensions"][0], create_dimension_expr(1, 10))
        self.assertEqual(variables["full"]["initial_value"], "(i, i=1,10)")

        self.assertEqual(variables["part"]["type"], "integer")
        dimension = cast(Dimension, variables["part"]["dimension"])
        self.assertEqual(dimension["dimensions"][0], create_dimension_expr(1, 5))
        self.assertEqual(variables["part"]["initial_value"], "full(1:10:2)")


    def test_allocatable_and_pointers(self):
        self.fs.create_file("/fake/path/module.f90",
                            contents="""\
    module test_mod
        implicit none

        ! Allocatable arrays (no initialization at declaration)
        real, allocatable :: alloc_arr(:)
        real, allocatable :: alloc_matrix(:,:)
        
        ! Pointers
        real, pointer :: p_scalar => null()
        real, pointer :: p_arr(:) => null()
        
        ! Targets for pointers
        real, target :: target_val = 1.0
        real, target :: target_arr(3) = [1.0, 2.0, 3.0]

        real, target :: scalar_target = 42.0
        real, target :: array_target(3) = [1.0, 2.0, 3.0]
    
        ! Pointers to actual targets
        real, pointer :: p_to_scalar => scalar_target
        real, pointer :: p_to_array(:) => array_target
        real, pointer :: p_to_slice(:) => array_target(1:2)  ! Points to part of array
    end module test_mod
    """)
        result = extract_module_data([Path("/fake/path/module.f90")])
        
        # Check basic structure
        self.assertEqual(len(result), 1)
        module_data = result[0]
        self.assertEqual(module_data["module_name"], "test_mod")
        
        # Check variables
        variables = module_data["variables"]
        self.assertEqual(len(variables), 11)

        # Check allocatable arrays
        self.assertEqual(variables["alloc_arr"]["type"], "real")
        self.assertIn("allocatable", variables["alloc_arr"]["attributes"])
        dimension = cast(Dimension, variables["alloc_arr"]["dimension"])
        self.assertIsNone(dimension["dimensions"][0].lower)
        self.assertIsNone(dimension["dimensions"][0].upper)
        self.assertIsNone(variables["alloc_arr"]["initial_value"])

        self.assertEqual(variables["alloc_matrix"]["type"], "real")
        self.assertIn("allocatable", variables["alloc_matrix"]["attributes"])
        dimension = cast(Dimension, variables["alloc_matrix"]["dimension"])
        self.assertEqual(len(dimension["dimensions"]), 2)
        self.assertIsNone(dimension["dimensions"][0].lower)
        self.assertIsNone(dimension["dimensions"][0].upper)
        self.assertIsNone(dimension["dimensions"][1].lower)
        self.assertIsNone(dimension["dimensions"][1].upper)
        self.assertIsNone(variables["alloc_matrix"]["initial_value"])

        # Check pointers
        self.assertEqual(variables["p_scalar"]["type"], "real")
        self.assertIn("pointer", variables["p_scalar"]["attributes"])
        self.assertEqual(variables["p_scalar"]["initial_value"], "null()")

        self.assertEqual(variables["p_arr"]["type"], "real")
        self.assertIn("pointer", variables["p_arr"]["attributes"])
        dimension = cast(Dimension, variables["p_arr"]["dimension"])
        self.assertIsNone(dimension["dimensions"][0].lower)
        self.assertIsNone(dimension["dimensions"][0].upper)
        self.assertEqual(variables["p_arr"]["initial_value"], "null()")

        # Check targets
        self.assertEqual(variables["target_val"]["type"], "real")
        self.assertIn("target", variables["target_val"]["attributes"])
        self.assertEqual(variables["target_val"]["initial_value"], "1.0")

        self.assertEqual(variables["target_arr"]["type"], "real")
        self.assertIn("target", variables["target_arr"]["attributes"])
        dimension = cast(Dimension, variables["target_arr"]["dimension"])
        self.assertEqual(dimension["dimensions"][0], create_dimension_expr(1, 3))
        self.assertEqual(variables["target_arr"]["initial_value"], "1.0, 2.0, 3.0")

        # Test pointers to actual targets
        self.assertEqual(variables["p_to_scalar"]["type"], "real")
        self.assertIn("pointer", variables["p_to_scalar"]["attributes"])
        self.assertEqual(variables["p_to_scalar"]["initial_value"], "scalar_target")

        self.assertEqual(variables["p_to_array"]["type"], "real")
        self.assertIn("pointer", variables["p_to_array"]["attributes"])
        dimension = cast(Dimension, variables["p_to_array"]["dimension"])
        self.assertIsNone(dimension["dimensions"][0].lower)
        self.assertIsNone(dimension["dimensions"][0].upper)
        self.assertEqual(variables["p_to_array"]["initial_value"], "array_target")

        self.assertEqual(variables["p_to_slice"]["type"], "real")
        self.assertIn("pointer", variables["p_to_slice"]["attributes"])
        dimension = cast(Dimension, variables["p_to_slice"]["dimension"])
        self.assertIsNone(dimension["dimensions"][0].lower)
        self.assertIsNone(dimension["dimensions"][0].upper)
        self.assertEqual(variables["p_to_slice"]["initial_value"], "array_target(1:2)")

    def test_special_initializations(self):
        self.fs.create_file("/fake/path/module.f90",
                            contents="""\
    module test_mod
        implicit none

        ! Using expressions in initialization
        real :: computed = 2.0 * acos(-1.0)
        
        ! Using previously declared parameters
        real :: factor = 2.0
        real :: scaled = factor * 5.0
        
        ! Array initialization with mixed operations
        real :: mixed(4) = [1.0, factor*2.0, 3.0**2, sqrt(16.0)]
        
        ! Character with len
        character(len=19) :: long_str = "This is a long string"
        
    end module test_mod
    """)
        result = extract_module_data([Path("/fake/path/module.f90")])
        
        # Check basic structure
        self.assertEqual(len(result), 1)
        module_data = result[0]
        self.assertEqual(module_data["module_name"], "test_mod")
        
        # Check variables and parameters
        variables = module_data["variables"]
        
        # Check expression initialization
        self.assertEqual(variables["computed"]["type"], "real")
        self.assertEqual(variables["computed"]["initial_value"], "2.0 * acos(-1.0)")
        
        # Check parameter and its use
        self.assertEqual(variables["factor"]["type"], "real")
        self.assertEqual(variables["factor"]["initial_value"], "2.0")
        
        self.assertEqual(variables["scaled"]["type"], "real")
        self.assertEqual(variables["scaled"]["initial_value"], "factor * 5.0")
        
        # Check array with mixed operations
        self.assertEqual(variables["mixed"]["type"], "real")
        dimension = cast(Dimension, variables["mixed"]["dimension"])
        self.assertEqual(dimension["dimensions"][0], create_dimension_expr(1, 4))
        self.assertEqual(variables["mixed"]["initial_value"], "1.0, factor*2.0, 3.0**2, sqrt(16.0)")
        
        # Check character with len
        self.assertEqual(variables["long_str"]["type"], "character")
        self.assertEqual(variables["long_str"]["length"], "19")
        self.assertEqual(variables["long_str"]["initial_value"], '"This is a long string"')
        
if __name__ == "__main__":
    unittest.main()
