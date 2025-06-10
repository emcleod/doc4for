import unittest
from pathlib import Path
from typing import cast
from pyfakefs.fake_filesystem_unittest import TestCase
from doc4for.models.common import Expression, ExpressionType
from doc4for.models.variable_models import PolymorphismType
from doc4for.models.dimension_models import ArrayBound, BoundType, Dimension
from doc4for.f90.generate_module_tree import extract_module_data

def create_dimension_expr(lower, upper):
    """Helper function for creating dimension expressions"""
    return ArrayBound(
        bound_type=BoundType.FIXED if not (isinstance(lower, str) and "n" in lower.lower() or 
                                         isinstance(upper, str) and "n" in upper.lower()) else BoundType.VARIABLE,
        lower=Expression(expr_type=ExpressionType.LITERAL, value=str(lower), function_name=None, arguments=None),
        upper=Expression(expr_type=ExpressionType.LITERAL, value=str(upper), function_name=None, arguments=None)
    )

class TestArrayDimensions(TestCase):
    maxDiff = None

    def setUp(self):
        self.setUpPyfakefs()

    def test_simple_array_declarations(self):
        """Test basic array declarations with simple dimensions."""
        self.fs.create_file(
            "/fake/path/simple_arrays.f90",
            contents="""
module simple_arrays
    implicit none
    
    ! Simple 1D arrays with different syntaxes
    real x(10)
    real :: y(10)
    
    ! Simple 2D arrays
    real z(10, 20)
    real :: w(10,20)
    
end module simple_arrays
            """
        )
        
        result = extract_module_data([Path("/fake/path/simple_arrays.f90")])
        
        # Check basic structure
        self.assertEqual(len(result), 1)
        module_data = result[0]
        self.assertEqual(module_data["module_name"], "simple_arrays")
        
        # Check variables
        variables = module_data["variables"]
        self.assertEqual(len(variables), 4)
        
        # Check 1D arrays
        for var_name in ["x", "y"]:
            self.assertEqual(variables[var_name]["type"], "REAL")
            dimension = cast(Dimension, variables[var_name]["dimension"])
            self.assertEqual(len(dimension["dimensions"]), 1)
            self.assertEqual(dimension["dimensions"][0], create_dimension_expr(1, 10))
            self.assertEqual(variables[var_name]["polymorphism_type"], PolymorphismType.NONE)
            
        # Check 2D arrays
        for var_name in ["z", "w"]:
            self.assertEqual(variables[var_name]["type"], "REAL")
            dimension = cast(Dimension, variables[var_name]["dimension"])
            self.assertEqual(len(dimension["dimensions"]), 2)
            self.assertEqual(dimension["dimensions"][0], create_dimension_expr(1, 10))
            self.assertEqual(dimension["dimensions"][1], create_dimension_expr(1, 20))
            self.assertEqual(variables[var_name]["polymorphism_type"], PolymorphismType.NONE)

    def test_explicit_bounds(self):
        """Test array declarations with explicit bounds."""
        self.fs.create_file(
            "/fake/path/explicit_bounds.f90",
            contents="""
module explicit_bounds
    implicit none
    
    ! Arrays with explicit bounds
    real x(0:9)
    real :: y(-5:5)
    
    ! Arrays with variable upper bounds
    integer :: n = 10
    real :: z(2:n)
    
end module explicit_bounds
            """
        )
        
        result = extract_module_data([Path("/fake/path/explicit_bounds.f90")])
        
        # Check basic structure
        module_data = result[0]
        
        # Check variables
        variables = module_data["variables"]
        
        # Check array with 0-based indexing
        self.assertEqual(variables["x"]["type"], "REAL")
        dimension = cast(Dimension, variables["x"]["dimension"])
        self.assertEqual(dimension["dimensions"][0], create_dimension_expr(0, 9))
        self.assertEqual(variables["x"]["polymorphism_type"], PolymorphismType.NONE)

        # Check array with negative lower bound
        self.assertEqual(variables["y"]["type"], "REAL")
        dimension = cast(Dimension, variables["y"]["dimension"])
        self.assertEqual(dimension["dimensions"][0], create_dimension_expr(-5, 5))
        self.assertEqual(variables["y"]["polymorphism_type"], PolymorphismType.NONE)
        
        # Check array with variable upper bound
        self.assertEqual(variables["z"]["type"], "REAL")
        dimension = cast(Dimension, variables["z"]["dimension"])
        self.assertEqual(dimension["dimensions"][0].bound_type, BoundType.VARIABLE)
        self.assertEqual(dimension["dimensions"][0].lower.value, "2")
        self.assertEqual(dimension["dimensions"][0].upper.value, "n")
        self.assertEqual(variables["z"]["polymorphism_type"], PolymorphismType.NONE)

    def test_variable_dimensions(self):
        """Test array declarations with variable dimensions."""
        self.fs.create_file(
            "/fake/path/variable_dims.f90",
            contents="""
module variable_dims
    implicit none
    
    integer :: n = 10, m = 20
    
    ! Simple variable dimension
    real :: x(n)
    
    ! Function call in dimension
    real :: y(max(n,m), 10)
    
    ! Expression in dimension
    real :: z(2*5, n+1)
    
    ! Variable bounds
    real :: w(n:m, 1:10)
    
    ! Function to be used in test
    contains
    
    function max(a, b)
        integer, intent(in) :: a, b
        integer :: max
        if (a > b) then
            max = a
        else
            max = b
        end if
    end function max
    
end module variable_dims
            """
        )
        
        result = extract_module_data([Path("/fake/path/variable_dims.f90")])
        module_data = result[0]
        variables = module_data["variables"]
        
        # Check simple variable dimension
        self.assertEqual(variables["x"]["type"], "REAL")
        dimension = cast(Dimension, variables["x"]["dimension"])
        self.assertEqual(dimension["dimensions"][0].bound_type, BoundType.VARIABLE)
        self.assertEqual(dimension["dimensions"][0].lower.value, "1")
        self.assertEqual(dimension["dimensions"][0].upper.value, "n")
        self.assertEqual(variables["x"]["polymorphism_type"], PolymorphismType.NONE)
        
        # Check function call in dimension
        self.assertEqual(variables["y"]["type"], "REAL")
        dimension = cast(Dimension, variables["y"]["dimension"])
        self.assertEqual(dimension["dimensions"][0].bound_type, BoundType.VARIABLE)
        self.assertEqual(dimension["dimensions"][0].lower.value, "1")
        self.assertTrue("MAX" in dimension["dimensions"][0].upper.value.upper())
        self.assertEqual(variables["y"]["polymorphism_type"], PolymorphismType.NONE)
        
        # Check expression in dimension
        self.assertEqual(variables["z"]["type"], "REAL")
        dimension = cast(Dimension, variables["z"]["dimension"])
        self.assertEqual(dimension["dimensions"][0].lower.value, "1")
        self.assertEqual(dimension["dimensions"][0].upper.value, "2 * 5")
        self.assertEqual(dimension["dimensions"][1].upper.value, "n + 1")
        self.assertEqual(variables["z"]["polymorphism_type"], PolymorphismType.NONE)
        
        # Check variable bounds
        self.assertEqual(variables["w"]["type"], "REAL")
        dimension = cast(Dimension, variables["w"]["dimension"])
        self.assertEqual(dimension["dimensions"][0].bound_type, BoundType.VARIABLE)
        self.assertEqual(dimension["dimensions"][0].lower.value, "n")
        self.assertEqual(dimension["dimensions"][0].upper.value, "m")
        self.assertEqual(variables["w"]["polymorphism_type"], PolymorphismType.NONE)

    def test_dimension_attribute_style(self):
        """Test arrays declared using dimension attribute syntax."""
        self.fs.create_file(
            "/fake/path/dimension_attr.f90",
            contents="""
    module dimension_attr
        implicit none
        
        ! Simple dimension attribute
        real, dimension(10) :: x
        
        ! 2D dimension attribute
        real, dimension(10, 20) :: y
        
        ! Dimension with explicit bounds
        real, dimension(-5:5) :: z
        
        ! Complex dimension specifications
        integer :: n = 10
        real, dimension(-5:5,10,n:20+n) :: w
        
    end module dimension_attr
            """
        )
        
        result = extract_module_data([Path("/fake/path/dimension_attr.f90")])
        module_data = result[0]
        variables = module_data["variables"]
        
        # Check simple dimension attribute
        self.assertEqual(variables["x"]["type"], "REAL")
        dimension = cast(Dimension, variables["x"]["dimension"])
        self.assertEqual(dimension["dimensions"][0], create_dimension_expr(1, 10))
        self.assertEqual(variables["x"]["polymorphism_type"], PolymorphismType.NONE)
        
        # Check 2D dimension attribute
        self.assertEqual(variables["y"]["type"], "REAL")
        dimension = cast(Dimension, variables["y"]["dimension"])
        self.assertEqual(dimension["dimensions"][0], create_dimension_expr(1, 10))
        self.assertEqual(dimension["dimensions"][1], create_dimension_expr(1, 20))
        self.assertEqual(variables["y"]["polymorphism_type"], PolymorphismType.NONE)
       
        # Check dimension with explicit bounds
        self.assertEqual(variables["z"]["type"], "REAL")
        dimension = cast(Dimension, variables["z"]["dimension"])
        self.assertEqual(dimension["dimensions"][0], create_dimension_expr(-5, 5))
        self.assertEqual(variables["z"]["polymorphism_type"], PolymorphismType.NONE)
        
        # Check complex dimension specifications
        self.assertEqual(variables["w"]["type"], "REAL")
        dimension = cast(Dimension, variables["w"]["dimension"])
        self.assertEqual(dimension["dimensions"][0], create_dimension_expr(-5, 5))
        self.assertEqual(dimension["dimensions"][1], create_dimension_expr(1, 10))
        self.assertEqual(dimension["dimensions"][2].bound_type, BoundType.VARIABLE)
        self.assertEqual(dimension["dimensions"][2].lower.value, "n")
        self.assertEqual(dimension["dimensions"][2].upper.value, "20 + n")
        self.assertEqual(variables["w"]["polymorphism_type"], PolymorphismType.NONE)

    def test_array_initialization(self):
        """Test arrays with initialization values."""
        self.fs.create_file(
            "/fake/path/array_init.f90",
            contents="""
    module array_init
        implicit none
        
        ! Reshape initialization
        real :: x(2,2) = reshape((/1.0, 2.0, 3.0, 4.0/), (/2, 2/))
        
        ! Direct 2D initialization (non-standard, might not work with all compilers)
        real :: y(2,2) = reshape((/1.0, 2.0, 3.0, 4.0/), shape(y))
        
        ! Parameter array with modern syntax
        integer, parameter, dimension(3) :: arr = [1, 2, 3]
        
        ! Alternative array initialization
        real, parameter :: matrix(2,2) = reshape([1.0, 2.0, 3.0, 4.0], [2, 2])
        
    end module array_init
            """
        )
        
        result = extract_module_data([Path("/fake/path/array_init.f90")])
        module_data = result[0]
        variables = module_data["variables"]
        parameters = module_data["parameters"]
        
        # Check reshape initialization
        self.assertEqual(variables["x"]["type"], "REAL")
        self.assertEqual(variables["x"]["type"], "REAL")
        dimension = cast(Dimension, variables["x"]["dimension"])
        self.assertEqual(len(dimension["dimensions"]), 2)  # 2D array
        self.assertEqual(dimension["dimensions"][0], create_dimension_expr(1, 2))
        self.assertEqual(dimension["dimensions"][1], create_dimension_expr(1, 2))
        self.assertTrue("RESHAPE" in variables["x"]["initial_value"].upper())        
        self.assertEqual(variables["x"]["polymorphism_type"], PolymorphismType.NONE)
        
        # Check reshape initialization
        self.assertEqual(variables["y"]["type"], "REAL")
        self.assertEqual(variables["y"]["type"], "REAL")
        dimension = cast(Dimension, variables["y"]["dimension"])
        self.assertEqual(len(dimension["dimensions"]), 2)  # 2D array
        self.assertEqual(dimension["dimensions"][0], create_dimension_expr(1, 2))
        self.assertEqual(dimension["dimensions"][1], create_dimension_expr(1, 2))
        self.assertTrue("RESHAPE" in variables["y"]["initial_value"].upper())        
        self.assertEqual(variables["y"]["polymorphism_type"], PolymorphismType.NONE)

        # Check parameter array
        self.assertEqual(parameters["arr"]["type"], "INTEGER")
        self.assertEqual(parameters["arr"]["type"], "INTEGER")
        dimension = cast(Dimension, parameters["arr"]["dimension"])
        self.assertEqual(len(dimension["dimensions"]), 1)  # 1D array
        self.assertEqual(dimension["dimensions"][0], create_dimension_expr(1, 3))
        self.assertEqual(parameters["arr"]["value"], "[1, 2, 3]")
        self.assertEqual(parameters["arr"]["polymorphism_type"], PolymorphismType.NONE)

        # Check matrix parameter
        self.assertEqual(parameters["matrix"]["type"], "REAL")
        self.assertTrue("RESHAPE" in parameters["matrix"]["value"].upper())
        self.assertEqual(parameters["matrix"]["polymorphism_type"], PolymorphismType.NONE)

    def test_multiple_array_declarations(self):
        """Test multiple array declarations on same line."""
        self.fs.create_file(
            "/fake/path/multiple_arrays.f90",
            contents="""
    module multiple_arrays
        implicit none
        
        ! Multiple declarations with mixed dimensions
        real :: x(10, 20), y, z(5)
        
        ! Multiple arrays with same dimensions
        integer :: a(5,5), b(5,5), c(5,5)
        
        ! Mixed scalar and array declarations
        real :: scalar1, array1(3), scalar2, array2(2,2)
        
    end module multiple_arrays
            """
        )
        
        result = extract_module_data([Path("/fake/path/multiple_arrays.f90")])
        module_data = result[0]
        variables = module_data["variables"]
        
        # Check mixed dimensions declaration
        self.assertEqual(variables["x"]["type"], "REAL")
        dimension = cast(Dimension, variables["x"]["dimension"])
        self.assertEqual(dimension["dimensions"][0], create_dimension_expr(1, 10))
        self.assertEqual(dimension["dimensions"][1], create_dimension_expr(1, 20))
        self.assertEqual(variables["x"]["polymorphism_type"], PolymorphismType.NONE)
        
        self.assertEqual(variables["y"]["polymorphism_type"], PolymorphismType.NONE)
        
        self.assertEqual(variables["z"]["type"], "REAL")
        dimension = cast(Dimension, variables["z"]["dimension"])
        self.assertEqual(dimension["dimensions"][0], create_dimension_expr(1, 5))
        self.assertEqual(variables["z"]["polymorphism_type"], PolymorphismType.NONE)
        
        # Check same dimensions
        for var in ["a", "b", "c"]:
            self.assertEqual(variables[var]["type"], "INTEGER")
            dimension = cast(Dimension, variables[var]["dimension"])
            self.assertEqual(dimension["dimensions"][0], create_dimension_expr(1, 5))
            self.assertEqual(dimension["dimensions"][1], create_dimension_expr(1, 5))
            self.assertEqual(variables[var]["polymorphism_type"], PolymorphismType.NONE)

        # Check mixed scalar and array
        self.assertEqual(variables["scalar1"]["type"], "REAL")
        self.assertIsNone(variables["scalar1"].get("dimension"))
        self.assertEqual(variables["scalar1"]["polymorphism_type"], PolymorphismType.NONE)
        
        self.assertEqual(variables["array1"]["type"], "REAL")
        dimension = cast(Dimension, variables["array1"]["dimension"])
        self.assertEqual(len(dimension["dimensions"]), 1)
        self.assertEqual(variables["array1"]["polymorphism_type"], PolymorphismType.NONE)

    def test_multiple_arrays_same_attributes(self):
        """Test multiple arrays sharing dimension attribute."""
        self.fs.create_file(
            "/fake/path/shared_dimensions.f90",
            contents="""
    module shared_dimensions
        implicit none
        
        ! Multiple arrays with shared dimension attribute
        real, dimension(10,10) :: x, y, z
        
        ! Multiple arrays with different shared attributes
        integer, dimension(5), parameter :: arr1 = 1, arr2 = 2, arr3 = 3
        
        ! Complex shared attributes
        real, dimension(:,:), allocatable :: matrix1, matrix2, matrix3
        
    end module shared_dimensions
            """
        )
        
        result = extract_module_data([Path("/fake/path/shared_dimensions.f90")])
        module_data = result[0]
        variables = module_data["variables"]
        parameters = module_data["parameters"]
        
        # Check shared dimension attribute
        expected_dims = [
            create_dimension_expr(1, 10),
            create_dimension_expr(1, 10)
        ]
        
        for var_name in ["x", "y", "z"]:
            self.assertEqual(variables[var_name]["type"], "REAL")
            dimension = cast(Dimension, variables[var_name]["dimension"])
            self.assertEqual(dimension["dimensions"], expected_dims)
            self.assertEqual(variables[var_name]["polymorphism_type"], PolymorphismType.NONE)
        
        # Check shared parameter arrays
        for i, arr_name in enumerate(["arr1", "arr2", "arr3"], 1):
            self.assertEqual(parameters[arr_name]["type"], "INTEGER")
            dimension = cast(Dimension, parameters[arr_name]["dimension"])
            self.assertEqual(dimension["dimensions"][0], create_dimension_expr(1, 5))
            self.assertEqual(parameters[arr_name]["value"], str(i))
            self.assertEqual(parameters[arr_name]["polymorphism_type"], PolymorphismType.NONE)
        
        # Check allocatable arrays
        for var_name in ["matrix1", "matrix2", "matrix3"]:
            self.assertEqual(variables[var_name]["type"], "REAL")
            dimension = cast(Dimension, variables[var_name]["dimension"])
            # Allocatable dimensions might need special handling
            self.assertEqual(len(dimension["dimensions"]), 2)
            self.assertIn("ALLOCATABLE", [attr.upper() for attr in variables[var_name]["attributes"]])
            self.assertEqual(variables[var_name]["polymorphism_type"], PolymorphismType.NONE)

    def test_allocatable_arrays(self):
        """Test allocatable array declarations."""
        self.fs.create_file(
            "/fake/path/allocatable.f90",
            contents="""
    module allocatable_arrays
        implicit none
        
        ! 1D allocatable
        real, allocatable :: x(:)
        
        ! 2D allocatable
        real, allocatable :: matrix(:,:)
        
        ! 3D allocatable
        real, allocatable :: cube(:,:,:)
        
        ! Multiple allocatable arrays
        real, allocatable :: a(:), b(:), c(:)
        
    end module allocatable_arrays
            """
        )
        
        result = extract_module_data([Path("/fake/path/allocatable.f90")])
        module_data = result[0]
        variables = module_data["variables"]
        
        # Check 1D allocatable
        self.assertEqual(variables["x"]["type"], "REAL")
        self.assertIn("ALLOCATABLE", [attr.upper() for attr in variables["x"]["attributes"]])
        dimension = cast(Dimension, variables["x"]["dimension"])
        self.assertEqual(len(dimension["dimensions"]), 1)
        self.assertEqual(dimension["dimensions"][0].bound_type, BoundType.ASSUMED_SHAPE)
        self.assertEqual(variables["x"]["polymorphism_type"], PolymorphismType.NONE)
        
        # Check 2D allocatable
        dimension = cast(Dimension, variables["matrix"]["dimension"])
        self.assertEqual(len(dimension["dimensions"]), 2)
        for i in range(2):
            self.assertEqual(dimension["dimensions"][i].bound_type, BoundType.ASSUMED_SHAPE)
        self.assertEqual(variables["matrix"]["polymorphism_type"], PolymorphismType.NONE)
            
        # Check 3D allocatable
        dimension = cast(Dimension, variables["cube"]["dimension"])
        self.assertEqual(len(dimension["dimensions"]), 3)
        self.assertEqual(variables["cube"]["polymorphism_type"], PolymorphismType.NONE)
        
        # Check multiple allocatable arrays
        for var_name in ["a", "b", "c"]:
            self.assertIn("ALLOCATABLE", [attr.upper() for attr in variables[var_name]["attributes"]])
            dimension = cast(Dimension, variables[var_name]["dimension"])
            self.assertEqual(len(dimension["dimensions"]), 1)
            self.assertEqual(variables[var_name]["polymorphism_type"], PolymorphismType.NONE)


    def test_complex_array_expressions(self):
        """Test arrays with complex dimension expressions."""
        self.fs.create_file(
            "/fake/path/complex_dims.f90",
            contents="""
    module complex_dims
        implicit none
        
        integer :: n = 10
        integer :: k = 2
        
        ! Variable dimensions
        integer :: arr1(n)
        
        ! Expression in dimension
        real :: arr2(2*k)
        
        ! Function call in dimension
        real :: arr3(max(n,k))
        
        ! Multiple complex dimensions
        real :: matrix(n, 2*n)
        
        ! Expressions in bounds
        real :: bounded(2:n+1)
        
        contains
        
        function get_size() result(size)
            integer :: size
            size = 20
        end function get_size
        
    end module complex_dims
            """
        )
        
        result = extract_module_data([Path("/fake/path/complex_dims.f90")])
        module_data = result[0]
        variables = module_data["variables"]
        
        # Check variable dimension
        dimension = cast(Dimension, variables["arr1"]["dimension"])
        self.assertEqual(dimension["dimensions"][0].upper.value, "n")
        self.assertEqual(variables["arr1"]["polymorphism_type"], PolymorphismType.NONE)
        
        # Check expression in dimension
        dimension = cast(Dimension, variables["arr2"]["dimension"])
        self.assertEqual(dimension["dimensions"][0].upper.value, "2 * k")
        self.assertEqual(variables["arr2"]["polymorphism_type"], PolymorphismType.NONE)
        
        # Check function call in dimension
        dimension = cast(Dimension, variables["arr3"]["dimension"])
        self.assertTrue("MAX" in dimension["dimensions"][0].upper.value.upper())
        self.assertEqual(variables["arr3"]["polymorphism_type"], PolymorphismType.NONE)
        
        # Check bounded array
        dimension = cast(Dimension, variables["bounded"]["dimension"])
        self.assertEqual(dimension["dimensions"][0].lower.value, "2")
        self.assertEqual(dimension["dimensions"][0].upper.value, "n + 1")
        self.assertEqual(variables["bounded"]["polymorphism_type"], PolymorphismType.NONE)

    def test_character_arrays(self):
        """Test character array declarations."""
        self.fs.create_file(
            "/fake/path/char_arrays.f90",
            contents="""
    module char_arrays
        implicit none
        
        ! Character array with fixed length
        character(len=10) :: names1(100)
        
        ! Alternative syntax
        character(10) :: names2(5,10)
        
        ! Using dimension attribute
        character(len=20), dimension(50) :: strings
        
        ! Assumed length character array (typically in dummy arguments)
        character(len=*), parameter :: fixed_strings(3) = ["Hello", "World", "Test "]
        
        ! Deferred length allocatable character array
        character(len=:), allocatable :: flex_string(:)
        
        ! Character array initialization
        character(10) :: greetings(2) = (/'Hello     ', 'World     '/)
        
    end module char_arrays
            """
        )
        
        result = extract_module_data([Path("/fake/path/char_arrays.f90")])
        module_data = result[0]
        variables = module_data["variables"]
        parameters = module_data["parameters"]
        
        # Check fixed length character array
        self.assertEqual(variables["names1"]["type"], "CHARACTER")
        self.assertEqual(variables["names1"]["length"], "10")
        dimension = cast(Dimension, variables["names1"]["dimension"])
        self.assertEqual(dimension["dimensions"][0], create_dimension_expr(1, 100))
        self.assertEqual(variables["names1"]["polymorphism_type"], PolymorphismType.NONE)
        
        # Check alternative syntax
        self.assertEqual(variables["names2"]["type"], "CHARACTER")
        self.assertEqual(variables["names2"]["length"], "10")
        dimension = cast(Dimension, variables["names2"]["dimension"])
        self.assertEqual(dimension["dimensions"][0], create_dimension_expr(1, 5))
        self.assertEqual(dimension["dimensions"][1], create_dimension_expr(1, 10))
        self.assertEqual(variables["names2"]["polymorphism_type"], PolymorphismType.NONE)
        
        # Check dimension attribute
        self.assertEqual(variables["strings"]["type"], "CHARACTER")
        self.assertEqual(variables["strings"]["length"], "20")
        dimension = cast(Dimension, variables["strings"]["dimension"])
        self.assertEqual(dimension["dimensions"][0], create_dimension_expr(1, 50))
        self.assertEqual(variables["strings"]["polymorphism_type"], PolymorphismType.NONE)
        
        # Check assumed length parameter
        self.assertEqual(parameters["fixed_strings"]["type"], "CHARACTER")
        self.assertEqual(parameters["fixed_strings"]["length"], "*")
        self.assertEqual(parameters["fixed_strings"]["polymorphism_type"], PolymorphismType.NONE)
        
        # Check deferred length allocatable
        self.assertEqual(variables["flex_string"]["type"], "CHARACTER")
        self.assertEqual(variables["flex_string"]["length"], ":")
        self.assertIn("ALLOCATABLE", [attr.upper() for attr in variables["flex_string"]["attributes"]])
        self.assertEqual(variables["flex_string"]["polymorphism_type"], PolymorphismType.NONE)
        
        # Check initialization
        self.assertEqual(variables["greetings"]["type"], "CHARACTER")
        self.assertEqual(variables["greetings"]["initial_value"], "(/'Hello     ', 'World     '/)")
        self.assertEqual(variables["greetings"]["polymorphism_type"], PolymorphismType.NONE)

    def test_pointer_and_target_arrays(self):
        """Test pointer and target array attributes."""
        self.fs.create_file(
            "/fake/path/pointer_target.f90",
            contents="""
    module pointer_target
        implicit none
        
        ! Pointer array
        real, pointer :: ptr_array(:,:)
        
        ! Target array
        real, target :: target_array(10,20)
        
        ! Combined attributes
        real, pointer, dimension(10,10) :: ptr_with_dims
        
        ! Multiple attributes
        real, allocatable, target :: alloc_target(:)
        
    end module pointer_target
            """
        )
        
        result = extract_module_data([Path("/fake/path/pointer_target.f90")])
        module_data = result[0]
        variables = module_data["variables"]
        
        # Check pointer array
        self.assertIn("POINTER", [attr.upper() for attr in variables["ptr_array"]["attributes"]])
        dimension = cast(Dimension, variables["ptr_array"]["dimension"])
        self.assertEqual(len(dimension["dimensions"]), 2)
        self.assertEqual(variables["ptr_array"]["polymorphism_type"], PolymorphismType.NONE)
        
        # Check target array
        self.assertIn("TARGET", [attr.upper() for attr in variables["target_array"]["attributes"]])
        dimension = cast(Dimension, variables["target_array"]["dimension"])
        self.assertEqual(dimension["dimensions"][0], create_dimension_expr(1, 10))
        self.assertEqual(dimension["dimensions"][1], create_dimension_expr(1, 20))
        self.assertEqual(variables["target_array"]["polymorphism_type"], PolymorphismType.NONE)
        
        # Check combined attributes
        self.assertIn("POINTER", [attr.upper() for attr in variables["ptr_with_dims"]["attributes"]])
        dimension = cast(Dimension, variables["ptr_with_dims"]["dimension"])
        self.assertEqual(len(dimension["dimensions"]), 2)
        self.assertEqual(variables["ptr_with_dims"]["polymorphism_type"], PolymorphismType.NONE)
        
        # Check multiple attributes
        attrs = [attr.upper() for attr in variables["alloc_target"]["attributes"]]
        self.assertIn("ALLOCATABLE", attrs)
        self.assertIn("TARGET", attrs)
        self.assertEqual(variables["alloc_target"]["polymorphism_type"], PolymorphismType.NONE)

    def test_fortran77_style(self):
        self.fs.create_file(
            "/fake/path/fortran77.f77",
            contents="""
      MODULE FORTRAN77
      IMPLICIT NONE

C     Old-style dimension statement
      DIMENSION X(10)
      DIMENSION Y(20), Z(5,5)
      REAL X, Y
      INTEGER Z

C     Old-style character declarations
      CHARACTER*20 NAME
      CHARACTER*10 TITLES(5)

C     Real*8 syntax
      REAL*8 DPVAL
      REAL*4 SPVAL

C     Integer*2 and Integer*4
      INTEGER*2 SHORTINT
      INTEGER*4 LONGINT

C     Complex*16
      COMPLEX*16 DCMPLX

C     Logical*1
      LOGICAL*1 FLAG

C     Arrays with old-style declarations
      REAL*8 MATRIX(10,10)
      INTEGER*4 INDICES(100)

C     Common blocks with arrays
      COMMON /BLOCK1/ A(5), B(10), C
      REAL A, B, C

C     Fixed format array initialization
      INTEGER IARRAY(5)
      DATA IARRAY /1, 2, 3, 4, 5/

C     Character array with dimension
      DIMENSION NAMES(10)
      CHARACTER*30 NAMES

C     Equivalence statement (less common)
      DIMENSION BUFFER(1000)
      REAL BUFFER
      REAL WORK(100)
      EQUIVALENCE (BUFFER(1), WORK(1))

      END MODULE FORTRAN77
        """
        )
        
        result = extract_module_data([Path("/fake/path/fortran77.f77")])
        module_data = result[0]
        variables = module_data["variables"]
        
        # Check DIMENSION statement variables
        self.assertEqual(variables["X"]["type"], "REAL")
        dimension = cast(Dimension, variables["X"]["dimension"])
        self.assertEqual(dimension["dimensions"][0], create_dimension_expr(1, 10))
        
        self.assertEqual(variables["Y"]["type"], "REAL")
        dimension = cast(Dimension, variables["Y"]["dimension"])
        self.assertEqual(dimension["dimensions"][0], create_dimension_expr(1, 20))
        
        self.assertEqual(variables["Z"]["type"], "INTEGER")
        dimension = cast(Dimension, variables["Z"]["dimension"])
        self.assertEqual(dimension["dimensions"][0], create_dimension_expr(1, 5))
        self.assertEqual(dimension["dimensions"][1], create_dimension_expr(1, 5))
        
        # Check old-style character declarations
        self.assertEqual(variables["NAME"]["type"], "CHARACTER")
        self.assertEqual(variables["NAME"]["length"], "20")
        
        self.assertEqual(variables["TITLES"]["type"], "CHARACTER")
        self.assertEqual(variables["TITLES"]["length"], "10")
        dimension = cast(Dimension, variables["TITLES"]["dimension"])
        self.assertEqual(dimension["dimensions"][0], create_dimension_expr(1, 5))
        
        # Check REAL*8 and other * syntax
        self.assertEqual(variables["DPVAL"]["type"], "REAL")
        self.assertEqual(variables["DPVAL"]["kind"], "8")
        
        self.assertEqual(variables["SPVAL"]["type"], "REAL")
        self.assertEqual(variables["SPVAL"]["kind"], "4")
        
        self.assertEqual(variables["SHORTINT"]["type"], "INTEGER")
        self.assertEqual(variables["SHORTINT"]["kind"], "2")
        
        self.assertEqual(variables["LONGINT"]["type"], "INTEGER")
        self.assertEqual(variables["LONGINT"]["kind"], "4")
        
        self.assertEqual(variables["DCMPLX"]["type"], "COMPLEX")
        self.assertEqual(variables["DCMPLX"]["kind"], "16")
        
        self.assertEqual(variables["FLAG"]["type"], "LOGICAL")
        self.assertEqual(variables["FLAG"]["kind"], "1")
        
        # Check arrays with old-style type declarations
        self.assertEqual(variables["MATRIX"]["type"], "REAL")
        self.assertEqual(variables["MATRIX"]["kind"], "8")
        dimension = cast(Dimension, variables["MATRIX"]["dimension"])
        self.assertEqual(dimension["dimensions"][0], create_dimension_expr(1, 10))
        self.assertEqual(dimension["dimensions"][1], create_dimension_expr(1, 10))
        
        # Check character array with separate DIMENSION
        self.assertEqual(variables["NAMES"]["type"], "CHARACTER")
        self.assertEqual(variables["NAMES"]["length"], "30")
        dimension = cast(Dimension, variables["NAMES"]["dimension"])
        self.assertEqual(dimension["dimensions"][0], create_dimension_expr(1, 10))
        
        # Check DATA statement initialization
        self.assertEqual(variables["IARRAY"]["type"], "INTEGER")
        self.assertEqual(variables["IARRAY"]["initial_value"], "/1, 2, 3, 4, 5/")

        # Check common block variables
        #TODO 
        self.assertIn("A", variables)
        self.assertIn("B", variables)
        self.assertIn("C", variables)

    def test_fortran77_fixed_format(self):
        """Test FORTRAN 77 fixed format with column restrictions."""
        self.fs.create_file(
            "/fake/path/fixed_format.f",  # Note: .f extension for fixed format
            contents="""
C23456789012345678901234567890123456789012345678901234567890123456789072
      MODULE FIXEDFORM
      IMPLICIT NONE
C
C     Comment in column 1
C
      REAL ARRAY(10,20)
      INTEGER I,J,K
      INTEGER NMAX
      PARAMETER (NMAX=100)
      REAL WORK(NMAX)
C
C     Continuation lines
      REAL LONGNAMEDVARIABLE1, LONGNAMEDVARIABLE2,
     &     LONGNAMEDVARIABLE3, LONGNAMEDVARIABLE4
C
      REAL PI, E
C
C     Character variables
      CHARACTER*72 LINE
      CHARACTER*8 WORD(10)
C
C     Old-style parameter statement with the type declared elsewhere
      PARAMETER (PI=3.14159, E=2.71828)
      END MODULE FIXEDFORM
            """
        )
        
        result = extract_module_data([Path("/fake/path/fixed_format.f")])
        module_data = result[0]
        variables = module_data["variables"]
        parameters = module_data["parameters"]
        
        # Check array declaration
        self.assertEqual(variables["ARRAY"]["type"], "REAL")
        dimension = cast(Dimension, variables["ARRAY"]["dimension"])
        self.assertEqual(dimension["dimensions"][0], create_dimension_expr(1, 10))
        self.assertEqual(dimension["dimensions"][1], create_dimension_expr(1, 20))
        
        # Check PARAMETER statement
        self.assertEqual(parameters["NMAX"]["type"], "INTEGER")
        self.assertEqual(parameters["NMAX"]["value"], "100")
        
        self.assertEqual(parameters["PI"]["type"], "REAL")
        self.assertEqual(parameters["PI"]["value"], "3.14159")
        
        # Check continuation lines
        for var in ["LONGNAMEDVARIABLE1", "LONGNAMEDVARIABLE2", 
                    "LONGNAMEDVARIABLE3", "LONGNAMEDVARIABLE4"]:
            self.assertIn(var, variables)
            self.assertEqual(variables[var]["type"], "REAL")

    def test_fortran77_character_arrays(self):
        """Test FORTRAN 77 style character array declarations."""
        self.fs.create_file(
            "/fake/path/char77.f90",
            contents="""
    module char77
        implicit none
        
        ! Old-style character array declaration
        character*10 names(100)
        
        ! Alternative character array syntax
        character*20 titles(5,10)
        
        ! Character with dimension attribute
        character*30 descriptions(50)
        
        ! Mixed declaration
        character*15 labels(25), tag
        
    end module char77
            """
        )
        
        result = extract_module_data([Path("/fake/path/char77.f90")])
        module_data = result[0]
        variables = module_data["variables"]
        
        # Check old-style character array
        self.assertEqual(variables["names"]["type"], "CHARACTER")
        self.assertEqual(variables["names"]["length"], "10")
        dimension = cast(Dimension, variables["names"]["dimension"])
        self.assertEqual(dimension["dimensions"][0], create_dimension_expr(1, 100))
        
        # Check 2D character array
        self.assertEqual(variables["titles"]["type"], "CHARACTER")
        self.assertEqual(variables["titles"]["length"], "20")
        dimension = cast(Dimension, variables["titles"]["dimension"])
        self.assertEqual(len(dimension["dimensions"]), 2)

    def test_array_initialization_patterns(self):
        """Test various array initialization patterns."""
        self.fs.create_file(
            "/fake/path/array_init.f90",
            contents="""
    module array_init
        implicit none
        
        ! Implied do loop initialization
        real :: x(10) = (/ (real(i), i=1,10) /)
        
        ! Nested implied do loops
        real :: matrix(3,3) = reshape((/ ((real(i+j), i=1,3), j=1,3) /), [3,3])
        
        ! Array constructor with expressions
        integer, parameter :: arr(5) = [(i**2, i=1,5)]
        
        ! Character array initialization
        character(10) :: words(3) = (/ 'Hello     ', 'World     ', 'Fortran   ' /)
        
        ! Complex initialization
        complex :: c_arr(3) = [(cmplx(i, i+1), i=1,3)]
        
    end module array_init
            """
        )
        
        result = extract_module_data([Path("/fake/path/array_init.f90")])
        module_data = result[0]
        variables = module_data["variables"]
        parameters = module_data["parameters"]
        
        # Check implied do loop
        self.assertEqual(variables["x"]["type"], "REAL")
        self.assertIn("i = 1, 10", variables["x"]["initial_value"])
        self.assertEqual(variables["x"]["polymorphism_type"], PolymorphismType.NONE)
        
        # Check nested implied do loops
        self.assertEqual(variables["matrix"]["type"], "REAL")
        self.assertIn("RESHAPE", variables["matrix"]["initial_value"].upper())
        self.assertEqual(variables["matrix"]["polymorphism_type"], PolymorphismType.NONE)
        
        # Check array constructor
        self.assertEqual(parameters["arr"]["type"], "INTEGER")
        self.assertIn("i = 1, 5", parameters["arr"]["value"])
        self.assertEqual(parameters["arr"]["polymorphism_type"], PolymorphismType.NONE)

    def test_derived_type_arrays(self):
        """Test arrays of derived types."""
        self.fs.create_file(
            "/fake/path/derived_arrays.f90",
            contents="""
    module derived_arrays
        implicit none
        
        type :: point
            real :: x, y
        end type point
        
        type :: line
            type(point) :: start_pt, end_pt
        end type line
        
        ! Array of derived type
        type(point) :: points(100)
        
        ! 2D array of derived type
        type(point) :: grid(10,10)
        
        ! Allocatable array of derived type
        type(line), allocatable :: lines(:)
        
        ! Derived type with array component
        type :: vector
            real :: components(3)
        end type vector
        
        type(vector) :: basis(3)
        
    end module derived_arrays
            """
        )
        
        result = extract_module_data([Path("/fake/path/derived_arrays.f90")])
        module_data = result[0]
        variables = module_data["variables"]
        
        # Check array of derived type
        self.assertEqual(variables["points"]["type"], "point")
        dimension = cast(Dimension, variables["points"]["dimension"])
        self.assertEqual(dimension["dimensions"][0], create_dimension_expr(1, 100))
        self.assertEqual(variables["points"]["polymorphism_type"], PolymorphismType.NONE)
        
        # Check 2D array of derived type
        self.assertEqual(variables["grid"]["type"], "point")
        dimension = cast(Dimension, variables["grid"]["dimension"])
        self.assertEqual(len(dimension["dimensions"]), 2)
        self.assertEqual(variables["grid"]["polymorphism_type"], PolymorphismType.NONE)
        
        # Check allocatable array
        self.assertEqual(variables["lines"]["type"], "line")
        self.assertIn("ALLOCATABLE", [attr.upper() for attr in variables["lines"]["attributes"]])
        self.assertEqual(variables["lines"]["polymorphism_type"], PolymorphismType.NONE)

    def test_polymorphic_arrays(self):
        """Test polymorphic arrays with CLASS declarations."""
        self.fs.create_file(
            "/fake/path/polymorphic.f90",
            contents="""
    module polymorphic
        implicit none
        
        type :: shape
            real :: area
        end type shape
        
        type, extends(shape) :: circle
            real :: radius
        end type circle
        
        type, extends(shape) :: rectangle
            real :: length, width
        end type rectangle
        
        ! Polymorphic arrays
        class(shape), allocatable :: shapes(:)
        class(shape), pointer :: shape_ptrs(:)
        
        ! Polymorphic array with dimension attribute
        class(shape), allocatable, dimension(:,:) :: shape_grid
        
        ! Unlimited polymorphic
        class(*), allocatable :: anything(:)
        
    end module polymorphic
            """
        )
        
        result = extract_module_data([Path("/fake/path/polymorphic.f90")])
        module_data = result[0]
        variables = module_data["variables"]
        
        # Check polymorphic allocatable array
        self.assertEqual(variables["shapes"]["type"], "shape")
        self.assertEqual(variables["shapes"]["polymorphism_type"], PolymorphismType.LIMITED)
        self.assertIn("ALLOCATABLE", [attr.upper() for attr in variables["shapes"]["attributes"]])
        
        # Check polymorphic pointer array
        self.assertEqual(variables["shape_ptrs"]["type"], "shape")
        self.assertEqual(variables["shape_ptrs"]["polymorphism_type"], PolymorphismType.LIMITED)
        self.assertIn("POINTER", [attr.upper() for attr in variables["shape_ptrs"]["attributes"]])
        
        # Check unlimited polymorphic
        self.assertEqual(variables["anything"]["type"], "*")
        self.assertEqual(variables["anything"]["polymorphism_type"], PolymorphismType.UNLIMITED)
        self.assertIn("ALLOCATABLE", [attr.upper() for attr in variables["anything"]["attributes"]])

    def test_coarrays(self):
        # coarrays aren't arrays, but I'm just putting this test in here for now
        self.fs.create_file(
            "/fake/path/coarrays.f90",
            contents="""
    module coarrays
        implicit none

        ! Note that only this form is parsed - integer :: scalar[*] doesn't work
        ! Simple coarray
        integer, codimension[*] :: scalar
    
        ! Array with coarray dimension
        real, dimension(10), codimension[*] :: vector
    
        ! Multi-dimensional array with coarray
        real, dimension(10,20), codimension[3,*] :: matrix
    
        ! Allocatable coarray
        real, allocatable, codimension[:,:,:] :: dynamic
    
        ! Coarray with explicit bounds
        integer, codimension[2:5,*] :: fixed_coarray
    
        ! Character coarray
        character(len=10), dimension(5), codimension[*] :: names
        
    end module coarrays
            """
        )
        
        result = extract_module_data([Path("/fake/path/coarrays.f90")])
        module_data = result[0]
        variables = module_data["variables"]
        
        self.assertEqual(variables["scalar"]["type"], "INTEGER")
        self.assertIsNone(variables["scalar"]["dimension"])
        self.assertEqual(variables["scalar"]["polymorphism_type"], PolymorphismType.NONE)

        self.assertEqual(variables["vector"]["type"], "REAL")
        self.assertEqual(len(variables["vector"]["dimension"]["dimensions"]), 1)
        self.assertEqual(variables["vector"]["dimension"]["dimensions"][0], create_dimension_expr(1, 10))
        self.assertEqual(variables["vector"]["polymorphism_type"], PolymorphismType.NONE)
                
        self.assertEqual(variables["matrix"]["type"], "REAL")
        self.assertEqual(len(variables["matrix"]["dimension"]["dimensions"]), 2)
        self.assertEqual(variables["matrix"]["dimension"]["dimensions"][0], create_dimension_expr(1, 10))
        self.assertEqual(variables["matrix"]["dimension"]["dimensions"][1], create_dimension_expr(1, 20))
        self.assertEqual(variables["matrix"]["polymorphism_type"], PolymorphismType.NONE)
        
        self.assertEqual(variables["dynamic"]["type"], "REAL")
        self.assertIsNone(variables["dynamic"]["dimension"])
        self.assertIn("ALLOCATABLE", [attr.upper() for attr in variables["dynamic"]["attributes"]])
        
        self.assertEqual(variables["fixed_coarray"]["type"], "INTEGER")
        self.assertIsNone(variables["fixed_coarray"]["dimension"])
        
        self.assertEqual(variables["names"]["type"], "CHARACTER")
        self.assertEqual(len(variables["names"]["dimension"]["dimensions"]), 1)
        self.assertEqual(variables["names"]["dimension"]["dimensions"][0], create_dimension_expr(1, 5))
        self.assertEqual(variables["names"]["length"], "10")
                
    def test_array_edge_cases(self):
        """Test edge cases and unusual array declarations."""
        self.fs.create_file(
            "/fake/path/edge_cases.f90",
            contents="""
    module edge_cases
        implicit none
        
        ! Very large dimensions
        real :: huge_array(1000000)
        
        ! Negative indices
        integer :: centered(-10:10)
        real :: offset_matrix(-5:5, -3:3)
        
        ! Arrays with complex expressions
        integer, parameter :: n = 10
        real :: computed(2*n+1, n**2)
        
        ! Mixed array and scalar declarations
        real :: a, b(10), c, d(5,5), e
        
        ! Array with many dimensions (up to 15 allowed in Fortran 2008)
        real :: tensor_5d(2,2,2,2,2)
        
        ! Character arrays with variable length
        character(len=:), allocatable :: var_strings(:)
        
        ! Arrays in common blocks
        common /data/ arr1(100), arr2(50,50)
        real :: arr1, arr2, arr3
        
    end module edge_cases
            """
        )
        
        result = extract_module_data([Path("/fake/path/edge_cases.f90")])
        module_data = result[0]
        variables = module_data["variables"]
        
        # Check very large dimensions
        self.assertEqual(variables["huge_array"]["type"], "REAL")
        self.assertEqual(variables["huge_array"]["polymorphism_type"], PolymorphismType.NONE)
        dimension = cast(Dimension, variables["huge_array"]["dimension"])
        self.assertEqual(dimension["dimensions"][0], create_dimension_expr(1, 1000000))
        
        # Check negative indices
        self.assertEqual(variables["centered"]["type"], "INTEGER")
        self.assertEqual(variables["centered"]["polymorphism_type"], PolymorphismType.NONE)
        dimension = cast(Dimension, variables["centered"]["dimension"])
        self.assertEqual(dimension["dimensions"][0], create_dimension_expr(-10, 10))
        
        # Check offset matrix with negative indices
        self.assertEqual(variables["offset_matrix"]["type"], "REAL")
        self.assertEqual(variables["offset_matrix"]["polymorphism_type"], PolymorphismType.NONE)
        dimension = cast(Dimension, variables["offset_matrix"]["dimension"])
        self.assertEqual(dimension["dimensions"][0], create_dimension_expr(-5, 5))
        self.assertEqual(dimension["dimensions"][1], create_dimension_expr(-3, 3))
        
        # Check complex expressions
        self.assertEqual(variables["computed"]["type"], "REAL")
        self.assertEqual(variables["computed"]["polymorphism_type"], PolymorphismType.NONE)
        dimension = cast(Dimension, variables["computed"]["dimension"])
        self.assertEqual(dimension["dimensions"][0].upper.value, "2 * n + 1")
        self.assertEqual(dimension["dimensions"][1].upper.value, "n ** 2")
        
        # Check mixed declarations
        self.assertIn("a", variables)
        self.assertEqual(variables["a"]["type"], "REAL")
        self.assertEqual(variables["a"]["polymorphism_type"], PolymorphismType.NONE)
        self.assertIsNone(variables["a"].get("dimension"))
        
        self.assertIn("b", variables)
        self.assertEqual(variables["b"]["type"], "REAL")
        self.assertEqual(variables["b"]["polymorphism_type"], PolymorphismType.NONE)
        self.assertIsNotNone(variables["b"].get("dimension"))
        dimension = cast(Dimension, variables["b"]["dimension"])
        self.assertEqual(dimension["dimensions"][0], create_dimension_expr(1, 10))
        
        self.assertIn("c", variables)
        self.assertEqual(variables["c"]["type"], "REAL")
        self.assertEqual(variables["c"]["polymorphism_type"], PolymorphismType.NONE)
        self.assertIsNone(variables["c"].get("dimension"))
        
        self.assertIn("d", variables)
        self.assertEqual(variables["d"]["type"], "REAL")
        self.assertEqual(variables["d"]["polymorphism_type"], PolymorphismType.NONE)
        self.assertIsNotNone(variables["d"].get("dimension"))
        dimension = cast(Dimension, variables["d"]["dimension"])
        self.assertEqual(dimension["dimensions"][0], create_dimension_expr(1, 5))
        self.assertEqual(dimension["dimensions"][1], create_dimension_expr(1, 5))
        
        self.assertIn("e", variables)
        self.assertEqual(variables["e"]["type"], "REAL")
        self.assertEqual(variables["e"]["polymorphism_type"], PolymorphismType.NONE)
        self.assertIsNone(variables["e"].get("dimension"))
        
        # Check 5D tensor
        self.assertEqual(variables["tensor_5d"]["type"], "REAL")
        self.assertEqual(variables["tensor_5d"]["polymorphism_type"], PolymorphismType.NONE)
        dimension = cast(Dimension, variables["tensor_5d"]["dimension"])
        self.assertEqual(len(dimension["dimensions"]), 5)
        for i in range(5):
            self.assertEqual(dimension["dimensions"][i], create_dimension_expr(1, 2))
        
        # Check variable length character array
        self.assertEqual(variables["var_strings"]["type"], "CHARACTER")
        self.assertEqual(variables["var_strings"]["polymorphism_type"], PolymorphismType.NONE)
        self.assertIn("ALLOCATABLE", [attr.upper() for attr in variables["var_strings"]["attributes"]])
        dimension = cast(Dimension, variables["var_strings"]["dimension"])
        self.assertEqual(len(dimension["dimensions"]), 1)
        self.assertEqual(dimension["dimensions"][0].bound_type, BoundType.ASSUMED_SHAPE)
        self.assertIsNone(dimension["dimensions"][0].lower)
        self.assertIsNone(dimension["dimensions"][0].upper)
        self.assertEqual(variables["var_strings"]["length"], ":")  # Variable length
        
        # Check arrays in common blocks
        self.assertEqual(variables["arr1"]["type"], "REAL")
        self.assertEqual(variables["arr1"]["polymorphism_type"], PolymorphismType.NONE)
        dimension = variables["arr1"]["dimension"]
        self.assertEqual(dimension["dimensions"][0], create_dimension_expr(1, 100))
        
        self.assertEqual(variables["arr2"]["type"], "REAL")
        self.assertEqual(variables["arr2"]["polymorphism_type"], PolymorphismType.NONE)
        dimension = cast(Dimension, variables["arr2"]["dimension"])
        self.assertEqual(dimension["dimensions"][0], create_dimension_expr(1, 50))
        self.assertEqual(dimension["dimensions"][1], create_dimension_expr(1, 50))
        
        
if __name__ == "__main__":
    unittest.main()
