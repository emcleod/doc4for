import unittest
from pathlib import Path
from typing import cast
from pyfakefs.fake_filesystem_unittest import TestCase
from doc4for.models.common import Expression, ExpressionType
from doc4for.models.dimension_models import ArrayBound, BoundType, Dimension
from doc4for.f90.generate_module_tree import extract_module_data

# Helper function for creating dimension expressions
def create_dimension_expr(lower, upper):
    return ArrayBound(
        bound_type=BoundType.FIXED,
        lower=Expression(expr_type=ExpressionType.LITERAL, value=str(lower), function_name=None, arguments=None),
        upper=Expression(expr_type=ExpressionType.LITERAL, value=str(upper), function_name=None, arguments=None)
    )

class TestParameterDeclarations(TestCase):
    maxDiff=None

    def setUp(self):
        self.setUpPyfakefs()

    def test_find_constants(self):
        # Create a fake Fortran file
        self.fs.create_file(
            "/fake/path/constants.f90",
            contents="""\
            
    !!*
    ! Module containing constants
    !*!
    module simple

        ! Constants
        real, parameter :: PI = 3.14159
    end module simple
            """,
        )
        result = extract_module_data([Path("/fake/path/constants.f90")])

        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(module["module_name"], "simple")
        self.assertEqual(module["file_name"], "/fake/path/constants.f90")
        self.assertEqual(
            module["module_description"], "Module containing constants\n"
        )
        self.assertIn("PI", module["parameters"])
        self.assertEqual(module["parameters"]["PI"]["value"], "3.14159")
        self.assertEqual(module["parameters"]["PI"]["type"], "REAL")

    def test_parameters_defined_in_module(self):
        self.fs.create_file(
            "/fake/path/parameters.f90",
            contents="""\
module global_parameters_module
    implicit none

    !!* Defines pi *!
    real, parameter :: PI = 3.14159265358979323846
    ! No comment 
    integer, parameter :: MAX_ITERATIONS = 1000

    ! Rest of the program code

end module 
""")
        result = extract_module_data([Path("/fake/path/parameters.f90")])
        self.assertEqual(len(result), 1)
        module_data = result[0]
        self.assertEqual(module_data["file_name"], "/fake/path/parameters.f90")
        self.assertEqual(len(module_data["parameters"]), 2)
        self.assertIn("PI", module_data["parameters"])
        pi_info = module_data["parameters"]["PI"]
        self.assertEqual(pi_info["type"], "REAL")
        self.assertEqual(pi_info["name"], "PI")
        self.assertEqual(pi_info["value"], "3.14159265358979323846")
        self.assertIn("MAX_ITERATIONS", module_data["parameters"])
        max_iter_info = module_data["parameters"]["MAX_ITERATIONS"]
        self.assertEqual(max_iter_info["type"], "INTEGER")
        self.assertEqual(max_iter_info["name"], "MAX_ITERATIONS")
        self.assertEqual(max_iter_info["value"], "1000")
        self.assertEqual(pi_info["description"], "Defines pi\n")
        self.assertEqual(max_iter_info["description"], "")

    def test_simple_declarations(self):
        self.fs.create_file("/fake/path/module.f90",
                            contents="""\
    module test_mod
        implicit none
        
        ! Named constants (parameters)
        integer, parameter :: const_i = 42
        real, parameter :: pi = 3.14159
        character(len=*), parameter :: name = "Test"

    end module test_mod
    """)
        result = extract_module_data([Path("/fake/path/module.f90")])

        self.assertEqual(len(result), 1)
        module_data = result[0]
        self.assertEqual(module_data["module_name"], "test_mod")

        parameters = module_data["parameters"]
        self.assertEqual(len(parameters), 3)

        self.assertEqual(parameters["const_i"]["type"], "INTEGER")
        self.assertEqual(parameters["const_i"]["value"], "42")

        self.assertEqual(parameters["pi"]["type"], "REAL")
        self.assertEqual(parameters["pi"]["value"], "3.14159")

        self.assertEqual(parameters["name"]["type"], "CHARACTER")
        self.assertEqual(parameters["name"]["length"], "*")  # len=* case
        self.assertEqual(parameters["name"]["value"], '"Test"')

    def test_array_declarations(self):
        self.fs.create_file("/fake/path/module.f90",
                            contents="""\
    module test_mod
        implicit none

        ! Fixed size arrays with different syntaxes
        integer, parameter :: arr1(5) = [1, 2, 3, 4, 5]              ! Modern syntax
        integer, parameter :: arr2(3) = (/1, 2, 3/)                  ! Old syntax
        real, parameter :: arr3(4) = [1.0, 2*2.0, 1.0]              ! With repeat
        
        ! Multi-dimensional arrays
        real, parameter :: matrix(2,3) = reshape([1.0, 2.0, 3.0, 4.0, 5.0, 6.0], [3,2])
        
        ! Arrays with explicit bounds
        integer, parameter :: explicit(-1:1) = [-1, 0, 1]
        
        ! Array constructors with operations
        real, parameter :: computed(3) = [1.0, 1.5, 2.0] * 2.0
                
        ! Character arrays
        character(len=5), parameter :: str_arr(2) = ["Hello", "World"]

    end module test_mod
    """)
        result = extract_module_data([Path("/fake/path/module.f90")])

        # Check basic structure
        self.assertEqual(len(result), 1)
        module_data = result[0]
        self.assertEqual(module_data["module_name"], "test_mod")

        parameters = module_data["parameters"]
        self.assertEqual(len(parameters), 7)

        # Check simple array with modern syntax
        self.assertEqual(parameters["arr1"]["type"], "INTEGER")
        dimension = cast(Dimension, parameters["arr1"]["dimension"])
        self.assertEqual(dimension["dimensions"][0], create_dimension_expr(1, 5))
        self.assertEqual(parameters["arr1"]["value"], "[1, 2, 3, 4, 5]")

        # Check array with old syntax
        self.assertEqual(parameters["arr2"]["type"], "INTEGER")
        dimension = cast(Dimension, parameters["arr2"]["dimension"])
        self.assertEqual(dimension["dimensions"][0], create_dimension_expr(1, 3))
        self.assertEqual(parameters["arr2"]["value"], "(/1, 2, 3/)")

        # Check array with repeat syntax
        self.assertEqual(parameters["arr3"]["type"], "REAL")
        dimension = cast(Dimension, parameters["arr3"]["dimension"])
        self.assertEqual(dimension["dimensions"][0], create_dimension_expr(1, 4))
        self.assertEqual(parameters["arr3"]["value"], "[1.0, 2 * 2.0, 1.0]")

        # Check multi-dimensional array
        self.assertEqual(parameters["matrix"]["type"], "REAL")
        dimension = cast(Dimension, parameters["matrix"]["dimension"])
        self.assertEqual(dimension["dimensions"][0], create_dimension_expr(1, 2))
        self.assertEqual(dimension["dimensions"][1], create_dimension_expr(1, 3))
        self.assertEqual(parameters["matrix"]["value"],
                         "RESHAPE([1.0, 2.0, 3.0, 4.0, 5.0, 6.0], [3, 2])")

        # Check explicit bounds
        self.assertEqual(parameters["explicit"]["type"], "INTEGER")
        dimension = cast(Dimension, parameters["explicit"]["dimension"])
        self.assertEqual(dimension["dimensions"][0], create_dimension_expr(-1, 1))
        self.assertEqual(parameters["explicit"]["value"], "[-1, 0, 1]")

        # Check array with operations
        self.assertEqual(parameters["computed"]["type"], "REAL")
        dimension = cast(Dimension, parameters["computed"]["dimension"])
        self.assertEqual(dimension["dimensions"][0], create_dimension_expr(1, 3))
        self.assertEqual(parameters["computed"]
                         ["value"], "[1.0, 1.5, 2.0] * 2.0")

        # Check character array
        self.assertEqual(parameters["str_arr"]["type"], "CHARACTER")
        self.assertEqual(parameters["str_arr"]["length"], "5")
        dimension = cast(Dimension, parameters["str_arr"]["dimension"])
        self.assertEqual(dimension["dimensions"][0], create_dimension_expr(1, 2))
        self.assertEqual(parameters["str_arr"]["value"], '["Hello", "World"]')

    def test_derived_type_declarations(self):
        self.fs.create_file("/fake/path/module.f90",
                            contents="""\
module test_mod
    implicit none

    ! Define some constants to use in the derived types
    real, parameter :: DEFAULT_X = 0.0
    real, parameter :: DEFAULT_Y = 0.0
    real, parameter :: PI = 3.14159

    type :: point
        real :: x, y
    end type point

    type :: line
        type(point) :: start, end
    end type line

    ! Using named constants in derived type parameters
    type(point), parameter :: ORIGIN = point(DEFAULT_X, DEFAULT_Y)
    
    ! Using expressions in derived type parameters
    type(point), parameter :: UNIT_X = point(1.0, 0.0)
    type(point), parameter :: UNIT_Y = point(0.0, 1.0)
    
    ! Array of derived type parameters with expressions
    type(point), parameter :: CORNERS(4) = [ &
        point(-1.0, -1.0), &
        point( 1.0, -1.0), &
        point( 1.0,  1.0), &
        point(-1.0,  1.0)  &
    ]
    
    ! Nested derived type parameter using other parameters
    type(line), parameter :: X_AXIS = line(ORIGIN, UNIT_X)
    
    ! Parameter using mathematical expressions
    type(point), parameter :: CIRCLE_POINT = point(cos(PI/4), sin(PI/4))

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
        self.assertEqual(types["point"]["data_components"]["x"]["type"], "REAL")
        self.assertEqual(types["point"]["data_components"]["y"]["type"], "REAL")

        self.assertIn("line", types)
        self.assertEqual(types["line"]["type_name"], "line")
        # start and end
        self.assertEqual(len(types["line"]["data_components"]), 2)
        self.assertEqual(types["line"]["data_components"]["start"]["type"], "point")
        self.assertEqual(types["line"]["data_components"]["end"]["type"], "point")

        # Check variables
        parameters = module_data["parameters"]
        self.assertEqual(len(parameters), 9)

        # Check basic parameter declarations
        self.assertEqual(parameters["DEFAULT_X"]["type"], "REAL")
        self.assertEqual(parameters["DEFAULT_X"]["value"], "0.0")
        
        self.assertEqual(parameters["DEFAULT_Y"]["type"], "REAL")
        self.assertEqual(parameters["DEFAULT_Y"]["value"], "0.0")
        
        self.assertEqual(parameters["PI"]["type"], "REAL")
        self.assertEqual(parameters["PI"]["value"], "3.14159")
        
        # Check derived type parameters
        self.assertEqual(parameters["ORIGIN"]["type"], "point")
        self.assertEqual(parameters["ORIGIN"]["value"], "point(DEFAULT_X, DEFAULT_Y)")
        
        self.assertEqual(parameters["UNIT_X"]["type"], "point")
        self.assertEqual(parameters["UNIT_X"]["value"], "point(1.0, 0.0)")
        
        self.assertEqual(parameters["UNIT_Y"]["type"], "point")
        self.assertEqual(parameters["UNIT_Y"]["value"], "point(0.0, 1.0)")
        
        # Check array parameter
        self.assertEqual(parameters["CORNERS"]["type"], "point")
        dimension = cast(Dimension, parameters["CORNERS"]["dimension"])
        self.assertEqual(dimension["dimensions"][0], create_dimension_expr(1, 4))
        self.assertEqual(parameters["CORNERS"]["value"], 
                        "[point(-1.0, -1.0), point(1.0, -1.0), point(1.0, 1.0), point(-1.0, 1.0)]")
        
        # Check nested derived type parameter
        self.assertEqual(parameters["X_AXIS"]["type"], "line")
        self.assertEqual(parameters["X_AXIS"]["value"], "line(ORIGIN, UNIT_X)")
        
        # Check parameter with mathematical expressions
        self.assertEqual(parameters["CIRCLE_POINT"]["type"], "point")
        self.assertEqual(parameters["CIRCLE_POINT"]["value"], "point(COS(PI / 4), SIN(PI / 4))")


    def test_array_constructor_functions(self):
        self.fs.create_file("/fake/path/module.f90",
                            contents="""\
    module test_mod
        implicit none
        
        ! Using array constructor functions
        real, parameter :: seq(5) = [(real(i), i=1,5)]
        real, parameter :: seq2(5) = [(2.0*i, i=1,5)]
        
        ! Using intrinsic functions
        real, parameter :: zeros(3) = spread(0.0, dim=1, ncopies=3)
        real, parameter :: ones(3,3) = reshape([9*1.0], [3,3])
        
    end module test_mod
    """)
        result = extract_module_data([Path("/fake/path/module.f90")])

        # Check basic structure
        self.assertEqual(len(result), 1)
        module_data = result[0]
        self.assertEqual(module_data["module_name"], "test_mod")

        # Check variables
        parameters = module_data["parameters"]
        self.assertEqual(len(parameters), 4)

        # Check array constructor function initializations
        self.assertEqual(parameters["seq"]["type"], "REAL")
        dimension = cast(Dimension, parameters["seq"]["dimension"])
        self.assertEqual(dimension["dimensions"][0], create_dimension_expr(1, 5))
        self.assertEqual(parameters["seq"]["value"], "[(REAL(i), i = 1, 5)]")

        self.assertEqual(parameters["seq2"]["type"], "REAL")
        dimension = cast(Dimension, parameters["seq2"]["dimension"])
        self.assertEqual(dimension["dimensions"][0], create_dimension_expr(1, 5))
        self.assertEqual(parameters["seq2"]["value"], "[(2.0 * i, i = 1, 5)]")

        # Check intrinsic function initializations
        self.assertEqual(parameters["zeros"]["type"], "REAL")
        dimension = cast(Dimension, parameters["zeros"]["dimension"])
        self.assertEqual(dimension["dimensions"][0], create_dimension_expr(1, 3))
        self.assertEqual(parameters["zeros"]["value"],
                         "SPREAD(0.0, dim = 1, ncopies = 3)")

        self.assertEqual(parameters["ones"]["type"], "REAL")
        dimension = cast(Dimension, parameters["ones"]["dimension"])
        self.assertEqual(len(dimension["dimensions"]), 2)
        self.assertEqual(dimension["dimensions"][0], create_dimension_expr(1, 3))
        self.assertEqual(dimension["dimensions"][1], create_dimension_expr(1, 3))
        self.assertEqual(parameters["ones"]["value"],
                         "RESHAPE([9 * 1.0], [3, 3])")

    def test_special_initializations(self):
        self.fs.create_file("/fake/path/module.f90",
                            contents="""\
    module test_mod
        implicit none

        ! Using expressions in initialization
        real, parameter :: computed = 2.0 * acos(-1.0)
        
        ! Using previously declared parameters
        real, parameter :: factor = 2.0
        real, parameter :: scaled = factor * 5.0
        
        ! Array initialization with mixed operations
        real, parameter :: mixed(4) = [1.0, factor*2.0, 3.0**2, sqrt(16.0)]
        
        ! Character with len=*
        character(len=*), parameter :: long_str = "This is a long string"
        
        ! Hollerith constants (old style, might need support)
        !CHARACTER*4 old
        !old = 4HABCD
    end module test_mod
    """)
        result = extract_module_data([Path("/fake/path/module.f90")])

        # Check basic structure
        self.assertEqual(len(result), 1)
        module_data = result[0]
        self.assertEqual(module_data["module_name"], "test_mod")

        parameters = module_data["parameters"]

        # Check expression initialization
        self.assertEqual(parameters["computed"]["type"], "REAL")
        self.assertEqual(parameters["computed"]["value"], "2.0 * ACOS(-1.0)")

        # Check parameter and its use
        self.assertEqual(parameters["factor"]["type"], "REAL")
        self.assertEqual(parameters["factor"]["value"], "2.0")

        self.assertEqual(parameters["scaled"]["type"], "REAL")
        self.assertEqual(parameters["scaled"]["value"], "factor * 5.0")

        # Check array with mixed operations
        self.assertEqual(parameters["mixed"]["type"], "REAL")
        dimension = cast(Dimension, parameters["mixed"]["dimension"])
        self.assertEqual(dimension["dimensions"][0], create_dimension_expr(1, 4))
        self.assertEqual(parameters["mixed"]["value"],
                         "[1.0, factor * 2.0, 3.0 ** 2, SQRT(16.0)]")

        # Check character with len=*
        self.assertEqual(parameters["long_str"]["type"], "CHARACTER")
        self.assertEqual(parameters["long_str"]["length"], "*")
        self.assertEqual(parameters["long_str"]
                         ["value"], '"This is a long string"')

        # Check Hollerith constant
        #TODO
#        self.assertEqual(parameters["old"]["type"], "character")
#        self.assertEqual(parameters["old"]["value"], "4habcd")

    def test_extended_parameter_types(self):
        self.fs.create_file(
            "/fake/path/extended_params.f90",
            contents="""\
    module extended_params
        implicit none
        
        ! Old-style real*8 syntax
        real*8, parameter :: pi_old = 3.14159265358979D0
        
        ! Kind specifications - multiple ways
        real(kind=8), parameter :: pi_kind = 3.14159265358979D0
        real(8), parameter :: pi_kind2 = 3.14159265358979D0
        
        ! Complex numbers
        complex, parameter :: i = (0.0, 1.0)
        complex, parameter :: z1 = cmplx(1.0, 2.0)
        complex(kind=8), parameter :: z2 = (1.0d0, 2.0d0)
        
        ! Multiple parameters in one line
        integer, parameter :: a = 1, b = 2, c = 3
        real, parameter :: x = 1.0, y = 2.0
        
        ! Logical parameters
        logical, parameter :: debug = .true.
        logical, parameter :: verbose = .false.
        
        ! Parameters with multiple attributes
        integer, public, parameter :: max_size = 100
        real, private, parameter :: internal_factor = 0.5
        
        ! Named constants with different kind values
        integer, parameter :: int_kind = selected_int_kind(9)  ! For integers up to 10^9
        real, parameter :: float_kind = selected_real_kind(10, 50)  ! Precision 10, exponent range 50
        integer(kind=int_kind), parameter :: big_int = 1000000000
        real(kind=float_kind), parameter :: precise_pi = 3.1415926535897932384626
        
        ! Character parameters - different length specifications
        character(len=10), parameter :: fixed_len = "Hello"
        character(10), parameter :: another_way = "World"
        character(*), parameter :: star_len = "Variable length"
        character, parameter :: single_char = 'A'
        
        ! Hexadecimal, octal, and binary literals
        integer, parameter :: hex_value = Z'FF'
        integer, parameter :: octal_value = O'77'
        integer, parameter :: binary_value = B'11111111'
        
        ! BOZ constants (modern syntax)
        integer, parameter :: boz_hex = z"10305070"
        integer, parameter :: boz_convert = int(z"FF", kind=4)
        
        ! Scientific notation variants
        real, parameter :: sci1 = 1.0e10
        real, parameter :: sci2 = 1.0d-10
        real, parameter :: sci3 = 1.0E+10
        
        ! Intrinsic function parameters
        real, parameter :: eps = epsilon(1.0)
        integer, parameter :: kind_val = kind(1.0)
        real, parameter :: max_real = huge(1.0)
        
        ! Expressions combining parameters
        real, parameter :: circle_area = pi_kind * 10.0**2
        complex, parameter :: exp_i_pi = exp(i * pi_kind)
        
        ! String concatenation
        character(len=10), parameter :: hello = "Hello"
        character(len=6), parameter :: world = "World!"
        character(len=17), parameter :: greeting = hello // " " // world
        
        ! String continuation
        character(len=*), parameter :: long_string = "This is a very long string that &
                                                &continues on the next line"
    end module extended_params
    """
        )
        result = extract_module_data([Path("/fake/path/extended_params.f90")])

        # Check basic structure
        self.assertEqual(len(result), 1)
        module_data = result[0]
        self.assertEqual(module_data["module_name"], "extended_params")

        parameters = module_data["parameters"]
        
        # Test old-style real*8
        self.assertEqual(parameters["pi_old"]["type"], "REAL")
        self.assertEqual(parameters["pi_old"]["kind"], "8")  # Should extract the *8
        self.assertTrue("3.14159265358979D0" in parameters["pi_old"]["value"])
        
        # Test kind specifications
        self.assertEqual(parameters["pi_kind"]["type"], "REAL")
        self.assertEqual(parameters["pi_kind"]["kind"], "8")
        self.assertTrue("3.14159265358979D0" in parameters["pi_kind"]["value"])
        
        # Test complex numbers
        self.assertEqual(parameters["i"]["type"], "COMPLEX")
        self.assertEqual(parameters["i"]["value"], "(0.0, 1.0)")
        
        self.assertEqual(parameters["z1"]["type"], "COMPLEX")
        self.assertTrue("CMPLX" in parameters["z1"]["value"].upper())
        
        # Test multiple parameters in one line
        for param in ["a", "b", "c"]:
            self.assertEqual(parameters[param]["type"], "INTEGER")
        self.assertEqual(parameters["a"]["value"], "1")
        self.assertEqual(parameters["b"]["value"], "2")
        self.assertEqual(parameters["c"]["value"], "3")
        
        # Test logical parameters
        self.assertEqual(parameters["debug"]["type"], "LOGICAL")
        self.assertEqual(parameters["debug"]["value"].lower(), ".true.")
        
        self.assertEqual(parameters["verbose"]["type"], "LOGICAL")
        self.assertEqual(parameters["verbose"]["value"].lower(), ".false.")
        
        # Test parameters with attributes
        self.assertEqual(parameters["max_size"]["type"], "INTEGER")
        self.assertTrue("PUBLIC" in [a.upper() for a in parameters["max_size"]["attributes"]])
        
        self.assertEqual(parameters["internal_factor"]["type"], "REAL")
        self.assertTrue("PRIVATE" in [a.upper() for a in parameters["internal_factor"]["attributes"]])
        
        # Test character parameters
        self.assertEqual(parameters["fixed_len"]["type"], "CHARACTER")
        self.assertEqual(parameters["fixed_len"]["length"], "10")
        
        self.assertEqual(parameters["star_len"]["type"], "CHARACTER")
        self.assertEqual(parameters["star_len"]["length"], "*")
        
        self.assertEqual(parameters["single_char"]["type"], "CHARACTER")
        self.assertEqual(parameters["single_char"]["length"], "1")
        self.assertEqual(parameters["single_char"]["value"], "'A'")
        
        # Test literal types
        for param in ["hex_value", "octal_value", "binary_value"]:
            self.assertEqual(parameters[param]["type"], "INTEGER")
        
        # Test scientific notation
        for param in ["sci1", "sci2", "sci3"]:
            self.assertEqual(parameters[param]["type"], "REAL")
        
        # Test intrinsic functions
        self.assertEqual(parameters["eps"]["type"], "REAL")
        self.assertEqual(parameters["kind_val"]["type"], "INTEGER")
        
        # Test expressions
        self.assertEqual(parameters["circle_area"]["type"], "REAL")
        self.assertTrue("pi_kind" in parameters["circle_area"]["value"].lower())
        
        # Test string concatenation
        self.assertEqual(parameters["greeting"]["type"], "CHARACTER")
        self.assertEqual(parameters["greeting"]["length"], "17")
        self.assertTrue("//" in parameters["greeting"]["value"])

    def test_comment_association(self):
        self.fs.create_file(
            "/fake/path/comments.f90",
            contents="""\
    !!*
    ! Module with comprehensive documentation
    ! Demonstrates comment association at various levels
    !*!
    module comment_test
        implicit none
        
        !!* Global parameter group *!
        integer, parameter :: a = 1, b = 2, c = 3  ! Multiple declarations on one line
        
        !!* Floating point constants *!
        real, parameter :: pi = 3.14159, e = 2.71828  ! Multiple declarations
        
        !!*
        ! Comprehensive type documentation
        ! Models a 3D vector with coordinates
        !*!
        type :: vector3d
            !!* Coordinate values in 3D space *!
            real :: x, y, z  ! Multiple components on one line
            
            !!* Length for the vector name *!
            character(len=20) :: name
            
            !!*
            ! Multi-dimensional array of values
            ! Used for storing historical positions
            !*!
            real, dimension(10,3) :: history
        end type vector3d
        
        !!* Defines common vectors *!
        type(vector3d), parameter :: origin = vector3d(0.0, 0.0, 0.0, "ORIGIN", 0.0)
        
        !!*
        ! Array of standard basis vectors
        ! These form an orthonormal basis
        !*!
        type(vector3d), parameter :: basis(3) = [ &
            vector3d(1.0, 0.0, 0.0, "X_AXIS", 0.0), &
            vector3d(0.0, 1.0, 0.0, "Y_AXIS", 0.0), &
            vector3d(0.0, 0.0, 1.0, "Z_AXIS", 0.0)  &
        ]
        
        !!* These are just simple values *!
        integer :: simple_var1, simple_var2
        
    end module comment_test
    """
        )
        result = extract_module_data([Path("/fake/path/comments.f90")])

        # Check basic structure
        self.assertEqual(len(result), 1)
        module_data = result[0]
        
        # Check module-level comment
        self.assertEqual(
            module_data["module_description"],
            "Module with comprehensive documentation\nDemonstrates comment association at various levels\n"
        )
        
        # Check parameter group comments (multiple declarations on one line)
        for param in ["a", "b", "c"]:
            self.assertEqual(
                module_data["parameters"][param]["description"],
                "Global parameter group\n"
            )
        
        # Check second parameter group
        for param in ["pi", "e"]:
            self.assertEqual(
                module_data["parameters"][param]["description"],
                "Floating point constants\n"
            )
        
        # Check type documentation
        vector_type = module_data["types"]["vector3d"]
        self.assertEqual(
            vector_type["description"],
            "Comprehensive type documentation\nModels a 3D vector with coordinates\n"
        )
        
        # Check type components with shared comment
        for coord in ["x", "y", "z"]:
            self.assertEqual(
                vector_type["data_components"][coord]["description"],
                "Coordinate values in 3D space\n"
            )
        
        # Check separate component comments
        self.assertEqual(
            vector_type["data_components"]["name"]["description"],
            "Length for the vector name\n"
        )
        
        # Check multi-line component comment
        self.assertEqual(
            vector_type["data_components"]["history"]["description"],
            "Multi-dimensional array of values\nUsed for storing historical positions\n"
        )
        
        # Check parameter with derived type
        self.assertEqual(
            module_data["parameters"]["origin"]["description"],
            "Defines common vectors\n"
        )
        
        # Check array parameter with multi-line comment
        self.assertEqual(
            module_data["parameters"]["basis"]["description"],
            "Array of standard basis vectors\nThese form an orthonormal basis\n"
        )
        
        # Check variable comment
        for var in ["simple_var1", "simple_var2"]:
            self.assertEqual(
                module_data["variables"][var]["description"],
                "These are just simple values\n"
            )

    @unittest.skip("fparser doesn't support nested types")
    def test_nested_types(self):
        self.fs.create_file("/fake/path/nested_types.f90", 
            contents="""\
    module nested_types
        implicit none

        !!* 
        ! Base structure for 2D points
        !*!
        type :: point2d
            real :: x, y
        end type point2d

        !!*
        ! Container for geometric shapes
        ! Demonstrates both composition and nested types
        !*!
        type :: geometry_container
            !!* Primary point location *!
            type(point2d) :: location  ! Using existing type (composition)
            
            !!* Scale factor for all shapes *!
            real :: scale
            
            !!*
            ! Nested type definition for 3D point
            ! Only visible within geometry_container
            !*!
            type :: point3d
                !!* 3D coordinates *!
                real :: x, y, z
                
                !!* Distance from origin *!
                real :: distance
            end type point3d
            
            !!* Origin point in 3D *!
            type(point3d) :: origin
            
            !!* Array of reference points *!
            type(point3d) :: reference_points(3)
        end type geometry_container
        
        !!* Demonstration instance *!
        type(geometry_container) :: demo_container
    end module nested_types
    """)

        result = extract_module_data([Path("/fake/path/nested_types.f90")])
        module_data = result[0]
        
        # Check module structure
        self.assertEqual(module_data["module_name"], "nested_types")
        
        # Check top-level types
        types = module_data["types"]
        self.assertEqual(len(types), 2)  # point2d and geometry_container
        
        # Check point2d type
        self.assertIn("point2d", types)
        point2d = types["point2d"]
        self.assertEqual(point2d["type_name"], "point2d")
        self.assertEqual(point2d["description"].strip(), "Base structure for 2D points")
        self.assertEqual(len(point2d["data_components"]), 2)
        
        # Check geometry_container type
        self.assertIn("geometry_container", types)
        container = types["geometry_container"]
        self.assertEqual(container["type_name"], "geometry_container")
        self.assertEqual(container["description"].strip(), 
                        "Container for geometric shapes\nDemonstrates both composition and nested types")
        
        # Check composition components
        self.assertEqual(container["data_components"]["location"]["type"], "point2d")
        self.assertEqual(container["data_components"]["location"]["description"].strip(), 
                        "Primary point location")
        
        self.assertEqual(container["data_components"]["scale"]["type"], "REAL")
        self.assertEqual(container["data_components"]["scale"]["description"].strip(), 
                        "Scale factor for all shapes")
        
        # TODO: Check nested type definition
        # This would require extending TypeDescription to support nested types
        
        # Check components that use the nested type
        self.assertEqual(container["data_components"]["origin"]["type"], "point3d")
        self.assertEqual(container["data_components"]["origin"]["description"].strip(), 
                        "Origin point in 3D")
        
        self.assertEqual(container["data_components"]["reference_points"]["type"], "point3d")
        self.assertEqual(container["data_components"]["reference_points"]["description"].strip(), 
                        "Array of reference points")
        
        # Check that it has the proper dimension
        dimension = cast(Dimension, container["data_components"]["reference_points"]["dimension"])
        self.assertEqual(dimension["dimensions"][0], create_dimension_expr(1, 3))
        
        # Check module-level variable
        self.assertEqual(module_data["variables"]["demo_container"]["type"], "geometry_container")
        self.assertEqual(module_data["variables"]["demo_container"]["description"].strip(),
                        "Demonstration instance")
        
        # TODO add tests like
        # self.assertIn("nested_types", container)
        # self.assertIn("point3d", container["nested_types"])
        # nested = container["nested_types"]["point3d"]
        # self.assertEqual(nested["description"].strip(), 
        #                  "Nested type definition for 3D point\nOnly visible within geometry_container")

if __name__ == "__main__":
    unittest.main()





