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
        upper=Expression(expr_type=ExpressionType.LITERAL, value=str(upper), function_name=None, arguments=None),
        stride=None
    )

class TestConstants(TestCase):
    maxDiff=None

    def setUp(self):
        self.setUpPyfakefs()

    def test_find_constants(self):
        # Create a fake Fortran file
        self.fs.create_file(
            '/fake/path/constants.f90',
            contents='''\
    !!*
    ! Module containing constants
    !*!
    module complex
        ! Constants
        real, parameter :: PI = 3.14159
    end module complex
            ''',
        )

        # Call the function with the fake file
        result = extract_module_data([Path('/fake/path/constants.f90')])

        # Assertions
        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(module['module_name'], 'complex')
        self.assertEqual(module['file_name'], '/fake/path/constants.f90')
        self.assertEqual(
            module['module_description'], '\nModule containing constants\n\n'
        )
        self.assertIn('pi', module['parameters'])
        self.assertEqual(module['parameters']['pi']['value'], '3.14159')
        self.assertEqual(module['parameters']['pi']['type'], 'real')

    def test_parameters_defined_in_module(self):
        self.fs.create_file(
            '/fake/path/parameters.f90',
            contents='''\
module global_parameters_module
    implicit none

    !!* Defines pi *!
    real, parameter :: PI = 3.14159265358979323846
    ! No comment 
    integer, parameter :: MAX_ITERATIONS = 1000

    ! Rest of the program code

end module 
''')
        result = extract_module_data([Path('/fake/path/parameters.f90')])
        self.assertEqual(len(result), 1)
        module_data = result[0]
        self.assertEqual(module_data['file_name'], '/fake/path/parameters.f90')
        self.assertEqual(len(module_data['parameters']), 2)
        self.assertIn('pi', module_data['parameters'])
        pi_info = module_data['parameters']['pi']
        self.assertEqual(pi_info['type'], 'real')
        self.assertEqual(pi_info['name'], 'pi')
        self.assertEqual(pi_info['value'], '3.14159265358979323846')
        self.assertIn('max_iterations', module_data['parameters'])
        max_iter_info = module_data['parameters']['max_iterations']
        self.assertEqual(max_iter_info['type'], 'integer')
        self.assertEqual(max_iter_info['name'], 'max_iterations')
        self.assertEqual(max_iter_info['value'], '1000')
        self.assertEqual(pi_info['description'], 'Defines pi\n')
        self.assertEqual(max_iter_info['description'], '')

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

        # Check basic structure
        self.assertEqual(len(result), 1)
        module_data = result[0]
        self.assertEqual(module_data["module_name"], "test_mod")

        parameters = module_data["parameters"]
        self.assertEqual(len(parameters), 3)

        self.assertEqual(parameters["const_i"]["type"], "integer")
        self.assertEqual(parameters["const_i"]["value"], "42")

        self.assertEqual(parameters["pi"]["type"], "real")
        self.assertEqual(parameters["pi"]["value"], "3.14159")

        self.assertEqual(parameters["name"]["type"], "character")
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
        real, parameter :: matrix(2,2) = reshape([1.0, 2.0, 3.0, 4.0], [2,2])
        
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
        self.assertEqual(parameters["arr1"]["type"], "integer")
        dimension = cast(Dimension, parameters["arr1"]["dimension"])
        self.assertEqual(dimension["dimensions"][0], create_dimension_expr(1, 5))
        self.assertEqual(parameters["arr1"]["value"], "1, 2, 3, 4, 5")

        # Check array with old syntax
        self.assertEqual(parameters["arr2"]["type"], "integer")
        dimension = cast(Dimension, parameters["arr2"]["dimension"])
        self.assertEqual(dimension["dimensions"][0], create_dimension_expr(1, 3))
        self.assertEqual(parameters["arr2"]["value"], "1, 2, 3")

        # Check array with repeat syntax
        self.assertEqual(parameters["arr3"]["type"], "real")
        dimension = cast(Dimension, parameters["arr3"]["dimension"])
        self.assertEqual(dimension["dimensions"][0], create_dimension_expr(1, 4))
        self.assertEqual(parameters["arr3"]["value"], "1.0, 2.0, 2.0, 1.0")

        # Check multi-dimensional array
        self.assertEqual(parameters["matrix"]["type"], "real")
        dimension = cast(Dimension, parameters["matrix"]["dimension"])
        self.assertEqual(dimension["dimensions"][0], create_dimension_expr(1, 2))
        self.assertEqual(dimension["dimensions"][1], create_dimension_expr(1, 2))
        self.assertEqual(parameters["matrix"]["value"],
                         "reshape([1.0, 2.0, 3.0, 4.0], [2,2])")

        # Check explicit bounds
        self.assertEqual(parameters["explicit"]["type"], "integer")
        dimension = cast(Dimension, parameters["explicit"]["dimension"])
        self.assertEqual(dimension["dimensions"][0], create_dimension_expr(-1, 1))
        self.assertEqual(parameters["explicit"]["value"], "-1, 0, 1")

        # Check array with operations
        self.assertEqual(parameters["computed"]["type"], "real")
        dimension = cast(Dimension, parameters["computed"]["dimension"])
        self.assertEqual(dimension["dimensions"][0], create_dimension_expr(1, 3))
        self.assertEqual(parameters["computed"]
                         ["value"], "[1.0, 1.5, 2.0] * 2.0")

        # Check character array
        self.assertEqual(parameters["str_arr"]["type"], "character")
        self.assertEqual(parameters["str_arr"]["length"], "5")
        dimension = cast(Dimension, parameters["str_arr"]["dimension"])
        self.assertEqual(dimension["dimensions"][0], create_dimension_expr(1, 2))
        self.assertEqual(parameters["str_arr"]["value"], '"Hello", "World"')

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
        self.assertEqual(types["point"]["data_components"]
                         ["x"]["type"], "real")
        self.assertEqual(types["point"]["data_components"]
                         ["y"]["type"], "real")

        self.assertIn("line", types)
        self.assertEqual(types["line"]["type_name"], "line")
        # start and end
        self.assertEqual(len(types["line"]["data_components"]), 2)
        self.assertEqual(types["line"]["data_components"]
                         ["start"]["type"], "point")
        self.assertEqual(types["line"]["data_components"]
                         ["end"]["type"], "point")

        # Check variables
        parameters = module_data["parameters"]
        self.assertEqual(len(parameters), 9)

        # Check basic parameter declarations
        self.assertEqual(parameters["default_x"]["type"], "real")
        self.assertEqual(parameters["default_x"]["value"], "0.0")
        
        self.assertEqual(parameters["default_y"]["type"], "real")
        self.assertEqual(parameters["default_y"]["value"], "0.0")
        
        self.assertEqual(parameters["pi"]["type"], "real")
        self.assertEqual(parameters["pi"]["value"], "3.14159")
        
        # Check derived type parameters
        self.assertEqual(parameters["origin"]["type"], "point")
        self.assertEqual(parameters["origin"]["value"], "point(default_x, default_y)")
        
        self.assertEqual(parameters["unit_x"]["type"], "point")
        self.assertEqual(parameters["unit_x"]["value"], "point(1.0, 0.0)")
        
        self.assertEqual(parameters["unit_y"]["type"], "point")
        self.assertEqual(parameters["unit_y"]["value"], "point(0.0, 1.0)")
        
        # Check array parameter
        self.assertEqual(parameters["corners"]["type"], "point")
        dimension = cast(Dimension, parameters["corners"]["dimension"])
        self.assertEqual(dimension["dimensions"][0], create_dimension_expr(1, 4))
        self.assertEqual(parameters["corners"]["value"], 
                        "point(-1.0, -1.0), point( 1.0, -1.0), point( 1.0,  1.0), point(-1.0,  1.0)")
        
        # Check nested derived type parameter
        self.assertEqual(parameters["x_axis"]["type"], "line")
        self.assertEqual(parameters["x_axis"]["value"], "line(origin, unit_x)")
        
        # Check parameter with mathematical expressions
        self.assertEqual(parameters["circle_point"]["type"], "point")
        self.assertEqual(parameters["circle_point"]["value"], "point(cos(pi/4), sin(pi/4))")


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
        self.assertEqual(parameters["seq"]["type"], "real")
        dimension = cast(Dimension, parameters["seq"]["dimension"])
        self.assertEqual(dimension["dimensions"][0], create_dimension_expr(1, 5))
        self.assertEqual(parameters["seq"]["value"], "(real(i), i=1,5)")

        self.assertEqual(parameters["seq2"]["type"], "real")
        dimension = cast(Dimension, parameters["seq2"]["dimension"])
        self.assertEqual(dimension["dimensions"][0], create_dimension_expr(1, 5))
        self.assertEqual(parameters["seq2"]["value"], "(2.0*i, i=1,5)")

        # Check intrinsic function initializations
        self.assertEqual(parameters["zeros"]["type"], "real")
        dimension = cast(Dimension, parameters["zeros"]["dimension"])
        self.assertEqual(dimension["dimensions"][0], create_dimension_expr(1, 3))
        self.assertEqual(parameters["zeros"]["value"],
                         "spread(0.0, dim=1, ncopies=3)")

        self.assertEqual(parameters["ones"]["type"], "real")
        dimension = cast(Dimension, parameters["ones"]["dimension"])
        self.assertEqual(len(dimension["dimensions"]), 2)
        self.assertEqual(dimension["dimensions"][0], create_dimension_expr(1, 3))
        self.assertEqual(dimension["dimensions"][1], create_dimension_expr(1, 3))
        self.assertEqual(parameters["ones"]["value"],
                         "reshape([9*1.0], [3,3])")

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
        character, parameter :: old = 4hABCD

    end module test_mod
    """)
        result = extract_module_data([Path("/fake/path/module.f90")])

        # Check basic structure
        self.assertEqual(len(result), 1)
        module_data = result[0]
        self.assertEqual(module_data["module_name"], "test_mod")

        parameters = module_data["parameters"]

        # Check expression initialization
        self.assertEqual(parameters["computed"]["type"], "real")
        self.assertEqual(parameters["computed"]["value"], "2.0 * acos(-1.0)")

        # Check parameter and its use
        self.assertEqual(parameters["factor"]["type"], "real")
        self.assertEqual(parameters["factor"]["value"], "2.0")

        self.assertEqual(parameters["scaled"]["type"], "real")
        self.assertEqual(parameters["scaled"]["value"], "factor * 5.0")

        # Check array with mixed operations
        self.assertEqual(parameters["mixed"]["type"], "real")
        dimension = cast(Dimension, parameters["mixed"]["dimension"])
        self.assertEqual(dimension["dimensions"][0], create_dimension_expr(1, 4))
        self.assertEqual(parameters["mixed"]["value"],
                         "1.0, factor*2.0, 3.0**2, sqrt(16.0)")

        # Check character with len=*
        self.assertEqual(parameters["long_str"]["type"], "character")
        self.assertEqual(parameters["long_str"]["length"], "*")
        self.assertEqual(parameters["long_str"]
                         ["value"], '"This is a long string"')

        # Check Hollerith constant
        self.assertEqual(parameters["old"]["type"], "character")
        self.assertEqual(parameters["old"]["value"], "4habcd")


if __name__ == '__main__':
    unittest.main()
