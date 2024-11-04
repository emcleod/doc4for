import unittest
from fparser.one.typedecl_statements import TypeDeclarationStatement
from unittest import TestCase
from unittest.mock import Mock
from doc4for.f90.populate_data_models import parse_variable


class TestVariables(TestCase):

    # def test_old_style_one_variable(self):
    #     declaration = Mock()
    #     declaration.name = "REAL"
    #     declaration.item.line = "REAL x"
    #     declaration.attrspec = []

    #     result = parse_variable(declaration, [])
    #     expected = [
    #         {
    #             "description": "",
    #             "type": "real",
    #             "name": "x",
    #             "dimension": None,
    #             "attributes": [],
    #             "kind": None,
    #             "initial_value": None,
    #         },
    #     ]
    #     self.assertEqual(result, expected)

    # def test_f90_style_one_variable(self):
    #     declaration = Mock()
    #     declaration.name = "REAL"
    #     declaration.item.line = "REAL :: x"
    #     declaration.attrspec = []

    #     result = parse_variable(declaration, [])
    #     expected = [
    #         {
    #             "description": "",
    #             "type": "real",
    #             "name": "x",
    #             "dimension": None,
    #             "attributes": [],
    #             "kind": None,
    #             "initial_value": None,
    #         },
    #     ]
    #     self.assertEqual(result, expected)

    # def test_old_style_multiple_variables(self):
    #     declaration = Mock()
    #     declaration.name = "real"
    #     declaration.item.line = "real x, y"
    #     declaration.attrspec = []

    #     result = parse_variable(declaration, [])
    #     expected = [
    #         {
    #             "description": "",
    #             "type": "real",
    #             "name": "x",
    #             "dimension": None,
    #             "attributes": [],
    #             "kind": None,
    #             "initial_value": None,
    #         },
    #         {
    #             "description": "",
    #             "type": "real",
    #             "name": "y",
    #             "dimension": None,
    #             "attributes": [],
    #             "kind": None,
    #             "initial_value": None,
    #         },
    #     ]
    #     self.assertEqual(result, expected)

    # def test_f90_style_multiple_variables(self):
    #     declaration = Mock()
    #     declaration.name = "real"
    #     declaration.item.line = "real :: x, y, z"
    #     declaration.attrspec = []

    #     result = parse_variable(declaration, [])
    #     expected = [
    #         {
    #             "description": "",
    #             "type": "real",
    #             "name": "x",
    #             "dimension": None,
    #             "attributes": [],
    #             "kind": None,
    #             "initial_value": None,
    #         },
    #         {
    #             "description": "",
    #             "type": "real",
    #             "name": "y",
    #             "dimension": None,
    #             "attributes": [],
    #             "kind": None,
    #             "initial_value": None,
    #         },
    #         {
    #             "description": "",
    #             "type": "real",
    #             "name": "z",
    #             "dimension": None,
    #             "attributes": [],
    #             "kind": None,
    #             "initial_value": None,
    #         },
    #     ]
    #     self.assertEqual(result, expected)

    # def test_old_style_one_initialised_variable(self):
    #     declaration = Mock()
    #     declaration.name = "REAL"
    #     declaration.item.line = "REAL x = 10"
    #     declaration.attrspec = []

    #     result = parse_variable(declaration, [])
    #     expected = [
    #         {
    #             "description": "",
    #             "type": "real",
    #             "name": "x",
    #             "dimension": None,
    #             "attributes": [],
    #             "kind": None,
    #             "initial_value": "10",
    #         },
    #     ]
    #     self.assertEqual(result, expected)

    # def test_f90_style_one_initialised_variable(self):
    #     declaration = Mock()
    #     declaration.name = "REAL"
    #     declaration.item.line = "REAL :: x = 1.4"
    #     declaration.attrspec = []

    #     result = parse_variable(declaration, [])
    #     expected = [
    #         {
    #             "description": "",
    #             "type": "real",
    #             "name": "x",
    #             "dimension": None,
    #             "attributes": [],
    #             "kind": None,
    #             "initial_value": "1.4",
    #         },
    #     ]
    #     self.assertEqual(result, expected)

    # def test_old_style_multiple_initialised_variables(self):
    #     declaration = Mock()
    #     declaration.name = "real"
    #     declaration.item.line = "real x = 10, y = 20"
    #     declaration.attrspec = []

    #     result = parse_variable(declaration, [])
    #     expected = [
    #         {
    #             "description": "",
    #             "type": "real",
    #             "name": "x",
    #             "dimension": None,
    #             "attributes": [],
    #             "kind": None,
    #             "initial_value": "10",
    #         },
    #         {
    #             "description": "",
    #             "type": "real",
    #             "name": "y",
    #             "dimension": None,
    #             "attributes": [],
    #             "kind": None,
    #             "initial_value": "20",
    #         },
    #     ]
    #     self.assertEqual(result, expected)

    # def test_f90_style_multiple_initialised_variables(self):
    #     declaration = Mock()
    #     declaration.name = "real"
    #     declaration.item.line = "real :: x = 2, y = 4, z = 49"
    #     declaration.attrspec = []

    #     result = parse_variable(declaration, [])
    #     expected = [
    #         {
    #             "description": "",
    #             "type": "real",
    #             "name": "x",
    #             "dimension": None,
    #             "attributes": [],
    #             "kind": None,
    #             "initial_value": "2",
    #         },
    #         {
    #             "description": "",
    #             "type": "real",
    #             "name": "y",
    #             "dimension": None,
    #             "attributes": [],
    #             "kind": None,
    #             "initial_value": "4",
    #         },
    #         {
    #             "description": "",
    #             "type": "real",
    #             "name": "z",
    #             "dimension": None,
    #             "attributes": [],
    #             "kind": None,
    #             "initial_value": "49",
    #         },
    #     ]
    #     self.assertEqual(result, expected)

    # def test_f90_style_with_attributes(self):
    #     declaration = Mock()

    #     declaration.name = "real"
    #     declaration.item.line = "real, parameter :: pi = 3.14159"
    #     declaration.attrspec = ["parameter"]
    #     result = parse_variable(declaration, [])
    #     expected = [
    #         {
    #             "description": "",
    #             "type": "real",
    #             "name": "pi",
    #             "dimension": None,
    #             "attributes": ["parameter"],
    #             "kind": None,
    #             "initial_value": "3.14159",
    #         },
    #     ]
    #     self.assertEqual(result, expected)

    #     declaration.name = "real"
    #     declaration.item.line = "real, dimension(10) :: x"
    #     declaration.attrspec = []
    #     result = parse_variable(declaration, [])
    #     expected = [
    #         {
    #             "description": "",
    #             "type": "real",
    #             "name": "x",
    #             "dimension": None,
    #             "attributes": [],
    #             "kind": None,
    #             "initial_value": None,
    #         },
    #     ]
    #     self.assertEqual(result, expected)

    #     declaration.name = "real"
    #     declaration.item.line = "real, allocatable :: x(:)"
    #     declaration.attrspec = ["allocatable"]
    #     result = parse_variable(declaration, [])
    #     expected = [
    #         {
    #             "description": "",
    #             "type": "real",
    #             "name": "x",
    #             "dimension": None,
    #             "attributes": ["allocatable"],
    #             "kind": None,
    #             "initial_value": None,
    #         },
    #     ]
    #     self.assertEqual(result, expected)

    #     declaration.name = "real"
    #     declaration.item.line = "real, save :: x"
    #     declaration.attrspec = ["save"]
    #     result = parse_variable(declaration, [])
    #     expected = [
    #         {
    #             "description": "",
    #             "type": "real",
    #             "name": "x",
    #             "dimension": None,
    #             "attributes": ["save"],
    #             "kind": None,
    #             "initial_value": None,
    #         },
    #     ]
    #     self.assertEqual(result, expected)

    #     declaration.name = "real"
    #     declaration.item.line = "real, target :: x"
    #     declaration.attrspec = ["target"]
    #     result = parse_variable(declaration, [])
    #     expected = [
    #         {
    #             "description": "",
    #             "type": "real",
    #             "name": "x",
    #             "dimension": None,
    #             "attributes": ["target"],
    #             "kind": None,
    #             "initial_value": None,
    #         },
    #     ]
    #     self.assertEqual(result, expected)

    #     declaration.name = "real"
    #     declaration.item.line = "real, pointer :: x"
    #     declaration.attrspec = ["pointer"]
    #     result = parse_variable(declaration, [])
    #     expected = [
    #         {
    #             "description": "",
    #             "type": "real",
    #             "name": "x",
    #             "dimension": None,
    #             "attributes": ["pointer"],
    #             "kind": None,
    #             "initial_value": None,
    #         },
    #     ]
    #     self.assertEqual(result, expected)

    # def test_old_style_with_kind(self):
    #     declaration = Mock()

    #     declaration.name = "real"
    #     declaration.item.line = "real(8) x"
    #     declaration.attrspec = []
    #     declaration.selector = ('8', '')
    #     result = parse_variable(declaration, [])
    #     expected = [
    #         {
    #             "description": "",
    #             "type": "real",
    #             "name": "x",
    #             "dimension": None,
    #             "attributes": [],
    #             "kind": '8',
    #             "initial_value": None,
    #         },
    #     ]
    #     self.assertEqual(result, expected)

    #     declaration.name = "real"
    #     declaration.item.line = "real*8 x"
    #     declaration.attrspec = []
    #     declaration.selector = ('', '8')
    #     result = parse_variable(declaration, [])
    #     expected = [
    #         {
    #             "description": "",
    #             "type": "real",
    #             "name": "x",
    #             "dimension": None,
    #             "attributes": [],
    #             "kind": '8',
    #             "initial_value": None,
    #         },
    #     ]
    #     self.assertEqual(result, expected)

    #     declaration.name = "real"
    #     declaration.item.line = "real(kind=8) x"
    #     declaration.attrspec = []
    #     declaration.selector = ('', '8')
    #     result = parse_variable(declaration, [])
    #     expected = [
    #         {
    #             "description": "",
    #             "type": "real",
    #             "name": "x",
    #             "dimension": None,
    #             "attributes": [],
    #             "kind": '8',
    #             "initial_value": None,
    #         },
    #     ]
    #     self.assertEqual(result, expected)

    # def test_f90_style_with_kind(self):
    #     declaration = Mock()

    #     declaration.name = "real"
    #     declaration.item.line = "real(8) :: x"
    #     declaration.attrspec = []
    #     declaration.selector = ('8', '')
    #     result = parse_variable(declaration, [])
    #     expected = [
    #         {
    #             "description": "",
    #             "type": "real",
    #             "name": "x",
    #             "dimension": None,
    #             "attributes": [],
    #             "kind": '8',
    #             "initial_value": None,
    #         },
    #     ]
    #     self.assertEqual(result, expected)

    #     declaration.name = "real"
    #     declaration.item.line = "real*8 :: x"
    #     declaration.attrspec = []
    #     declaration.selector = ('', '8')
    #     result = parse_variable(declaration, [])
    #     expected = [
    #         {
    #             "description": "",
    #             "type": "real",
    #             "name": "x",
    #             "dimension": None,
    #             "attributes": [],
    #             "kind": '8',
    #             "initial_value": None,
    #         },
    #     ]
    #     self.assertEqual(result, expected)

    #     declaration.name = "real"
    #     declaration.item.line = "real(kind=8) :: x"
    #     declaration.attrspec = []
    #     declaration.selector = ('', '8')
    #     result = parse_variable(declaration, [])
    #     expected = [
    #         {
    #             "description": "",
    #             "type": "real",
    #             "name": "x",
    #             "dimension": None,
    #             "attributes": [],
    #             "kind": '8',
    #             "initial_value": None,
    #         },
    #     ]
    #     self.assertEqual(result, expected)

    def test_old_style_one_dimension_array(self):
        declaration = Mock()

        declaration.name = "real"
        declaration.item.line = "real x(10)"
        declaration.attrspec = []
        result = parse_variable(declaration, [])
        expected = [
            {
                "description": "",
                "type": "real",
                "name": "x",
                "dimension": {'dimensions': [10]},
                "attributes": [],
                "kind": None,
                "initial_value": None,
            },
        ]
        self.assertEqual(result, expected)

    def test_f90_style_one_dimension_array(self):
        declaration = Mock()

        declaration.name = "real"
        declaration.item.line = "real :: x(10)"
        declaration.attrspec = []
        result = parse_variable(declaration, [])
        expected = [
            {
                "description": "",
                "type": "real",
                "name": "x",
                "dimension": {'dimensions': [10]},
                "attributes": [],
                "kind": None,
                "initial_value": None,
            },
        ]
        self.assertEqual(result, expected)

    def test_old_style_two_dimension_array(self):
        declaration = Mock()

        declaration.name = "real"
        declaration.item.line = "real x(10, 20)"
        declaration.attrspec = []
        result = parse_variable(declaration, [])
        expected = [
            {
                "description": "",
                "type": "real",
                "name": "x",
                "dimension": {'dimensions': [10, 20]},
                "attributes": [],
                "kind": None,
                "initial_value": None,
            },
        ]
        self.assertEqual(result, expected)

    def test_f90_style_two_dimension_array(self):
        declaration = Mock()

        declaration.name = "real"
        declaration.item.line = "real :: x(10,20)"
        declaration.attrspec = []
        result = parse_variable(declaration, [])
        expected = [
            {
                "description": "",
                "type": "real",
                "name": "x",
                "dimension": {'dimensions': [10, 20]},
                "attributes": [],
                "kind": None,
                "initial_value": None,
            },
        ]
        self.assertEqual(result, expected)

if __name__ == "__main__":
    unittest.main()


# real x(10,10)            ! Two-dimensional array
# real :: x(0:9)           ! Array with explicit bounds
# real :: x(-5:5)          ! Array with negative lower bound
# real x(n)                ! Array with variable dimension
# real x(2:n)              ! Array with variable upper bound
# real, dimension(10) :: array      ! F90 style
# real, dimension(10,20) :: array   ! Multi-dimensional F90 style
# real, dimension(-5:5) :: array    ! Custom bounds F90 style
# ```

# Combining Features:
# ```fortran
# real(kind=8) :: x(10) = 1.0           ! Kind, dimension, and initialization
# real*8 x(10,10), y(5), z = 1.0        ! Multiple variables with different properties
# real(selected_real_kind(15)) :: x      ! Complex kind specification
# real, dimension(10) :: x               ! Alternative array declaration
# real, dimension(10,10) :: x, y, z      ! Multiple arrays
# ```

# With Attributes:
# ```fortran
# real, parameter :: pi = 3.14159        ! Parameter attribute
# real, dimension(10) :: x               ! Dimension attribute
# real, allocatable :: x(:)              ! Allocatable array
# real, save :: x                        ! Save attribute
# real, target :: x                      ! Target attribute
# real, pointer :: x                     ! Pointer attribute

# Character Variables:
# ```fortran
# character*10 str
# character(len=10) str
# character(10) str
# character(len=10) :: str
# character(len=*), parameter :: str = "hello"
# ```

# Mixed Examples:
# ```fortran
# real(kind=8), dimension(10) :: array
# real(kind=8), dimension(10), public :: array
# real*8, dimension(0:9) array
# real(8), dimension(-5:5), save :: array = 0.0

# "real, allocatable :: x(:)"              # 1D allocatable array
# "real, allocatable :: matrix(:,:)"       # 2D allocatable array
# "real, allocatable :: cube(:,:,:)"       # 3D allocatable array
# "real, allocatable :: x(:), y(:), z(:)"  # Multiple allocatable arrays