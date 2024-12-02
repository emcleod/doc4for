import unittest
from fparser.one.typedecl_statements import TypeDeclarationStatement
from unittest import TestCase
from unittest.mock import Mock
from doc4for.f90.populate_data_models import parse_variable


class TestVariables(TestCase):

    def test_old_style_one_variable(self):
        declaration = Mock()
        declaration.name = "REAL"
        declaration.item.line = "REAL x"
        declaration.attrspec = []
        declaration.entity_decls = ['x']

        result = parse_variable(declaration, [])
        expected = [
            {
                "description": "",
                "type": "real",
                "name": "x",
                "dimension": None,
                "attributes": [],
                "kind": None,
                "initial_value": None,
            },
        ]
        self.assertEqual(result, expected)

    def test_f90_style_one_variable(self):
        declaration = Mock()
        declaration.name = "REAL"
        declaration.item.line = "REAL :: x"
        declaration.attrspec = []
        declaration.entity_decls = ['x']

        result = parse_variable(declaration, [])
        expected = [
            {
                "description": "",
                "type": "real",
                "name": "x",
                "dimension": None,
                "attributes": [],
                "kind": None,
                "initial_value": None,
            },
        ]
        self.assertEqual(result, expected)

    def test_old_style_multiple_variables(self):
        declaration = Mock()
        declaration.name = "real"
        declaration.item.line = "real x, y"
        declaration.attrspec = []
        declaration.entity_decls = ['x', 'y']

        result = parse_variable(declaration, [])
        expected = [
            {
                "description": "",
                "type": "real",
                "name": "x",
                "dimension": None,
                "attributes": [],
                "kind": None,
                "initial_value": None,
            },
            {
                "description": "",
                "type": "real",
                "name": "y",
                "dimension": None,
                "attributes": [],
                "kind": None,
                "initial_value": None,
            },
        ]
        self.assertEqual(result, expected)

    def test_f90_style_multiple_variables(self):
        declaration = Mock()
        declaration.name = "real"
        declaration.item.line = "real :: x, y, z"
        declaration.attrspec = []
        declaration.entity_decls = ['x', 'y', 'z']

        result = parse_variable(declaration, [])
        expected = [
            {
                "description": "",
                "type": "real",
                "name": "x",
                "dimension": None,
                "attributes": [],
                "kind": None,
                "initial_value": None,
            },
            {
                "description": "",
                "type": "real",
                "name": "y",
                "dimension": None,
                "attributes": [],
                "kind": None,
                "initial_value": None,
            },
            {
                "description": "",
                "type": "real",
                "name": "z",
                "dimension": None,
                "attributes": [],
                "kind": None,
                "initial_value": None,
            },
        ]
        self.assertEqual(result, expected)

    def test_old_style_one_initialised_variable(self):
        declaration = Mock()
        declaration.name = "REAL"
        declaration.item.line = "REAL x = 10"
        declaration.attrspec = []
        declaration.entity_decls = ['x = 10']

        result = parse_variable(declaration, [])
        expected = [
            {
                "description": "",
                "type": "real",
                "name": "x",
                "dimension": None,
                "attributes": [],
                "kind": None,
                "initial_value": "10",
            },
        ]
        self.assertEqual(result, expected)

    def test_f90_style_one_initialised_variable(self):
        declaration = Mock()
        declaration.name = "REAL"
        declaration.item.line = "REAL :: x = 1.4"
        declaration.attrspec = []
        declaration.entity_decls = ['x = 1.4']

        result = parse_variable(declaration, [])
        expected = [
            {
                "description": "",
                "type": "real",
                "name": "x",
                "dimension": None,
                "attributes": [],
                "kind": None,
                "initial_value": "1.4",
            },
        ]
        self.assertEqual(result, expected)

    def test_old_style_multiple_initialised_variables(self):
        declaration = Mock()
        declaration.name = "real"
        declaration.item.line = "real x = 10, y = 20"
        declaration.attrspec = []
        declaration.entity_decls = ['x = 10', 'y = 20']

        result = parse_variable(declaration, [])
        expected = [
            {
                "description": "",
                "type": "real",
                "name": "x",
                "dimension": None,
                "attributes": [],
                "kind": None,
                "initial_value": "10",
            },
            {
                "description": "",
                "type": "real",
                "name": "y",
                "dimension": None,
                "attributes": [],
                "kind": None,
                "initial_value": "20",
            },
        ]
        self.assertEqual(result, expected)

    def test_f90_style_multiple_initialised_variables(self):
        declaration = Mock()
        declaration.name = "real"
        declaration.item.line = "real :: x = 2, y = 4, z = 49"
        declaration.attrspec = []
        declaration.entity_decls = ['x = 2', 'y = 4', 'z = 49']

        result = parse_variable(declaration, [])
        expected = [
            {
                "description": "",
                "type": "real",
                "name": "x",
                "dimension": None,
                "attributes": [],
                "kind": None,
                "initial_value": "2",
            },
            {
                "description": "",
                "type": "real",
                "name": "y",
                "dimension": None,
                "attributes": [],
                "kind": None,
                "initial_value": "4",
            },
            {
                "description": "",
                "type": "real",
                "name": "z",
                "dimension": None,
                "attributes": [],
                "kind": None,
                "initial_value": "49",
            },
        ]
        self.assertEqual(result, expected)

    def test_f90_style_with_attributes(self):
        declaration = Mock()
        declaration.name = "real"
        declaration.item.line = "real, parameter :: pi = 3.14159"
        declaration.attrspec = ["parameter"]
        declaration.entity_decls = ['pi = 3.14159']

        result = parse_variable(declaration, [])
        expected = [
            {
                "description": "",
                "type": "real",
                "name": "pi",
                "dimension": None,
                "attributes": ["parameter"],
                "kind": None,
                "initial_value": "3.14159",
            },
        ]
        self.assertEqual(result, expected)

        declaration.name = "real"
        declaration.item.line = "real, dimension(10) :: x"
        declaration.attrspec = []
        declaration.entity_decls = ['x']

        result = parse_variable(declaration, [])
        expected = [
            {
                "description": "",
                "type": "real",
                "name": "x",
                "dimension": None,
                "attributes": [],
                "kind": None,
                "initial_value": None,
            },
        ]
        self.assertEqual(result, expected)

        declaration.name = "real"
        declaration.item.line = "real, allocatable :: x(:)"
        declaration.attrspec = ["allocatable"]
        declaration.entity_decls = ['x(:)']

        result = parse_variable(declaration, [])
        expected = [
            {
                "description": "",
                "type": "real",
                "name": "x",
                "dimension": {"dimensions": [{"lower": None, "upper": None}]},
                "attributes": ["allocatable"],
                "kind": None,
                "initial_value": None,
            },
        ]
        self.assertEqual(result, expected)

        declaration.name = "real"
        declaration.item.line = "real, save :: x"
        declaration.attrspec = ["save"]
        declaration.entity_decls = ['x']

        result = parse_variable(declaration, [])
        expected = [
            {
                "description": "",
                "type": "real",
                "name": "x",
                "dimension": None,
                "attributes": ["save"],
                "kind": None,
                "initial_value": None,
            },
        ]
        self.assertEqual(result, expected)

        declaration.name = "real"
        declaration.item.line = "real, target :: x"
        declaration.attrspec = ["target"]
        declaration.entity_decls = ['x']

        result = parse_variable(declaration, [])
        expected = [
            {
                "description": "",
                "type": "real",
                "name": "x",
                "dimension": None,
                "attributes": ["target"],
                "kind": None,
                "initial_value": None,
            },
        ]
        self.assertEqual(result, expected)

        declaration.name = "real"
        declaration.item.line = "real, pointer :: x"
        declaration.attrspec = ["pointer"]
        declaration.entity_decls = ['x']

        result = parse_variable(declaration, [])
        expected = [
            {
                "description": "",
                "type": "real",
                "name": "x",
                "dimension": None,
                "attributes": ["pointer"],
                "kind": None,
                "initial_value": None,
            },
        ]
        self.assertEqual(result, expected)

    def test_old_style_with_kind(self):
        declaration = Mock()

        declaration.name = "real"
        declaration.item.line = "real(8) x"
        declaration.attrspec = []
        declaration.selector = ('8', '')
        declaration.entity_decls = ['x']

        result = parse_variable(declaration, [])
        expected = [
            {
                "description": "",
                "type": "real",
                "name": "x",
                "dimension": None,
                "attributes": [],
                "kind": '8',
                "initial_value": None,
            },
        ]
        self.assertEqual(result, expected)

        declaration.name = "real"
        declaration.item.line = "real*8 x"
        declaration.attrspec = []
        declaration.selector = ('', '8')
        declaration.entity_decls = ['x']

        result = parse_variable(declaration, [])
        expected = [
            {
                "description": "",
                "type": "real",
                "name": "x",
                "dimension": None,
                "attributes": [],
                "kind": '8',
                "initial_value": None,
            },
        ]
        self.assertEqual(result, expected)

        declaration.name = "real"
        declaration.item.line = "real(kind=8) x"
        declaration.attrspec = []
        declaration.selector = ('', '8')
        declaration.entity_decls = ['x']

        result = parse_variable(declaration, [])
        expected = [
            {
                "description": "",
                "type": "real",
                "name": "x",
                "dimension": None,
                "attributes": [],
                "kind": '8',
                "initial_value": None,
            },
        ]
        self.assertEqual(result, expected)

    def test_f90_style_with_kind(self):
        declaration = Mock()

        declaration.name = "real"
        declaration.item.line = "real(8) :: x"
        declaration.attrspec = []
        declaration.selector = ('8', '')
        declaration.entity_decls = ['x']

        result = parse_variable(declaration, [])
        expected = [
            {
                "description": "",
                "type": "real",
                "name": "x",
                "dimension": None,
                "attributes": [],
                "kind": '8',
                "initial_value": None,
            },
        ]
        self.assertEqual(result, expected)

        declaration.name = "real"
        declaration.item.line = "real*8 :: x"
        declaration.attrspec = []
        declaration.selector = ('', '8')
        declaration.entity_decls = ['x']

        result = parse_variable(declaration, [])
        expected = [
            {
                "description": "",
                "type": "real",
                "name": "x",
                "dimension": None,
                "attributes": [],
                "kind": '8',
                "initial_value": None,
            },
        ]
        self.assertEqual(result, expected)

        declaration.name = "real"
        declaration.item.line = "real(kind=8) :: x"
        declaration.attrspec = []
        declaration.selector = ('', '8')
        declaration.entity_decls = ['x']

        result = parse_variable(declaration, [])
        expected = [
            {
                "description": "",
                "type": "real",
                "name": "x",
                "dimension": None,
                "attributes": [],
                "kind": '8',
                "initial_value": None,
            },
        ]
        self.assertEqual(result, expected)

    def test_old_style_one_dimension_array(self):
        declaration = Mock()

        declaration.name = "real"
        declaration.item.line = "real x(10)"
        declaration.attrspec = []
        declaration.entity_decls = ['x(10)']

        result = parse_variable(declaration, [])
        expected = [
            {
                "description": "",
                "type": "real",
                "name": "x",
                "dimension": {'dimensions': [{"lower": "1", "upper": "10"}]},
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
        declaration.entity_decls = ['x(10)']

        result = parse_variable(declaration, [])
        expected = [
            {
                "description": "",
                "type": "real",
                "name": "x",
                "dimension": {'dimensions': [{"lower": "1", "upper": "10"}]},
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
        declaration.entity_decls = ['x(10, 20)']

        result = parse_variable(declaration, [])
        expected = [
            {
                "description": "",
                "type": "real",
                "name": "x",
                "dimension": {'dimensions': [
                    {"lower": "1", "upper": "10"},
                    {"lower": "1", "upper": "20"}
                ]},
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
        declaration.entity_decls = ['x(10,20)']

        result = parse_variable(declaration, [])
        expected = [
            {
                "description": "",
                "type": "real",
                "name": "x",
                "dimension": {'dimensions': [
                    {"lower": "1", "upper": "10"},
                    {"lower": "1", "upper": "20"}
                ]},
                "attributes": [],
                "kind": None,
                "initial_value": None,
            },
        ]
        self.assertEqual(result, expected)

    def test_old_style_explicit_bounds(self):
        declaration = Mock()

        declaration.name = "real"
        declaration.item.line = "real x(0:9)"
        declaration.attrspec = []
        declaration.entity_decls = ['x(0:9)']

        result = parse_variable(declaration, [])
        expected = [
            {
                "description": "",
                "type": "real",
                "name": "x",
                "dimension": {'dimensions': [{"lower": "0", "upper": "9"}]},
                "attributes": [],
                "kind": None,
                "initial_value": None,
            },
        ]
        self.assertEqual(result, expected)

    def test_f90_style_explicit_bounds(self):
        declaration = Mock()

        declaration.name = "real"
        declaration.item.line = "real :: x(-5 : 5)"
        declaration.attrspec = []
        declaration.entity_decls = ['x(-5:5)']

        result = parse_variable(declaration, [])
        expected = [
            {
                "description": "",
                "type": "real",
                "name": "x",
                "dimension": {'dimensions': [{"lower": "-5", "upper": "5"}]},
                "attributes": [],
                "kind": None,
                "initial_value": None,
            },
        ]
        self.assertEqual(result, expected)

    def test_f90_style_with_variable_dimension(self):
        declaration = Mock()

        declaration.name = "real"
        declaration.item.line = "real :: x(n)"
        declaration.attrspec = []
        declaration.entity_decls = ['x(n)']

        result = parse_variable(declaration, [])
        expected = [
            {
                "description": "",
                "type": "real",
                "name": "x",
                "dimension": {'dimensions': [{"lower": "1", "upper": "n"}]},
                "attributes": [],
                "kind": None,
                "initial_value": None,
            },
        ]
        self.assertEqual(result, expected)

    def test_f90_style_with_variable_upper_bound(self):
        declaration = Mock()

        declaration.name = "real"
        declaration.item.line = "real :: x(2:n)"
        declaration.attrspec = []
        declaration.entity_decls = ['x(2:n)']

        result = parse_variable(declaration, [])
        expected = [
            {
                "description": "",
                "type": "real",
                "name": "x",
                "dimension": {'dimensions': [{"lower": "2", "upper": "n"}]},
                "attributes": [],
                "kind": None,
                "initial_value": None,
            },
        ]
        self.assertEqual(result, expected)

    def test_f90_alternate_style(self):
        declaration = Mock()

        declaration.name = "real"
        declaration.item.line = "real dimension(10):: x"
        declaration.attrspec = ["dimension(10)"]
        declaration.entity_decls = ['x']

        result = parse_variable(declaration, [])
        expected = [
            {
                "description": "",
                "type": "real",
                "name": "x",
                "dimension": {'dimensions': [{"lower": "1", "upper": "10"}]},
                "attributes": [],
                "kind": None,
                "initial_value": None,
            },
        ]
        self.assertEqual(result, expected)

        declaration.name = "real"
        declaration.item.line = "real dimension(10, 20):: x"
        declaration.attrspec = ["dimension(10, 20)"]
        declaration.entity_decls = ['x']

        result = parse_variable(declaration, [])
        expected = [
            {
                "description": "",
                "type": "real",
                "name": "x",
                "dimension": {'dimensions': [
                    {"lower": "1", "upper": "10"},
                    {"lower": "1", "upper": "20"},
                ]},
                "attributes": [],
                "kind": None,
                "initial_value": None,
            },
        ]
        self.assertEqual(result, expected)

        declaration.name = "real"
        declaration.item.line = "real dimension(-5:5):: x"
        declaration.attrspec = ["dimension(-5:5)"]
        declaration.entity_decls = ['x']

        result = parse_variable(declaration, [])
        expected = [
            {
                "description": "",
                "type": "real",
                "name": "x",
                "dimension": {'dimensions': [{"lower": "-5", "upper": "5"}]},
                "attributes": [],
                "kind": None,
                "initial_value": None,
            },
        ]
        self.assertEqual(result, expected)

        declaration.name = "real"
        declaration.item.line = "real dimension(-5:5,10,n:20+n):: x"
        declaration.attrspec = ["dimension(-5:5,10,n:20+n)"]
        declaration.entity_decls = ['x']

        result = parse_variable(declaration, [])
        expected = [
            {
                "description": "",
                "type": "real",
                "name": "x",
                "dimension": {'dimensions': [
                    {"lower": "-5", "upper": "5"},
                    {"lower": "1", "upper": "10"},
                    {"lower": "n", "upper": "20+n"}
                ]},
                "attributes": [],
                "kind": None,
                "initial_value": None,
            },
        ]
        self.assertEqual(result, expected)

    def test_old_style_multiple_variable_types(self):
        declaration = Mock()
        declaration.name = "real"
        declaration.item.line = "real x(10, 20), y, z(5)"
        declaration.attrspec = []
        declaration.entity_decls = ['x(10, 20)', 'y', 'z(5)']

        result = parse_variable(declaration, [])
        expected = [
            {
                "description": "",
                "type": "real",
                "name": "x",
                "dimension": {'dimensions': [
                    {"lower": "1", "upper": "10"},
                    {"lower": "1", "upper": "20"}
                ]},
                "attributes": [],
                "kind": None,
                "initial_value": None,
            },
            {
                "description": "",
                "type": "real",
                "name": "y",
                "dimension": None,
                "attributes": [],
                "kind": None,
                "initial_value": None,
            },
            {
                "description": "",
                "type": "real",
                "name": "z",
                "dimension": {'dimensions': [{"lower": "1", "upper": "5"}]},
                "attributes": [],
                "kind": None,
                "initial_value": None,
            },
        ]
        self.assertEqual(result, expected)

    def test_old_style_kind_with_dimension(self):
        declaration = Mock()

        declaration.name = "real"
        declaration.item.line = "real(kind=8) x(10, 20)"
        declaration.attrspec = []
        declaration.selector = ('', '8')
        declaration.entity_decls = ['x(10, 20)']

        result = parse_variable(declaration, [])
        expected = [
            {
                "description": "",
                "type": "real",
                "name": "x",
                "dimension": {'dimensions': [
                    {"lower": "1", "upper": "10"},
                    {"lower": "1", "upper": "20"}
                ]},
                "attributes": [],
                "kind": '8',
                "initial_value": None,
            },
        ]
        self.assertEqual(result, expected)

        declaration.name = "real"
        declaration.item.line = "real*8 x(10, 20)"
        declaration.attrspec = []
        declaration.selector = ('', '8')
        declaration.entity_decls = ['x(10, 20)']

        result = parse_variable(declaration, [])
        expected = [
            {
                "description": "",
                "type": "real",
                "name": "x",
                "dimension": {'dimensions': [
                    {"lower": "1", "upper": "10"},
                    {"lower": "1", "upper": "20"}
                ]},
                "attributes": [],
                "kind": '8',
                "initial_value": None,
            },
        ]
        self.assertEqual(result, expected)

    def test_complex_initialization(self):
        declaration = Mock()

        declaration.name = "real"
        declaration.item.line = "real x(2,2) = reshape((/1,2,3,4/), (/2,2/))"
        declaration.attrspec = []
        declaration.entity_decls = ['x(2,2) = reshape((/1,2,3,4/), (/2,2/))']

        result = parse_variable(declaration, [])
        expected = [
            {
                "description": "",
                "type": "real",
                "name": "x",
                "dimension": {'dimensions': [
                    {"lower": "1", "upper": "2"},
                    {"lower": "1", "upper": "2"}
                ]},
                "attributes": [],
                "kind": None,
                "initial_value": "reshape((/1,2,3,4/), (/2,2/))",
            },
        ]
        self.assertEqual(result, expected)

        declaration.name = "real"
        declaration.item.line = "real y(2,2) = ((1,2),(3,4))"
        declaration.attrspec = []
        declaration.entity_decls = ['y(2,2) = ((1,2),(3,4))']

        result = parse_variable(declaration, [])
        expected = [
            {
                "description": "",
                "type": "real",
                "name": "y",
                "dimension": {'dimensions': [
                    {"lower": "1", "upper": "2"},
                    {"lower": "1", "upper": "2"}
                ]},
                "attributes": [],
                "kind": None,
                "initial_value": "((1,2),(3,4))",
            },
        ]
        self.assertEqual(result, expected)

    def test_character_declaration(self):
        declaration = Mock()

        declaration.name = "character"
        declaration.item.line = "character(len=10) str(5,5)"
        declaration.attrspec = []
        declaration.selector = ('10', '')
        declaration.entity_decls = ['str(5,5)']

        result = parse_variable(declaration, [])
        expected = [
            {
                "description": "",
                "type": "character",
                "name": "str",
                "dimension": {'dimensions': [
                    {"lower": "1", "upper": "5"},
                    {"lower": "1", "upper": "5"}
                ]},
                "attributes": [],
                "kind": '10',
                "initial_value": None,
            },
        ]
        self.assertEqual(result, expected)

    def test_complex_dimension_expressions(self):
        declaration = Mock()

        declaration.name = "real"
        declaration.item.line = "real x(f(1,2), 10)"
        declaration.attrspec = []
        declaration.entity_decls = ['x(f(1,2), 10)']

        result = parse_variable(declaration, [])
        expected = [
            {
                "description": "",
                "type": "real",
                "name": "x",
                "dimension": {'dimensions': [
                    {"lower": "1", "upper": "f(1,2)"},
                    {"lower": "1", "upper": "10"}
                ]},
                "attributes": [],
                "kind": None,
                "initial_value": None,
            },
        ]
        self.assertEqual(result, expected)

        declaration.name = "real"
        declaration.item.line = "real x(2*5, n+1)"
        declaration.attrspec = []
        declaration.entity_decls = ['x(2*5, n+1)']

        result = parse_variable(declaration, [])
        expected = [
            {
                "description": "",
                "type": "real",
                "name": "x",
                "dimension": {'dimensions': [
                    {"lower": "1", "upper": "2*5"},
                    {"lower": "1", "upper": "n+1"}
                ]},
                "attributes": [],
                "kind": None,
                "initial_value": None,
            },
        ]
        self.assertEqual(result, expected)

        declaration.name = "real"
        declaration.item.line = "real x(n:m, 1:10)"
        declaration.attrspec = []
        declaration.entity_decls = ['x(n:m, 1:10)']

        result = parse_variable(declaration, [])
        expected = [
            {
                "description": "",
                "type": "real",
                "name": "x",
                "dimension": {'dimensions': [
                    {"lower": "n", "upper": "m"},
                    {"lower": "1", "upper": "10"}
                ]},
                "attributes": [],
                "kind": None,
                "initial_value": None,
            },
        ]
        self.assertEqual(result, expected)

    def test_complex_kind_specification(self):
        declaration = Mock()
        declaration.name = "real"
        declaration.item.line = "real(selected_real_kind(15)) :: x"
        declaration.attrspec = []
        declaration.selector = ('selected_real_kind(15)', '')
        declaration.entity_decls = ['x']

        result = parse_variable(declaration, [])
        expected = [
            {
                "description": "",
                "type": "real",
                "name": "x",
                "dimension": None,
                "attributes": [],
                "kind": 'selected_real_kind(15)',
                "initial_value": None,
            },
        ]
        self.assertEqual(result, expected)

    def test_multiple_arrays_same_attributes(self):
        declaration = Mock()
        declaration.name = "real"
        declaration.item.line = "real, dimension(10,10) :: x, y, z"
        declaration.attrspec = ["dimension(10,10)"]
        declaration.entity_decls = ['x', 'y', 'z']

        result = parse_variable(declaration, [])
        expected = [
            {
                "description": "",
                "type": "real",
                "name": "x",
                "dimension": {'dimensions': [
                    {"lower": "1", "upper": "10"},
                    {"lower": "1", "upper": "10"}
                ]},
                "attributes": [],
                "kind": None,
                "initial_value": None,
            },
            {
                "description": "",
                "type": "real",
                "name": "y",
                "dimension": {'dimensions': [
                    {"lower": "1", "upper": "10"},
                    {"lower": "1", "upper": "10"}
                ]},
                "attributes": [],
                "kind": None,
                "initial_value": None,
            },
            {
                "description": "",
                "type": "real",
                "name": "z",
                "dimension": {'dimensions': [
                    {"lower": "1", "upper": "10"},
                    {"lower": "1", "upper": "10"}
                ]},
                "attributes": [],
                "kind": None,
                "initial_value": None,
            },
        ]
        self.assertEqual(result, expected)

    def test_character_declaration_styles(self):
        for line in [
            "character*10 str",
            "character(10) str",
            "character(len=10) :: str",
            "character(len=*), parameter :: str = 'hello'"
        ]:
            declaration = Mock()
            declaration.name = "character"
            declaration.item.line = line
            declaration.attrspec = ["parameter"] if "parameter" in line else []
            declaration.selector = ('10', '') if "*" not in line else ('', '10')
            declaration.entity_decls = [
                "str = 'hello'" if "=" in line else "str"
            ]

            result = parse_variable(declaration, [])
            
            expected_kind = '10' if "*" not in line else '10'
            expected_attributes = ["parameter"] if "parameter" in line else []
            expected_initial_value = "'hello'" if "=" in line else None

            self.assertEqual(result[0]["type"], "character")
            self.assertEqual(result[0]["name"], "str")
            self.assertEqual(result[0]["kind"], expected_kind)
            self.assertEqual(result[0]["attributes"], expected_attributes)
            self.assertEqual(result[0]["initial_value"], expected_initial_value)

    def test_multiple_attributes_with_initialization(self):
        declaration = Mock()
        declaration.name = "real"
        declaration.item.line = "real(8), dimension(-5:5), save :: array = 0.0"
        declaration.attrspec = ["dimension(-5:5)", "save"]
        declaration.selector = ('8', '')
        declaration.entity_decls = ['array = 0.0']

        result = parse_variable(declaration, [])
        expected = [
            {
                "description": "",
                "type": "real",
                "name": "array",
                "dimension": {'dimensions': [{"lower": "-5", "upper": "5"}]},
                "attributes": ["save"],
                "kind": '8',
                "initial_value": "0.0",
            },
        ]
        self.assertEqual(result, expected)           

    def test_allocatable_arrays(self):
       for line, expected_dims in [
           ("real, allocatable :: x(:)", 1),
           ("real, allocatable :: matrix(:,:)", 2),
           ("real, allocatable :: cube(:,:,:)", 3),
           ("real, allocatable :: x(:), y(:), z(:)", 1),  # Multiple declarations
       ]:
           declaration = Mock()
           declaration.name = "real"
           declaration.item.line = line
           declaration.attrspec = ["allocatable"]
           declaration.entity_decls = [p.strip() for p in line.split("::")[1].split(",")]

           result = parse_variable(declaration, [])
           
           for var in result:
               self.assertEqual(var["type"], "real")
               self.assertEqual(var["attributes"], ["allocatable"])
               self.assertIsNotNone(var["dimension"])
               if "," not in line.split("::")[1]:  # Single variable declaration
                   if var["dimension"]:
                       self.assertEqual(len(var["dimension"]["dimensions"]), expected_dims)

    def test_multiple_variables_with_different_properties(self):
        declaration = Mock()
        declaration.name = "real"
        declaration.item.line = "real*8 x(10,10), y(5), z = 1.0"
        declaration.attrspec = []
        declaration.selector = ('', '8')
        declaration.entity_decls = ['x(10,10)', 'y(5)', 'z = 1.0']

        result = parse_variable(declaration, [])
        expected = [
            {
                "description": "",
                "type": "real",
                "name": "x",
                "dimension": {'dimensions': [
                    {"lower": "1", "upper": "10"},
                    {"lower": "1", "upper": "10"}
                ]},
                "attributes": [],
                "kind": "8",
                "initial_value": None,
            },
            {
                "description": "",
                "type": "real",
                "name": "y",
                "dimension": {'dimensions': [
                    {"lower": "1", "upper": "5"},
                ]},
                "attributes": [],
                "kind": "8",
                "initial_value": None,
            },
            {
                "description": "",
                "type": "real",
                "name": "z",
                "dimension": None,
                "attributes": [],
                "kind": "8",
                "initial_value": "1.0",
            },
        ]
        self.assertEqual(result, expected)

    def test_mixed_attributes(self):
        declaration = Mock()
        declaration.name = "real"
        declaration.item.line = "real(kind=8), dimension(10), public :: array"
        declaration.attrspec = ["dimension(10)", "public"]
        declaration.selector = ('', '8')
        declaration.entity_decls = ['array']

        result = parse_variable(declaration, [])
        expected = [
            {
                "description": "",
                "type": "real",
                "name": "array",
                "dimension": {'dimensions': [
                    {"lower": "1", "upper": "10"}
                ]},
                "attributes": ["public"],
                "kind": "8",
                "initial_value": None,
            },
        ]
        self.assertEqual(result, expected)

    def test_initialization_with_array_constructor(self):
        declaration = Mock()
        declaration.name = "integer"
        declaration.item.line = "integer, parameter, dimension(3) :: arr = [1, 2, 3]"
        declaration.attrspec = ["parameter", "dimension(3)"]
        declaration.entity_decls = ['arr = [1, 2, 3]']

        result = parse_variable(declaration, [])
        expected = [
            {
                "description": "",
                "type": "integer",
                "name": "arr",
                "dimension": {'dimensions': [
                    {"lower": "1", "upper": "3"}
                ]},
                "attributes": ["parameter"],
                "kind": None,
                "initial_value": "[1, 2, 3]",
            },
        ]
        self.assertEqual(result, expected)


if __name__ == "__main__":
    unittest.main()

