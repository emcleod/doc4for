import unittest
from fparser.one.typedecl_statements import TypeDeclarationStatement
from unittest import TestCase
from unittest.mock import Mock
from doc4for.f90.populate_data_models import parse_variable


class TestComplexCases(TestCase):

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

    def test_multiple_attributes_with_initialization_and_array(self):
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

    def test_multiple_attributes_with_initialization(self):
        declaration = Mock()
        declaration.name = "real"
        declaration.item.line = "real(8), parameter, public :: pi = 3.14159265359"
        declaration.attrspec = ["parameter", "public"]
        declaration.selector = ('8', '')
        declaration.entity_decls = ['pi = 3.14159265359']

        result = parse_variable(declaration, [])
        expected = [
            {
                "description": "",
                "type": "real",
                "name": "pi",
                "dimension": None,
                "attributes": ["parameter", "public"],
                "kind": '8',
                "initial_value": "3.14159265359",
            },
        ]
        self.assertEqual(result, expected)

    def test_multiple_variables_different_initializations(self):
        declaration = Mock()
        declaration.name = "integer"
        declaration.item.line = "integer, parameter :: x = 1, y = 2*x, z = y**2"
        declaration.attrspec = ["parameter"]
        declaration.entity_decls = ['x = 1', 'y = 2*x', 'z = y**2']

        result = parse_variable(declaration, [])
        expected = [
            {
                "description": "",
                "type": "integer",
                "name": "x",
                "dimension": None,
                "attributes": ["parameter"],
                "kind": None,
                "initial_value": "1",
            },
            {
                "description": "",
                "type": "integer",
                "name": "y",
                "dimension": None,
                "attributes": ["parameter"],
                "kind": None,
                "initial_value": "2*x",
            },
            {
                "description": "",
                "type": "integer",
                "name": "z",
                "dimension": None,
                "attributes": ["parameter"],
                "kind": None,
                "initial_value": "y**2",
            },
        ]
        self.assertEqual(result, expected)

    def test_complex_initialization_expression(self):
        declaration = Mock()
        declaration.name = "real"
        declaration.item.line = "real :: x = 2.0 * sin(3.14159/2.0) ** 2"
        declaration.attrspec = []
        declaration.entity_decls = ['x = 2.0 * sin(3.14159/2.0) ** 2']

        result = parse_variable(declaration, [])
        expected = [
            {
                "description": "",
                "type": "real",
                "name": "x",
                "dimension": None,
                "attributes": [],
                "kind": None,
                "initial_value": "2.0 * sin(3.14159/2.0) ** 2",
            },
        ]
        self.assertEqual(result, expected)

    def test_mixed_variables_types_and_attributes(self):
        declaration = Mock()
        declaration.name = "real"
        declaration.item.line = "real(8), intent(in) :: x, y(10), z = 1.0"
        declaration.attrspec = ["intent(in)"]
        declaration.selector = ('8', '')
        declaration.entity_decls = ['x', 'y(10)', 'z = 1.0']

        result = parse_variable(declaration, [])
        expected = [
            {
                "description": "",
                "type": "real",
                "name": "x",
                "dimension": None,
                "attributes": ["intent(in)"],
                "kind": '8',
                "initial_value": None,
            },
            {
                "description": "",
                "type": "real",
                "name": "y",
                "dimension": {'dimensions': [{"lower": "1", "upper": "10"}]},
                "attributes": ["intent(in)"],
                "kind": '8',
                "initial_value": None,
            },
            {
                "description": "",
                "type": "real",
                "name": "z",
                "dimension": None,
                "attributes": ["intent(in)"],
                "kind": '8',
                "initial_value": "1.0",
            },
        ]
        self.assertEqual(result, expected)

    def test_complex_function_initialization(self):
        declaration = Mock()
        declaration.name = "real"
        declaration.item.line = "real :: vec(3) = [f(1), g(x, y), 3.0]"
        declaration.attrspec = []
        declaration.entity_decls = ['vec(3) = [f(1), g(x, y), 3.0]']

        result = parse_variable(declaration, [])
        expected = [
            {
                "description": "",
                "type": "real",
                "name": "vec",
                "dimension": {'dimensions': [{"lower": "1", "upper": "3"}]},
                "attributes": [],
                "kind": None,
                "initial_value": "[f(1), g(x, y), 3.0]",
            },
        ]
        self.assertEqual(result, expected)

    def test_all_features_combined(self):
        declaration = Mock()
        declaration.name = "real"
        declaration.item.line = "real(selected_real_kind(15)), dimension(10), intent(inout), public :: x = reshape([1,2,3,4,5,6,7,8,9,10], [10])"
        declaration.attrspec = ["dimension(10)", "intent(inout)", "public"]
        declaration.selector = ('selected_real_kind(15)', '')
        declaration.entity_decls = ['x = reshape([1,2,3,4,5,6,7,8,9,10], [10])']

        result = parse_variable(declaration, [])
        expected = [
            {
                "description": "",
                "type": "real",
                "name": "x",
                "dimension": {'dimensions': [{"lower": "1", "upper": "10"}]},
                "attributes": ["intent(inout)", "public"],
                "kind": 'selected_real_kind(15)',
                "initial_value": "reshape([1,2,3,4,5,6,7,8,9,10], [10])",
            },
        ]
        self.assertEqual(result, expected)

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

if __name__ == "__main__":
    unittest.main()