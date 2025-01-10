import unittest
from unittest import TestCase
from unittest.mock import Mock
from doc4for.f90.populate_data_models import parse_variable


class TestKindSpecifications(TestCase):

    def test_old_style_kind_parentheses(self):
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

    def test_old_style_kind_asterisk(self):
        declaration = Mock()
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

    def test_f90_style_kind_keyword(self):
        declaration = Mock()
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

    def test_kind_with_named_constant(self):
        declaration = Mock()
        declaration.name = "real"
        declaration.item.line = "real(kind=selected_real_kind) :: x"
        declaration.attrspec = []
        declaration.selector = ('selected_real_kind', '')
        declaration.entity_decls = ['x']

        result = parse_variable(declaration, [])
        expected = [
            {
                "description": "",
                "type": "real",
                "name": "x",
                "dimension": None,
                "attributes": [],
                "kind": 'selected_real_kind',
                "initial_value": None,
            },
        ]
        self.assertEqual(result, expected)

    def test_kind_with_function_call(self):
        declaration = Mock()
        declaration.name = "real"
        declaration.item.line = "real(kind=selected_real_kind(15)) :: x"
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

    def test_kind_with_multiple_variables(self):
        declaration = Mock()
        declaration.name = "integer"
        declaration.item.line = "integer(kind=8) :: x, y, z"
        declaration.attrspec = []
        declaration.selector = ('', '8')
        declaration.entity_decls = ['x', 'y', 'z']

        result = parse_variable(declaration, [])
        expected = [
            {
                "description": "",
                "type": "integer",
                "name": name,
                "dimension": None,
                "attributes": [],
                "kind": '8',
                "initial_value": None,
            }
            for name in ['x', 'y', 'z']
        ]
        self.assertEqual(result, expected)

    def test_kind_with_initialization(self):
        declaration = Mock()
        declaration.name = "real"
        declaration.item.line = "real(kind=8) :: x = 1.0d0"
        declaration.attrspec = []
        declaration.selector = ('', '8')
        declaration.entity_decls = ['x = 1.0d0']

        result = parse_variable(declaration, [])
        expected = [
            {
                "description": "",
                "type": "real",
                "name": "x",
                "dimension": None,
                "attributes": [],
                "kind": '8',
                "initial_value": "1.0d0",
            },
        ]
        self.assertEqual(result, expected)

    def test_character_kind(self):
        declaration = Mock()
        declaration.name = "character"
        declaration.item.line = "character(kind=selected_char_kind('ASCII')) :: x"
        declaration.attrspec = []
        declaration.selector = ('selected_char_kind(\'ASCII\')', '')
        declaration.entity_decls = ['x']

        result = parse_variable(declaration, [])
        expected = [
            {
                "description": "",
                "type": "character",
                "name": "x",
                "dimension": None,
                "attributes": [],
                "kind": 'selected_char_kind(\'ASCII\')',
                "initial_value": None,
            },
        ]
        self.assertEqual(result, expected)


if __name__ == "__main__":
    unittest.main()