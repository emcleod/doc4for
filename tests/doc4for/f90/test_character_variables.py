import unittest
from fparser.one.typedecl_statements import TypeDeclarationStatement
from unittest import TestCase
from unittest.mock import Mock
from doc4for.f90.populate_data_models import parse_variable


class TestCharacterVariables(TestCase):

    def test_basic_character_declaration(self):
        declaration = Mock()
        declaration.name = "character"
        declaration.item.line = "character :: x"
        declaration.attrspec = []
        declaration.selector = None
        declaration.entity_decls = ['x']

        result = parse_variable(declaration, [])
        expected = [
            {
                "description": "",
                "type": "character",
                "name": "x",
                "dimension": None,
                "attributes": [],
                "kind": None,
                "initial_value": None,
            },
        ]
        self.assertEqual(result, expected)

    def test_character_with_length(self):
        declaration = Mock()
        declaration.name = "character"
        declaration.item.line = "character(len=10) :: str"
        declaration.attrspec = []
        declaration.selector = ('10', '')
        declaration.entity_decls = ['str']

        result = parse_variable(declaration, [])
        expected = [
            {
                "description": "",
                "type": "character",
                "name": "str",
                "dimension": None,
                "attributes": [],
                "kind": '10',
                "initial_value": None,
            },
        ]
        self.assertEqual(result, expected)

    def test_character_old_style_length(self):
        declaration = Mock()
        declaration.name = "character"
        declaration.item.line = "character*20 str"
        declaration.attrspec = []
        declaration.selector = ('', '20')
        declaration.entity_decls = ['str']

        result = parse_variable(declaration, [])
        expected = [
            {
                "description": "",
                "type": "character",
                "name": "str",
                "dimension": None,
                "attributes": [],
                "kind": '20',
                "initial_value": None,
            },
        ]
        self.assertEqual(result, expected)

    def test_character_with_initialization(self):
        declaration = Mock()
        declaration.name = "character"
        declaration.item.line = "character :: str = 'Hello, World!'"
        declaration.attrspec = []
        declaration.selector = None
        declaration.entity_decls = ["str = 'Hello, World!'"]

        result = parse_variable(declaration, [])
        expected = [
            {
                "description": "",
                "type": "character",
                "name": "str",
                "dimension": None,
                "attributes": [],
                "kind": None,
                "initial_value": "'Hello, World!'",
            },
        ]
        self.assertEqual(result, expected)

    def test_character_length_with_initialization(self):
        declaration = Mock()
        declaration.name = "character"
        declaration.item.line = "character(len=10) :: str = 'Hello'"
        declaration.attrspec = []
        declaration.selector = ('10', '')
        declaration.entity_decls = ["str = 'Hello'"]

        result = parse_variable(declaration, [])
        expected = [
            {
                "description": "",
                "type": "character",
                "name": "str",
                "dimension": None,
                "attributes": [],
                "kind": '10',
                "initial_value": "'Hello'",
            },
        ]
        self.assertEqual(result, expected)

    def test_character_with_variable_length(self):
        declaration = Mock()
        declaration.name = "character"
        declaration.item.line = "character(len=n) :: str"
        declaration.attrspec = []
        declaration.selector = ('n', '')
        declaration.entity_decls = ['str']

        result = parse_variable(declaration, [])
        expected = [
            {
                "description": "",
                "type": "character",
                "name": "str",
                "dimension": None,
                "attributes": [],
                "kind": 'n',
                "initial_value": None,
            },
        ]
        self.assertEqual(result, expected)

    def test_character_with_assumed_length(self):
        declaration = Mock()
        declaration.name = "character"
        declaration.item.line = "character(len=*) :: str"
        declaration.attrspec = []
        declaration.selector = ('*', '')
        declaration.entity_decls = ['str']

        result = parse_variable(declaration, [])
        expected = [
            {
                "description": "",
                "type": "character",
                "name": "str",
                "dimension": None,
                "attributes": [],
                "kind": '*',
                "initial_value": None,
            },
        ]
        self.assertEqual(result, expected)

    def test_multiple_character_variables(self):
        declaration = Mock()
        declaration.name = "character"
        declaration.item.line = "character(len=10) :: str1, str2"
        declaration.attrspec = []
        declaration.selector = ('10', '')
        declaration.entity_decls = ['str1', 'str2']

        result = parse_variable(declaration, [])
        expected = [
            {
                "description": "",
                "type": "character",
                "name": "str1",
                "dimension": None,
                "attributes": [],
                "kind": '10',
                "initial_value": None,
            },
            {
                "description": "",
                "type": "character",
                "name": "str2",
                "dimension": None,
                "attributes": [],
                "kind": '10',
                "initial_value": None,
            },
        ]
        self.assertEqual(result, expected)


if __name__ == "__main__":
    unittest.main()