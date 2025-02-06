import unittest
from unittest import TestCase
from unittest.mock import Mock
from doc4for.f90.populate_data_models import parse_variable
from doc4for.models.dimension_models import ArrayBound, BoundType
from doc4for.models.common import Expression, ExpressionType

class TestCharacterVariables(TestCase):
    maxDiff=None

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
                "length": "1"
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
                "kind": None,
                "initial_value": None,
                "length": "10"
            },
        ]
        self.assertEqual(result, expected)

    def test_character_old_style_length(self):
        declaration = Mock()
        declaration.name = "character"
        declaration.item.line = "character*20 str"
        declaration.attrspec = []
        declaration.selector = ('20', '')
        declaration.entity_decls = ['str']

        result = parse_variable(declaration, [])
        expected = [
            {
                "description": "",
                "type": "character",
                "name": "str",
                "dimension": None,
                "attributes": [],
                "kind": None,
                "initial_value": None,
                "length": "20"
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
                "length": "13"
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
                "kind": None,
                "initial_value": "'Hello'",
                "length": "10"
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
                "kind": None,
                "initial_value": None,
                "length": "n"
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
                "kind": None,
                "initial_value": None,
                "length": "*"
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
                "kind": None,
                "initial_value": None,
                "length": "10"
            },
            {
                "description": "",
                "type": "character",
                "name": "str2",
                "dimension": None,
                "attributes": [],
                "kind": None,
                "initial_value": None,
                "length": "10"
            },
        ]
        self.assertEqual(result, expected)

    def test_character_explicit_length_and_kind(self):
        declaration = Mock()
        declaration.name = "character"
        declaration.item.line = "character(len=20, kind=selected_char_kind('ASCII')) :: x"
        declaration.attrspec = []
        declaration.selector = ("20", "selected_char_kind('ASCII')")  
        declaration.entity_decls = ['x']

        result = parse_variable(declaration, [])
        expected = [
            {
                "description": "",
                "type": "character",
                "name": "x",
                "dimension": None,
                "attributes": [],
                "kind": "selected_char_kind('ASCII')",
                "initial_value": None,
                "length": '20'
            },
        ]
        self.assertEqual(result, expected)

    def test_character_kind_then_length(self):
        declaration = Mock()
        declaration.name = "character"
        declaration.item.line = "character(kind=selected_char_kind('ASCII'), len=20) :: x"
        declaration.attrspec = []
        declaration.selector = ('20', "selected_char_kind('ASCII')")  
        declaration.entity_decls = ['x']

        result = parse_variable(declaration, [])
        expected = [
            {
                "description": "",
                "type": "character",
                "name": "x",
                "dimension": None,
                "attributes": [],
                "kind": "selected_char_kind('ASCII')",
                "initial_value": None,
                "length": '20'
            },
        ]
        self.assertEqual(result, expected)

    def test_character_positional_len_and_kind(self):
        declaration = Mock()
        declaration.name = "character"
        declaration.item.line = "character(30, kind=selected_char_kind('ASCII')) :: x"
        declaration.attrspec = []
        declaration.selector = ('30', "selected_char_kind('ASCII')")  
        declaration.entity_decls = ['x']

        result = parse_variable(declaration, [])
        expected = [
            {
                "description": "",
                "type": "character",
                "name": "x",
                "dimension": None,
                "attributes": [],
                "kind": "selected_char_kind('ASCII')",
                "initial_value": None,
                "length": '30'
            },
        ]
        self.assertEqual(result, expected)

    def test_character_ambiguous_spec(self):
        declaration = Mock()
        declaration.name = "character"
        declaration.item.line = "character(selected_char_kind('ASCII')) :: x"
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
                "kind": "selected_char_kind('ASCII')",
                "initial_value": None,
                "length": '1'  # Default length since no explicit length
            },
        ]
        self.assertEqual(result, expected)

    def test_character_non_ascii_kind(self):
        declaration = Mock()
        declaration.name = "character"
        declaration.item.line = "character(kind=selected_char_kind('ISO_10646')) :: x"
        declaration.attrspec = []
        declaration.selector = ('', "selected_char_kind('ISO_10646')")
        declaration.entity_decls = ['x']

        result = parse_variable(declaration, [])
        expected = [
            {
                "description": "",
                "type": "character",
                "name": "x",
                "dimension": None,
                "attributes": [],
                "kind": "selected_char_kind('ISO_10646')",
                "initial_value": None,
                "length": "1"
            },
        ]
        self.assertEqual(result, expected)

    def test_character_numeric_kind(self):
        declaration = Mock()
        declaration.name = "character"
        declaration.item.line = "character(kind=4) :: x"
        declaration.attrspec = []
        declaration.selector = ('', "4")
        declaration.entity_decls = ['x']

        result = parse_variable(declaration, [])
        expected = [
            {
                "description": "",
                "type": "character",
                "name": "x",
                "dimension": None,
                "attributes": [],
                "kind": "4",
                "initial_value": None,
                "length": "1"
            },
        ]
        self.assertEqual(result, expected)       

    def test_character_ambiguous_variable_spec(self):
        declaration = Mock()
        declaration.name = "character"
        declaration.item.line = "character(char_kind) :: x"
        declaration.attrspec = []
        declaration.selector = ('char_kind', '')
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
                "length": "char_kind"
            },
        ]
        self.assertEqual(result, expected)   

    def test_character_kind_complex_expression(self):
        declaration = Mock()
        declaration.name = "character"
        declaration.item.line = "character(kind=max(selected_char_kind('ASCII'),4)) :: x"
        declaration.attrspec = []
        declaration.selector = ('', "max(selected_char_kind('ASCII'),4)")
        declaration.entity_decls = ['x']

        result = parse_variable(declaration, [])
        expected = [
            {
                "description": "",
                "type": "character",
                "name": "x",
                "dimension": None,
                "attributes": [],
                "kind": "max(selected_char_kind('ASCII'),4)",
                "initial_value": None,
                "length": "1"
            },
        ]
        self.assertEqual(result, expected)              

    def test_character_kind_named_constant(self):
        declaration = Mock()
        declaration.name = "character"
        declaration.item.line = "character(kind=ascii_kind) :: x"
        declaration.attrspec = []
        declaration.selector = ('', 'ascii_kind')
        declaration.entity_decls = ['x']

        result = parse_variable(declaration, [])
        expected = [
            {
                "description": "",
                "type": "character",
                "name": "x",
                "dimension": None,
                "attributes": [],
                "kind": "ascii_kind",
                "initial_value": None,
                "length": '1'
            },
        ]
        self.assertEqual(result, expected)
            
    def test_character_array_with_length(self):
        declaration = Mock()
        declaration.name = "character"
        declaration.item.line = "character(len=30), dimension(10) :: str_array"
        declaration.attrspec = ['dimension(10)']
        declaration.selector = ('30', '')
        declaration.entity_decls = ['str_array']

        result = parse_variable(declaration, [])
        expected = [
            {
                "description": "",
                "type": "character",
                "name": "str_array",
                "dimension": {
                    "dimensions": [
                        ArrayBound(
                            bound_type=BoundType.FIXED,
                            lower=Expression(ExpressionType.LITERAL, '1'),
                            upper=Expression(ExpressionType.LITERAL, '10'),
                            stride=None
                        )
                    ]
                },
                "attributes": [],
                "kind": None,
                "initial_value": None,
                "length": '30'
            },
        ]
        self.assertEqual(result, expected)

    def test_character_kind_integer(self):
        declaration = Mock()
        declaration.name = "character"
        declaration.item.line = "character(kind=1) :: x"
        declaration.attrspec = []
        declaration.selector = ('1', '')
        declaration.entity_decls = ['x']

        result = parse_variable(declaration, [])
        expected = [
            {
                "description": "",
                "type": "character",
                "name": "x",
                "dimension": None,
                "attributes": [],
                "kind": "1",
                "initial_value": None,
                "length": '1'
            },
        ]
        self.assertEqual(result, expected)

    def test_character_complex_length_expression(self):
        declaration = Mock()
        declaration.name = "character"
        declaration.item.line = "character(len=2*n+1) :: str"
        declaration.attrspec = []
        declaration.selector = ('2*n+1', '')
        declaration.entity_decls = ['str']

        result = parse_variable(declaration, [])
        expected = [
            {
                "description": "",
                "type": "character",
                "name": "str",
                "dimension": None,
                "attributes": [],
                "kind": None,
                "initial_value": None,
                "length": "2*n+1"
            },
        ]
        self.assertEqual(result, expected)

    def test_character_complex_kind_and_length(self):
        declaration = Mock()
        declaration.name = "character"
        declaration.item.line = "character(len=2*n+1, kind=merge(ascii, utf8, use_ascii)) :: str"
        declaration.attrspec = []
        declaration.selector = ('2*n+1', 'merge(ascii, utf8, use_ascii)')
        declaration.entity_decls = ['str']

        result = parse_variable(declaration, [])
        expected = [
            {
                "description": "",
                "type": "character",
                "name": "str",
                "dimension": None,
                "attributes": [],
                "kind": "merge(ascii, utf8, use_ascii)",
                "initial_value": None,
                "length": "2*n+1"
            },
        ]
        self.assertEqual(result, expected)
                            
if __name__ == "__main__":
    unittest.main()