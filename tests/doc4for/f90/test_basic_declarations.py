import unittest
from fparser.one.typedecl_statements import TypeDeclarationStatement
from unittest import TestCase
from unittest.mock import Mock
from doc4for.f90.populate_data_models import parse_variable


class TestBasicDeclarations(TestCase):

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

    def test_different_data_types(self):
        for type_name in ["INTEGER", "LOGICAL", "COMPLEX", "DOUBLE PRECISION"]:
            declaration = Mock()
            declaration.name = type_name
            declaration.item.line = f"{type_name} :: x"
            declaration.attrspec = []
            declaration.entity_decls = ['x']
            
            result = parse_variable(declaration, [])
            expected = [{
                "description": "",
                "type": type_name.lower(),
                "name": "x",
                "dimension": None,
                "attributes": [],
                "kind": None,
                "initial_value": None,
            }]
            self.assertEqual(result, expected)

    def test_complex_initialization_values(self):
        declaration = Mock()
        declaration.name = "COMPLEX"
        declaration.item.line = "COMPLEX :: z = (1.0, 2.0)"
        declaration.attrspec = []
        declaration.entity_decls = ['z = (1.0, 2.0)']
        
        result = parse_variable(declaration, [])
        expected = [{
            "description": "",
            "type": "complex",
            "name": "z",
            "dimension": None,
            "attributes": [],
            "kind": None,
            "initial_value": "(1.0, 2.0)",
        }]
        self.assertEqual(result, expected)

    def test_initialization_values(self):
        test_cases = [
            ("REAL :: x = 2.0 * 3.14159", "2.0 * 3.14159"),
            ("INTEGER :: i = 1 + 2", "1 + 2"),
            ("LOGICAL :: flag = .TRUE.", ".TRUE."),
            ("COMPLEX :: c = (1.0, 2.0)", "(1.0, 2.0)"),
        ]
        
        for line, init_value in test_cases:
            declaration = Mock()
            declaration.name = line.split()[0]
            declaration.item.line = line
            declaration.attrspec = []
            declaration.entity_decls = [f"x = {init_value}"]

            result = parse_variable(declaration, [])
            self.assertEqual(result[0]["initial_value"], init_value)

if __name__ == "__main__":
    unittest.main()