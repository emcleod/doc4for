import unittest
from unittest import TestCase
from unittest.mock import Mock
from doc4for.f90.populate_data_models import parse_variable
from doc4for.models.dimension_models import ArrayBound, BoundType
class TestAttributes(TestCase):

    def test_parameter_attribute(self):
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
                "length": None
            },
        ]
        self.assertEqual(result, expected)

    def test_save_attribute(self):
        declaration = Mock()
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
                "length": None
            },
        ]
        self.assertEqual(result, expected)

    def test_target_attribute(self):
        declaration = Mock()
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
                "length": None
            },
        ]
        self.assertEqual(result, expected)

    def test_pointer_attribute(self):
        declaration = Mock()
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
                "length": None
            },
        ]
        self.assertEqual(result, expected)

    def test_allocatable_attribute(self):
        declaration = Mock()
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
                "dimension": {
                    "dimensions": [
                        ArrayBound(
                            bound_type=BoundType.ALLOCATABLE,
                            lower=None,
                            upper=None,
                            stride=None
                        )
                    ]
                },
                "attributes": ["allocatable"],
                "kind": None,
                "initial_value": None,
                "length": None
            },
        ]        
        self.assertEqual(result, expected)

    def test_intent_attributes(self):
        intent_types = ['in', 'out', 'inout']
        for intent in intent_types:
            declaration = Mock()
            declaration.name = "real"
            declaration.item.line = f"real, intent({intent}) :: x"
            declaration.attrspec = [f"intent({intent})"]
            declaration.entity_decls = ['x']

            result = parse_variable(declaration, [])
            expected = [
                {
                    "description": "",
                    "type": "real",
                    "name": "x",
                    "dimension": None,
                    "attributes": [f"intent({intent})"],
                    "kind": None,
                    "initial_value": None,
                    "length": None
                },
            ]
            self.assertEqual(result, expected)

    def test_optional_attribute(self):
        declaration = Mock()
        declaration.name = "real"
        declaration.item.line = "real, optional :: x"
        declaration.attrspec = ["optional"]
        declaration.entity_decls = ['x']

        result = parse_variable(declaration, [])
        expected = [
            {
                "description": "",
                "type": "real",
                "name": "x",
                "dimension": None,
                "attributes": ["optional"],
                "kind": None,
                "initial_value": None,
                "length": None
        },
        ]
        self.assertEqual(result, expected)

    def test_public_private_attributes(self):
        for visibility in ['public', 'private']:
            declaration = Mock()
            declaration.name = "real"
            declaration.item.line = f"real, {visibility} :: x"
            declaration.attrspec = [visibility]
            declaration.entity_decls = ['x']

            result = parse_variable(declaration, [])
            expected = [
                {
                    "description": "",
                    "type": "real",
                    "name": "x",
                    "dimension": None,
                    "attributes": [visibility],
                    "kind": None,
                    "initial_value": None,
                    "length": None
               },
            ]
            self.assertEqual(result, expected)

    def test_external_attribute(self):
        declaration = Mock()
        declaration.name = "real"
        declaration.item.line = "real, external :: x"
        declaration.attrspec = ["external"]
        declaration.entity_decls = ['x']

        result = parse_variable(declaration, [])
        expected = [
            {
                "description": "",
                "type": "real",
                "name": "x",
                "dimension": None,
                "attributes": ["external"],
                "kind": None,
                "initial_value": None,
                "length": None
            },
        ]
        self.assertEqual(result, expected)

    def test_intrinsic_attribute(self):
        declaration = Mock()
        declaration.name = "real"
        declaration.item.line = "real, intrinsic :: x"
        declaration.attrspec = ["intrinsic"]
        declaration.entity_decls = ['x']

        result = parse_variable(declaration, [])
        expected = [
            {
                "description": "",
                "type": "real",
                "name": "x",
                "dimension": None,
                "attributes": ["intrinsic"],
                "kind": None,
                "initial_value": None,
                "length": None
            },
        ]
        self.assertEqual(result, expected)

    def test_multiple_attributes(self):
        declaration = Mock()
        declaration.name = "real"
        declaration.item.line = "real, save, intent(in), optional :: x"
        declaration.attrspec = ["save", "intent(in)", "optional"]
        declaration.entity_decls = ['x']

        result = parse_variable(declaration, [])
        expected = [
            {
                "description": "",
                "type": "real",
                "name": "x",
                "dimension": None,
                "attributes": ["save", "intent(in)", "optional"],
                "kind": None,
                "initial_value": None,
                "length": None
            },
        ]
        self.assertEqual(result, expected)

    def test_multiple_variables_with_attributes(self):
        declaration = Mock()
        declaration.name = "real"
        declaration.item.line = "real, parameter :: x = 1.0, y = 2.0"
        declaration.attrspec = ["parameter"]
        declaration.entity_decls = ['x = 1.0', 'y = 2.0']

        result = parse_variable(declaration, [])
        expected = [
            {
                "description": "",
                "type": "real",
                "name": "x",
                "dimension": None,
                "attributes": ["parameter"],
                "kind": None,
                "initial_value": "1.0",
                "length": None
            },
            {
                "description": "",
                "type": "real",
                "name": "y",
                "dimension": None,
                "attributes": ["parameter"],
                "kind": None,
                "initial_value": "2.0",
                "length": None
            },
        ]
        self.assertEqual(result, expected)
        
if __name__ == "__main__":
    unittest.main()