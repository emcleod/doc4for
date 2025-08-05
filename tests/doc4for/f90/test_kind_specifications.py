import unittest
from unittest import TestCase
from pathlib import Path
from pyfakefs.fake_filesystem_unittest import TestCase
from doc4for.f90.generate_module_tree import extract_module_data
from doc4for.models.common import BindingTypeEnum
from doc4for.models.variable_models import PolymorphismType

class TestKindSpecifications(TestCase):
    maxDiff = None

    def setUp(self):
        self.setUpPyfakefs()

    def test_old_style_kind_parentheses(self):
        self.fs.create_file(
            "/fake/path/test.f90",
            contents="""\
    module test_module
        real(8) x
    end module test_module
    """,
        )
        result = extract_module_data([Path("/fake/path/test.f90")])
        module = result[0]
        variables = module["variables"]
        expected = {
            "x": {
                "description": "",
                "type": "REAL",
                "name": "x",
                "dimension": None,
                "polymorphism_type": PolymorphismType.NONE,
                "attributes": ["PUBLIC"],
                "kind": '8',
                "initial_value": None,
                "length": None,
                "binding_type": None,
                "is_saved": False
            }
        }
        self.assertEqual(variables, expected)

    def test_old_style_kind_asterisk(self):
        self.fs.create_file(
            "/fake/path/test.f90",
            contents="""\
    module test_module
        real*8 x
    end module test_module
    """,
        )
        result = extract_module_data([Path("/fake/path/test.f90")])
        module = result[0]
        variables = module["variables"]
        expected = {
            "x": {
                "description": "",
                "type": "REAL",
                "name": "x",
                "dimension": None,
                "polymorphism_type": PolymorphismType.NONE,
                "attributes": ["PUBLIC"],
                "kind": '8',
                "initial_value": None,
                "length": None,
                "binding_type": None,
                "is_saved": False
            }
        }
        self.assertEqual(variables, expected)

    def test_f90_style_kind_keyword(self):
        self.fs.create_file(
            "/fake/path/test.f90",
            contents="""\
    module test_module
        real(kind=8) :: x
    end module test_module
    """,
        )
        result = extract_module_data([Path("/fake/path/test.f90")])
        module = result[0]
        variables = module["variables"]
        expected = {
            "x": {
                "description": "",
                "type": "REAL",
                "name": "x",
                "dimension": None,
                "polymorphism_type": PolymorphismType.NONE,
                "attributes": ["PUBLIC"],
                "kind": '8',
                "initial_value": None,
                "length": None,
                "binding_type": None,
                "is_saved": False
            }
        }
        self.assertEqual(variables, expected)

    def test_kind_with_named_constant(self):
        self.fs.create_file(
            "/fake/path/test.f90",
            contents="""\
    module test_module
        integer, parameter :: dp = kind(1.0d0)
        real(kind=dp) :: x
    end module test_module
    """,
        )
        result = extract_module_data([Path("/fake/path/test.f90")])
        module = result[0]
        parameters = module["parameters"]
        variables = module["variables"]
        expected_parameters = {
            "dp": {
                "description": "",
                "type": "INTEGER",
                "name": "dp",
                "dimension": None,
                "polymorphism_type": PolymorphismType.NONE,
                "attributes": ["PUBLIC"],
                "kind": None,
                "value": "KIND(1.0D0)",
                "length": None,
                "binding_type": None
            }
        }
        expected_variables = {
            "x": {
                "description": "",
                "type": "REAL",
                "name": "x",
                "dimension": None,
                "polymorphism_type": PolymorphismType.NONE,
                "attributes": ["PUBLIC"],
                "kind": 'dp',
                "initial_value": None,
                "length": None,
                "binding_type": None,
                "is_saved": False
            }
        }
        self.assertEqual(parameters, expected_parameters)
        self.assertEqual(variables, expected_variables)

    def test_kind_with_function_call(self):
        self.fs.create_file(
            "/fake/path/test.f90",
            contents="""\
    module test_module
        real(kind=selected_real_kind(15)) :: x
    end module test_module
    """,
        )
        result = extract_module_data([Path("/fake/path/test.f90")])
        module = result[0]
        variables = module["variables"]
        expected = {
            "x": {
                "description": "",
                "type": "REAL",
                "name": "x",
                "dimension": None,
                "polymorphism_type": PolymorphismType.NONE,
                "attributes": ["PUBLIC"],
                "kind": 'selected_real_kind(15)',
                "initial_value": None,
                "length": None,
                "binding_type": None,
                "is_saved": False
            }
        }
        self.assertEqual(variables, expected)

    def test_kind_with_multiple_variables(self):
        self.fs.create_file(
            "/fake/path/test.f90",
            contents="""\
    module test_module
        integer(kind=8) :: x, y, z
    end module test_module
    """,
        )
        result = extract_module_data([Path("/fake/path/test.f90")])
        module = result[0]
        variables = module["variables"]
        expected = {
            name: {
                "description": "",
                "type": "INTEGER",
                "name": name,
                "dimension": None,
                "polymorphism_type": PolymorphismType.NONE,
                "attributes": ["PUBLIC"],
                "kind": '8',
                "initial_value": None,
                "length": None,
                "binding_type": None,
                "is_saved": False
            }
            for name in ['x', 'y', 'z']
        }
        self.assertEqual(variables, expected)

    def test_kind_with_initialization(self):
        self.fs.create_file(
            "/fake/path/test.f90",
            contents="""\
    module test_module
        real(kind=8) :: x = 1.0d0
    end module test_module
    """,
        )
        result = extract_module_data([Path("/fake/path/test.f90")])
        module = result[0]
        variables = module["variables"]
        expected = {
            "x": {
                "description": "",
                "type": "REAL",
                "name": "x",
                "dimension": None,
                "polymorphism_type": PolymorphismType.NONE,
                "attributes": ["PUBLIC"],
                "kind": '8',
                "initial_value": "1.0D0",
                "length": None,
                "binding_type": None,
                "is_saved": False
            }
        }
        self.assertEqual(variables, expected)

    def test_character_kind(self):
        self.fs.create_file(
            "/fake/path/test.f90",
            contents="""\
    module test_module
        character(kind=selected_char_kind('ASCII')) :: x
    end module test_module
    """,
        )
        result = extract_module_data([Path("/fake/path/test.f90")])
        module = result[0]
        variables = module["variables"]
        expected = {
            "x": {
                "description": "",
                "type": "CHARACTER",
                "name": "x",
                "dimension": None,
                "polymorphism_type": PolymorphismType.NONE,
                "attributes": ["PUBLIC"],
                "kind": 'selected_char_kind(\'ASCII\')',
                "initial_value": None,
                "length": "1",  # Default character length is 1
                "binding_type": None,
                "is_saved": False
            }
        }
        self.assertEqual(variables, expected)

if __name__ == "__main__":
    unittest.main()