import unittest
from unittest import TestCase
from pathlib import Path
from pyfakefs.fake_filesystem_unittest import TestCase
from doc4for.f90.generate_module_tree import extract_module_data
from doc4for.models.dimension_models import ArrayBound, BoundType
from doc4for.models.common import Expression, ExpressionType
from doc4for.models.variable_models import PolymorphismType

class TestCharacterVariables(TestCase):
    maxDiff = None

    def setUp(self):
        self.setUpPyfakefs()

    def test_basic_character_declaration(self):
        self.fs.create_file(
            "/fake/path/test.f90",
            contents="""\
    module test_module
        character :: x
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
                "attributes": [],
                "kind": None,
                "initial_value": None,
                "length": "1",
                "binding_type": None,
                "is_saved": False
            }
        }
        self.assertEqual(variables, expected)

    def test_character_with_length(self):
        self.fs.create_file(
            "/fake/path/test.f90",
            contents="""\
    module test_module
        character(len=10) :: str
    end module test_module
    """,
        )
        result = extract_module_data([Path("/fake/path/test.f90")])
        module = result[0]
        variables = module["variables"]
        expected = {
            "str": {
                "description": "",
                "type": "CHARACTER",
                "name": "str",
                "dimension": None,
                "polymorphism_type": PolymorphismType.NONE,
                "attributes": [],
                "kind": None,
                "initial_value": None,
                "length": "10",
                "binding_type": None,
                "is_saved": False
            }
        }
        self.assertEqual(variables, expected)

    def test_character_old_style_length(self):
        self.fs.create_file(
            "/fake/path/test.f90",
            contents="""\
    module test_module
        character*20 str
    end module test_module
    """,
        )
        result = extract_module_data([Path("/fake/path/test.f90")])
        module = result[0]
        variables = module["variables"]
        expected = {
            "str": {
                "description": "",
                "type": "CHARACTER",
                "name": "str",
                "dimension": None,
                "polymorphism_type": PolymorphismType.NONE,
                "attributes": [],
                "kind": None,
                "initial_value": None,
                "length": "20",
                "binding_type": None,
                "is_saved": False
            }
        }
        self.assertEqual(variables, expected)

    def test_character_with_initialization(self):
        self.fs.create_file(
            "/fake/path/test.f90",
            contents="""\
    module test_module
        character :: str = 'Hello, World!'
    end module test_module
    """,
        )
        result = extract_module_data([Path("/fake/path/test.f90")])
        module = result[0]
        variables = module["variables"]
        expected = {
            "str": {
                "description": "",
                "type": "CHARACTER",
                "name": "str",
                "dimension": None,
                "polymorphism_type": PolymorphismType.NONE,
                "attributes": [],
                "kind": None,
                "initial_value": "'Hello, World!'",
                "length": "1", # correct - it's a character so truncated to 1
                "binding_type": None,
                "is_saved": False
            }
        }
        self.assertEqual(variables, expected)

    def test_character_length_with_initialization(self):
        self.fs.create_file(
            "/fake/path/test.f90",
            contents="""\
    module test_module
        character(len=10) :: str = 'Hello'
    end module test_module
    """,
        )
        result = extract_module_data([Path("/fake/path/test.f90")])
        module = result[0]
        variables = module["variables"]
        expected = {
            "str": {
                "description": "",
                "type": "CHARACTER",
                "name": "str",
                "dimension": None,
                "polymorphism_type": PolymorphismType.NONE,
                "attributes": [],
                "kind": None,
                "initial_value": "'Hello'",
                "length": "10",
                "binding_type": None,
                "is_saved": False
            }
        }
        self.assertEqual(variables, expected)

    def test_character_with_variable_length(self):
        self.fs.create_file(
            "/fake/path/test.f90",
            contents="""\
    module test_module
        character(len=n) :: str
    end module test_module
    """,
        )
        result = extract_module_data([Path("/fake/path/test.f90")])
        module = result[0]
        variables = module["variables"]
        expected = {
            "str": {
                "description": "",
                "type": "CHARACTER",
                "name": "str",
                "dimension": None,
                "polymorphism_type": PolymorphismType.NONE,
                "attributes": [],
                "kind": None,
                "initial_value": None,
                "length": "n",
                "binding_type": None,
                "is_saved": False
            }
        }
        self.assertEqual(variables, expected)

    def test_character_with_assumed_length(self):
        self.fs.create_file(
            "/fake/path/test.f90",
            contents="""\
    module test_module
        character(len=*) :: str
    end module test_module
    """,
        )
        result = extract_module_data([Path("/fake/path/test.f90")])
        module = result[0]
        variables = module["variables"]
        expected = {
            "str": {
                "description": "",
                "type": "CHARACTER",
                "name": "str",
                "dimension": None,
                "polymorphism_type": PolymorphismType.NONE,
                "attributes": [],
                "kind": None,
                "initial_value": None,
                "length": "*",
                "binding_type": None,
                "is_saved": False
            }
        }
        self.assertEqual(variables, expected)

    def test_multiple_character_variables(self):
        self.fs.create_file(
            "/fake/path/test.f90",
            contents="""\
    module test_module
        character(len=10) :: str1, str2
    end module test_module
    """,
        )
        result = extract_module_data([Path("/fake/path/test.f90")])
        module = result[0]
        variables = module["variables"]
        expected = {
            "str1": {
                "description": "",
                "type": "CHARACTER",
                "name": "str1",
                "dimension": None,
                "polymorphism_type": PolymorphismType.NONE,
                "attributes": [],
                "kind": None,
                "initial_value": None,
                "length": "10",
                "binding_type": None,
                "is_saved": False
            },
            "str2": {
                "description": "",
                "type": "CHARACTER",
                "name": "str2",
                "dimension": None,
                "polymorphism_type": PolymorphismType.NONE,
                "attributes": [],
                "kind": None,
                "initial_value": None,
                "length": "10",
                "binding_type": None,
                "is_saved": False
            }
        }
        self.assertEqual(variables, expected)

    def test_character_explicit_length_and_kind(self):
        self.fs.create_file(
            "/fake/path/test.f90",
            contents="""\
    module test_module
        character(len=20, kind=selected_char_kind('ASCII')) :: x
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
                "attributes": [],
                "kind": "selected_char_kind('ASCII')",
                "initial_value": None,
                "length": "20",
                "binding_type": None,
                "is_saved": False
            }
        }
        self.assertEqual(variables, expected)

    def test_character_kind_then_length(self):
        self.fs.create_file(
            "/fake/path/test.f90",
            contents="""\
    module test_module
        character(kind=selected_char_kind('ASCII'), len=20) :: x
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
                "attributes": [],
                "kind": "selected_char_kind('ASCII')",
                "initial_value": None,
                "length": "20",
                "binding_type": None,
                "is_saved": False
            }
        }
        self.assertEqual(variables, expected)

    def test_character_positional_len_and_kind(self):
        self.fs.create_file(
            "/fake/path/test.f90",
            contents="""\
    module test_module
        character(30, kind=selected_char_kind('ASCII')) :: x
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
                "attributes": [],
                "kind": "selected_char_kind('ASCII')",
                "initial_value": None,
                "length": "30",
                "binding_type": None,
                "is_saved": False
            }
        }
        self.assertEqual(variables, expected)

    def test_character_ambiguous_spec(self):
        self.fs.create_file(
            "/fake/path/test.f90",
            contents="""\
    module test_module
        character(selected_char_kind('ASCII')) :: x
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
                "attributes": [],
                "kind": None, # note that the first value is assumed to be the length
                "initial_value": None,
                "length": "selected_char_kind('ASCII')",
                "binding_type": None,
                "is_saved": False
            }
        }
        self.assertEqual(variables, expected)

    def test_character_non_ascii_kind(self):
        self.fs.create_file(
            "/fake/path/test.f90",
            contents="""\
    module test_module
        character(kind=selected_char_kind('ISO_10646')) :: x
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
                "attributes": [],
                "kind": "selected_char_kind('ISO_10646')",
                "initial_value": None,
                "length": "1",
                "binding_type": None,
                "is_saved": False
            }
        }
        self.assertEqual(variables, expected)

    def test_character_numeric_kind(self):
        self.fs.create_file(
            "/fake/path/test.f90",
            contents="""\
    module test_module
        character(kind=4) :: x
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
                "attributes": [],
                "kind": "4",
                "initial_value": None,
                "length": "1",
                "binding_type": None,
                "is_saved": False
            }
        }
        self.assertEqual(variables, expected)

    def test_character_ambiguous_variable_spec(self):
        self.fs.create_file(
            "/fake/path/test.f90",
            contents="""\
    module test_module
        character(char_kind) :: x
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
                "attributes": [],
                "kind": None,
                "initial_value": None,
                "length": "char_kind",
                "binding_type": None,
                "is_saved": False
            }
        }
        self.assertEqual(variables, expected)

    @unittest.skip("fparser doesn't interpret selected_char_kind('ASCII') as number so it fails")
    def test_character_kind_complex_expression(self):
        self.fs.create_file(
            "/fake/path/test.f90",
            contents="""\
    module test_module
        character(kind=max(selected_char_kind('ASCII'),4)) :: x
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
                "attributes": [],
                "kind": "max(selected_char_kind('ASCII'),4)",
                "initial_value": None,
                "length": "1",
                "binding_type": None
            }
        }
        self.assertEqual(variables, expected)

    def test_character_kind_named_constant(self):
        self.fs.create_file(
            "/fake/path/test.f90",
            contents="""\
    module test_module
        character(kind=ascii_kind) :: x
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
                "attributes": [],
                "kind": "ascii_kind",
                "initial_value": None,
                "length": "1",
                "binding_type": None,
                "is_saved": False
            }
        }
        self.assertEqual(variables, expected)

    def test_character_array_with_length(self):
        self.fs.create_file(
            "/fake/path/test.f90",
            contents="""\
    module test_module
        character(len=30), dimension(10) :: str_array
    end module test_module
    """,
        )
        result = extract_module_data([Path("/fake/path/test.f90")])
        module = result[0]
        variables = module["variables"]
        expected = {
            "str_array": {
                "description": "",
                "type": "CHARACTER",
                "name": "str_array",
                "dimension": {
                    "dimensions": [
                        ArrayBound(
                            bound_type=BoundType.FIXED,
                            lower=Expression(ExpressionType.LITERAL, '1'),
                            upper=Expression(ExpressionType.LITERAL, '10')
                        )
                    ]
                },
                "polymorphism_type": PolymorphismType.NONE,
                "attributes": [],
                "kind": None,
                "initial_value": None,
                "length": "30",
                "binding_type": None,
                "is_saved": False
            }
        }
        self.assertEqual(variables, expected)

    def test_character_kind_integer(self):
        self.fs.create_file(
            "/fake/path/test.f90",
            contents="""\
    module test_module
        character(kind=1) :: x
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
                "attributes": [],
                "kind": "1",
                "initial_value": None,
                "length": "1",
                "binding_type": None,
                "is_saved": False
            }
        }
        self.assertEqual(variables, expected)

    def test_character_complex_length_expression(self):
        self.fs.create_file(
            "/fake/path/test.f90",
            contents="""\
    module test_module
        character(len=2*n+1) :: str
    end module test_module
    """,
        )
        result = extract_module_data([Path("/fake/path/test.f90")])
        module = result[0]
        variables = module["variables"]
        expected = {
            "str": {
                "description": "",
                "type": "CHARACTER",
                "name": "str",
                "dimension": None,
                "polymorphism_type": PolymorphismType.NONE,
                "attributes": [],
                "kind": None,
                "initial_value": None,
                "length": "2*n+1",
                "binding_type": None,
                "is_saved": False
            }
        }
        self.assertEqual(variables, expected)

    def test_character_complex_kind_and_length(self):
        self.fs.create_file(
            "/fake/path/test.f90",
            contents="""\
    module test_module
        character(len=2*n+1, kind=merge(ascii, utf8, use_ascii)) :: str
    end module test_module
    """,
        )
        result = extract_module_data([Path("/fake/path/test.f90")])
        module = result[0]
        variables = module["variables"]
        expected = {
            "str": {
                "description": "",
                "type": "CHARACTER",
                "name": "str",
                "dimension": None,
                "polymorphism_type": PolymorphismType.NONE,
                "attributes": [],
                "kind": "merge(ascii, utf8, use_ascii)",
                "initial_value": None,
                "length": "2*n+1",
                "binding_type": None,
                "is_saved": False
            }
        }
        self.assertEqual(variables, expected)


if __name__ == "__main__":
    unittest.main()