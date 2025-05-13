import unittest
from unittest import TestCase
from pathlib import Path
from pyfakefs.fake_filesystem_unittest import TestCase
from doc4for.f90.generate_module_tree import extract_module_data
from doc4for.models.common import BindingTypeEnum
from doc4for.models.variable_models import PolymorphismType
from doc4for.models.dimension_models import ArrayBound, BoundType

class TestAttributes(TestCase):
    maxDiff = None

    def setUp(self):
        self.setUpPyfakefs()

    def test_parameter_attribute(self):
        self.fs.create_file(
            "/fake/path/test.f90",
            contents="""\
    module test_module
        real, parameter :: pi = 3.14159
    end module test_module
    """,
        )
        result = extract_module_data([Path("/fake/path/test.f90")])
        module = result[0]
        self.assertEqual(len(module["variables"]), 0)
        parameters = module["parameters"]
        expected = {
            "pi": {
                "description": "",
                "type": "REAL",
                "name": "pi",
                "dimension": None,
                "polymorphism_type": PolymorphismType.NONE,
                "attributes": [],
                "kind": None,
                "value": "3.14159",
                "length": None,
                "binding_type": None
            }
        }
        self.assertEqual(parameters, expected)

    def test_save_attribute(self):
        self.fs.create_file(
            "/fake/path/test.f90",
            contents="""\
    module test_module
        real, save :: x
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
                "attributes": ["SAVE"],
                "kind": None,
                "initial_value": None,
                "length": None,
                "binding_type": None
            }
        }
        self.assertEqual(variables, expected)

    def test_target_attribute(self):
        self.fs.create_file(
            "/fake/path/test.f90",
            contents="""\
    module test_module
        real, target :: x
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
                "attributes": ["TARGET"],
                "kind": None,
                "initial_value": None,
                "length": None,
                "binding_type": None
            }
        }
        self.assertEqual(variables, expected)

    def test_pointer_attribute(self):
        self.fs.create_file(
            "/fake/path/test.f90",
            contents="""\
    module test_module
        real, pointer :: x
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
                "attributes": ["POINTER"],
                "kind": None,
                "initial_value": None,
                "length": None,
                "binding_type": None
            }
        }
        self.assertEqual(variables, expected)

    def test_allocatable_attribute(self):
        self.fs.create_file(
            "/fake/path/test.f90",
            contents="""\
    module test_module
        real, allocatable :: x(:)
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
                "polymorphism_type": PolymorphismType.NONE,
                "attributes": ["allocatable"],
                "kind": None,
                "initial_value": None,
                "length": None,
                "binding_type": None
            }
        }
        self.assertEqual(variables, expected)

    def test_intent_attributes(self):
        intent_types = ['in', 'out', 'inout']
        for intent in intent_types:
            self.fs.create_file(
                f"/fake/path/test_{intent}.f90",
                contents=f"""\
    module test_module
        contains
        subroutine test_sub(x)
            real, intent({intent}) :: x
        end subroutine test_sub
    end module test_module
    """,
            )
            result = extract_module_data([Path(f"/fake/path/test_{intent}.f90")])
            module = result[0]
            subroutine = module["subroutines"]["test_sub"]
            arguments = subroutine["arguments"]
            expected = {
                "x": {
                    "description": "",
                    "type": "REAL",
                    "name": "x",
                    "dimension": None,
                    "polymorphism_type": PolymorphismType.NONE,
                    "attributes": [f"intent({intent})"],
                    "kind": None,
                    "initial_value": None,
                    "length": None,
                    "binding_type": None
                }
            }
            self.assertEqual(arguments, expected)

    def test_optional_attribute(self):
        self.fs.create_file(
            "/fake/path/test.f90",
            contents="""\
    module test_module
        contains
        subroutine test_sub(x)
            real, optional :: x
        end subroutine test_sub
    end module test_module
    """,
        )
        result = extract_module_data([Path("/fake/path/test.f90")])
        module = result[0]
        subroutine = module["subroutines"]["test_sub"]
        arguments = subroutine["arguments"]
        expected = {
            "x": {
                "description": "",
                "type": "REAL",
                "name": "x",
                "dimension": None,
                "polymorphism_type": PolymorphismType.NONE,
                "attributes": ["optional"],
                "kind": None,
                "initial_value": None,
                "length": None,
                "binding_type": None
            }
        }
        self.assertEqual(arguments, expected)

    def test_public_private_attributes(self):
        for visibility in ['public', 'private']:
            self.fs.create_file(
                f"/fake/path/test_{visibility}.f90",
                contents=f"""\
    module test_module
        real, {visibility} :: x
    end module test_module
    """,
            )
            result = extract_module_data([Path(f"/fake/path/test_{visibility}.f90")])
            module = result[0]
            variables = module["variables"]
            expected = {
                "x": {
                    "description": "",
                    "type": "REAL",
                    "name": "x",
                    "dimension": None,
                    "polymorphism_type": PolymorphismType.NONE,
                    "attributes": [visibility.upper()],
                    "kind": None,
                    "initial_value": None,
                    "length": None,
                    "binding_type": None
                }
            }
            self.assertEqual(variables, expected)

    def test_external_attribute_procedure(self):
        self.fs.create_file(
            "/fake/path/test.f90",
            contents="""\
    module test_module
        external :: mysub
    end module test_module
    """,
        )
        result = extract_module_data([Path("/fake/path/test.f90")])
        module = result[0]
        # External procedures might be stored differently - adjust based on your implementation
        # This test may need modification based on how your code handles external procedures

    def test_intrinsic_attribute_procedure(self):
        self.fs.create_file(
            "/fake/path/test.f90",
            contents="""\
    module test_module
        intrinsic :: sin
    end module test_module
    """,
        )
        result = extract_module_data([Path("/fake/path/test.f90")])
        module = result[0]
        # Intrinsic procedures might be stored differently - adjust based on your implementation
        # This test may need modification based on how your code handles intrinsic procedures

    def test_multiple_attributes_subroutine_arg(self):
        self.fs.create_file(
            "/fake/path/test.f90",
            contents="""\
    module test_module
        contains
        subroutine test_sub(x)
            real, intent(in), optional :: x
        end subroutine test_sub
    end module test_module
    """,
        )
        result = extract_module_data([Path("/fake/path/test.f90")])
        module = result[0]
        subroutine = module["subroutines"]["test_sub"]
        arguments = subroutine["arguments"]
        expected = {
            "x": {
                "description": "",
                "type": "REAL",
                "name": "x",
                "dimension": None,
                "polymorphism_type": PolymorphismType.NONE,
                "attributes": ["intent(in)", "optional"],
                "kind": None,
                "initial_value": None,
                "length": None,
                "binding_type": None
            }
        }
        self.assertEqual(arguments, expected)

    def test_multiple_attributes_module_var(self):
        self.fs.create_file(
            "/fake/path/test.f90",
            contents="""\
    module test_module
        real, save, private, target :: x
    end module test_module
    """,
        )
        result = extract_module_data([Path("/fake/path/test.f90")])
        module = result[0]
        variables = module["variables"]
        self.assertEqual(variables["x"]["type"], "REAL")
        self.assertEqual(variables["x"]["name"], "x")
        self.assertCountEqual(variables["x"]["attributes"], ["SAVE", "TARGET", "PRIVATE"])

    def test_multiple_variables_with_attributes(self):
        self.fs.create_file(
            "/fake/path/test.f90",
            contents="""\
    module test_module
        real, parameter :: x = 1.0, y = 2.0
    end module test_module
    """,
        )
        result = extract_module_data([Path("/fake/path/test.f90")])
        module = result[0]
        parameters = module["parameters"]
        expected = {
            "x": {
                "description": "",
                "type": "REAL",
                "name": "x",
                "dimension": None,
                "polymorphism_type": PolymorphismType.NONE,
                "attributes": [],
                "kind": None,
                "value": "1.0",
                "length": None,
                "binding_type": None
            },
            "y": {
                "description": "",
                "type": "REAL",
                "name": "y",
                "dimension": None,
                "polymorphism_type": PolymorphismType.NONE,
                "attributes": [],
                "kind": None,
                "value": "2.0",
                "length": None,
                "binding_type": None
            }
        }
        self.assertEqual(parameters, expected)

if __name__ == "__main__":
    unittest.main()