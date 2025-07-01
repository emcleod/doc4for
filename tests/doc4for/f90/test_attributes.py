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
                "binding_type": None,
                "is_saved": False
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
                "binding_type": None,
                "is_saved": False
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
                "binding_type": None,
                "is_saved": False
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
                    "dimensions": [ArrayBound(BoundType.ASSUMED_SHAPE)]
                },
                "polymorphism_type": PolymorphismType.NONE,
                "attributes": ["ALLOCATABLE"],
                "kind": None,
                "initial_value": None,
                "length": None,
                "binding_type": None,
                "is_saved": False
            }
        }
        self.assertEqual(variables, expected)

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
        self.assertEqual(arguments, ["x"])
        in_arguments = subroutine["in"]
        out_arguments = subroutine["out"]
        expected = {
            "x": {
                "description": "",
                "type": "REAL",
                "dimension": None,
                "attributes": ["OPTIONAL"],
                "kind": None,
                "default_value": None,
                "length": None,
                "interface_name": None,
                "enum_type": None,
                "polymorphism_type": PolymorphismType.NONE
            }
        }
        self.assertEqual(in_arguments, expected)
        self.assertEqual(out_arguments, expected)

    def test_public_private_attributes(self):
        for visibility in ["public", "private"]:
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
                    "binding_type": None,
                    "is_saved": False
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
        self.assertEqual(subroutine["arguments"], ["x"])
        arguments = subroutine["in"]
        expected = {
            "x": {
                "description": "",
                "type": "REAL",
                "dimension": None,
                "attributes": ["OPTIONAL"],
                "kind": None,
                "default_value": None,
                "length": None,
                "interface_name": None,
                "enum_type": None,
                "polymorphism_type": PolymorphismType.NONE
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

    def test_multiple_attributes_function_argument(self):
        self.fs.create_file(
            "/fake/path/test.f90",
            contents="""\
    module test_module

      contains
        function test_f(x, y, z) result(res)
            real, save, target :: x
            character(len=10), optional, intent(in) :: y
            real(kind=real64), pointer, intent(out) :: z
            real :: res
        end function test_f
    end module test_module
    """,
        )
        result = extract_module_data([Path("/fake/path/test.f90")])
        module = result[0]
        function = module["functions"]["test_f"]
        self.assertEqual(function["arguments"], ["x", "y", "z"])
        in_arguments = function["in"]
        out_arguments = function["out"]
        expected_x = {
            "type": "REAL",
            "kind": None,
            "length": None,
            "description": "",
            "dimension": None,
            "attributes": ["SAVE", "TARGET"],
            "default_value": None,
            "interface_name": None,
            "enum_type": None,
            "polymorphism_type": PolymorphismType.NONE
        }
        expected_y = {
            "type": "CHARACTER",
            "kind": None,
            "length": "10",
            "description": "",
            "dimension": None,
            "attributes": ["OPTIONAL"],
            "default_value": None,
            "interface_name": None,
            "enum_type": None,
            "polymorphism_type": PolymorphismType.NONE
        }
        expected_z = {
            "type": "REAL",
            "kind": "real64",
            "length": None,
            "description": "",
            "dimension": None,
            "attributes": ["POINTER"],
            "default_value": None,
            "interface_name": None,
            "enum_type": None,
            "polymorphism_type": PolymorphismType.NONE
        }
        self.assertEqual(in_arguments["x"], expected_x)
        self.assertEqual(in_arguments["y"], expected_y)
        self.assertEqual(out_arguments["x"], expected_x)
        self.assertEqual(out_arguments["z"], expected_z)

    def test_multiple_attributes_subroutine_argument(self):
        self.fs.create_file(
            "/fake/path/test.f90",
            contents="""\
    module test_module

      contains
          subroutine test_defaults(a, b, c, d, e, f)
            integer, intent(in) :: a
            real, intent(out) :: b
            character(*), intent(inout) :: c
            integer, optional, intent(in) :: d = 42
            real, dimension(:), allocatable, intent(out) :: e
            logical, parameter :: f = .true.
          end subroutine
    end module test_module
    """,
        )
        result = extract_module_data([Path("/fake/path/test.f90")])
        module = result[0]
        subroutine = module["subroutines"]["test_defaults"]
        self.assertEqual(subroutine["arguments"], ["a", "b", "c", "d", "e", "f"])
        in_arguments = subroutine["in"]
        out_arguments = subroutine["out"]
        expected_a = {
            "type": "INTEGER",
            "kind": None,
            "length": None,
            "description": "",
            "dimension": None,
            "attributes": [],
            "default_value": None,
            "interface_name": None,
            "enum_type": None,
            "polymorphism_type": PolymorphismType.NONE
        }
        expected_b = {
            "type": "REAL",
            "kind": None,
            "length": None,
            "description": "",
            "dimension": None,
            "attributes": [],
            "default_value": None,
            "interface_name": None,
            "enum_type": None,
            "polymorphism_type": PolymorphismType.NONE
        }
        expected_c = {
            "type": "CHARACTER",
            "kind": None,
            "length": "*",
            "description": "",
            "dimension": None,
            "attributes": [],
            "default_value": None,
            "interface_name": None,
            "enum_type": None,
            "polymorphism_type": PolymorphismType.NONE
        }
        expected_d = {
            "type": "INTEGER",
            "kind": None,
            "length": None,
            "description": "",
            "dimension": None,
            "attributes": ["OPTIONAL"],
            "default_value": "42",
            "interface_name": None,
            "enum_type": None,
            "polymorphism_type": PolymorphismType.NONE
        }
        expected_e = {
            "type": "REAL",
            "kind": None,
            "length": None,
            "description": "",
            "dimension": {"dimensions": [ArrayBound(BoundType.ASSUMED_SHAPE)]},
            "attributes": ["ALLOCATABLE"],
            "default_value": None,
            "interface_name": None,
            "enum_type": None,
            "polymorphism_type": PolymorphismType.NONE
        }
        expected_f = {
            "type": "LOGICAL",
            "kind": None,
            "length": None,
            "description": "",
            "dimension": None,
            "attributes": ["PARAMETER"],
            "default_value": ".true.",
            "interface_name": None,
            "enum_type": None,
            "polymorphism_type": PolymorphismType.NONE
        }
        self.assertEqual(in_arguments["a"], expected_a)
        self.assertEqual(in_arguments["c"], expected_c)
        self.assertEqual(in_arguments["d"], expected_d)
        self.assertEqual(in_arguments["f"], expected_f)
        self.assertEqual(out_arguments["b"], expected_b)
        self.assertEqual(out_arguments["c"], expected_c)
        self.assertEqual(out_arguments["e"], expected_e)

if __name__ == "__main__":
    unittest.main()