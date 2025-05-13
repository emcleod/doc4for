import unittest
from unittest import TestCase
from pathlib import Path
from pyfakefs.fake_filesystem_unittest import TestCase
from doc4for.f90.generate_module_tree import extract_module_data
from doc4for.models.common import BindingTypeEnum
from doc4for.models.variable_models import PolymorphismType

class TestBasicDeclarations(TestCase):
    maxDiff = None

    def setUp(self):
        self.setUpPyfakefs()

    def test_old_style_one_variable(self):
        self.fs.create_file(
            "/fake/path/test.f90",
            contents="""\
    module test_module
        REAL x
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
                "attributes": [],
                "kind": None,
                "initial_value": None,
                "length": None,
                "binding_type": None
            }
        }
        self.assertEqual(variables, expected)

    def test_f90_style_one_variable(self):
        self.fs.create_file(
            "/fake/path/test.f90",
            contents="""\
    module test_module
        REAL :: x
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
                "attributes": [],
                "kind": None,
                "initial_value": None,
                "length": None,
                "binding_type": None
            }
        }
        self.assertEqual(variables, expected)

    def test_old_style_multiple_variables(self):
        self.fs.create_file(
            "/fake/path/test.f90",
            contents="""\
    module test_module
        REAL x, y
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
                "attributes": [],
                "polymorphism_type": PolymorphismType.NONE,
                "kind": None,
                "initial_value": None,
                "length": None,
                "binding_type": None
            },
            "y": {
                "description": "",
                "type": "REAL",
                "name": "y",
                "dimension": None,
                "attributes": [],
                "polymorphism_type": PolymorphismType.NONE,
                "kind": None,
                "initial_value": None,
                "length": None,
                "binding_type": None
            }
        }
        self.assertEqual(variables, expected)

    def test_f90_style_multiple_variables(self):
        self.fs.create_file(
            "/fake/path/test.f90",
            contents="""\
    module test_module
        REAL :: x, y, z
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
                "attributes": [],
                "kind": None,
                "initial_value": None,
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
                "initial_value": None,
                "length": None,
                "binding_type": None
            },
            "z": {
                "description": "",
                "type": "REAL",
                "name": "z",
                "dimension": None,
                "polymorphism_type": PolymorphismType.NONE,
                "attributes": [],
                "kind": None,
                "initial_value": None,
                "length": None,
                "binding_type": None
            }
        }
        self.assertEqual(variables, expected)

    def test_old_style_one_initialised_variable(self):
        self.fs.create_file(
            "/fake/path/test.f90",
            contents="""\
    module test_module
        REAL x = 10
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
                "attributes": [],
                "kind": None,
                "initial_value": "10",
                "length": None,
                "binding_type": None
            }
        }
        self.assertEqual(variables, expected)

    def test_f90_style_one_initialised_variable(self):
        self.fs.create_file(
            "/fake/path/test.f90",
            contents="""\
    module test_module
        REAL :: x = 1.4
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
                "attributes": [],
                "kind": None,
                "initial_value": "1.4",
                "length": None,
                "binding_type": None
            }
        }
        self.assertEqual(variables, expected)

    def test_old_style_multiple_initialised_variables(self):
        self.fs.create_file(
            "/fake/path/test.f90",
            contents="""\
    module test_module
        REAL x = 10, y = 20
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
                "attributes": [],
                "polymorphism_type": PolymorphismType.NONE,
                "kind": None,
                "initial_value": "10",
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
                "initial_value": "20",
                "length": None,
                "binding_type": None
            }
        }
        self.assertEqual(variables, expected)

    def test_f90_style_multiple_initialised_variables(self):
        self.fs.create_file(
            "/fake/path/test.f90",
            contents="""\
    module test_module
        REAL :: x = 2, y = 4, z = 49
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
                "attributes": [],
                "kind": None,
                "initial_value": "2",
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
                "initial_value": "4",
                "length": None,
                "binding_type": None
            },
            "z": {
                "description": "",
                "type": "REAL",
                "name": "z",
                "polymorphism_type": PolymorphismType.NONE,
                "dimension": None,
                "attributes": [],
                "kind": None,
                "initial_value": "49",
                "length": None,
                "binding_type": None
            }
        }
        self.assertEqual(variables, expected)

    def test_different_data_types(self):
        for type_name in ["INTEGER", "LOGICAL", "COMPLEX", "DOUBLE PRECISION"]:
            self.fs.create_file(
                f"/fake/path/{type_name.lower().replace(' ', '_')}.f90",
                contents=f"""\
    module test_module
        {type_name} :: x
    end module test_module
    """,
            )
            result = extract_module_data([Path(f"/fake/path/{type_name.lower().replace(' ', '_')}.f90")])
            module = result[0]
            variables = module["variables"]
            expected = {
                "x": {
                    "description": "",
                    "type": type_name,
                    "name": "x",
                    "dimension": None,
                    "polymorphism_type": PolymorphismType.NONE,
                    "attributes": [],
                    "kind": None,
                    "initial_value": None,
                    "length": None,
                    "binding_type": None
                }
            }
            self.assertEqual(variables, expected)

    def test_complex_initialization_values(self):
        self.fs.create_file(
            "/fake/path/test.f90",
            contents="""\
    module test_module
        COMPLEX :: z = (1.0, 2.0)
    end module test_module
    """,
        )
        result = extract_module_data([Path("/fake/path/test.f90")])
        module = result[0]
        variables = module["variables"]
        expected = {
            "z": {
                "description": "",
                "type": "COMPLEX",
                "name": "z",
                "dimension": None,
                "polymorphism_type": PolymorphismType.NONE,
                "attributes": [],
                "kind": None,
                "initial_value": "(1.0, 2.0)",
                "length": None,
                "binding_type": None
            }
        }
        self.assertEqual(variables, expected)

    def test_initialization_values(self):
        test_cases = [
            ("REAL", "x", "2.0 * 3.14159"),
            ("INTEGER", "i", "1 + 2"),
            ("LOGICAL", "flag", ".TRUE."),
            ("COMPLEX", "c", "(1.0, 2.0)"),
        ]
        
        for type_name, var_name, init_value in test_cases:
            self.fs.create_file(
                f"/fake/path/{type_name.lower()}_{var_name}.f90",
                contents=f"""\
    module test_module
        {type_name} :: {var_name} = {init_value}
    end module test_module
    """,
            )
            result = extract_module_data([Path(f"/fake/path/{type_name.lower()}_{var_name}.f90")])
            module = result[0]
            variables = module["variables"]
            self.assertEqual(variables[var_name]["initial_value"], init_value)

    def test_c_binding_variable(self):
        self.fs.create_file(
            "/fake/path/test.f90",
            contents="""\
    module c_interop
        use iso_c_binding
        integer(c_int), bind(c) :: global_var
    end module c_interop
    """,
        )
        result = extract_module_data([Path("/fake/path/test.f90")])
        module = result[0]
        variables = module["variables"]
        expected = {
            "global_var": {
                "description": "",
                "type": "INTEGER",
                "name": "global_var",
                "dimension": None,
                "polymorphism_type": PolymorphismType.NONE,
                "attributes": [],
                "kind": "c_int",
                "initial_value": None,
                "length": None,
                "binding_type": {"type": BindingTypeEnum.BIND_C }
            }
        }
        self.assertEqual(variables, expected)

    def test_c_binding_with_name(self):
        self.fs.create_file(
            "/fake/path/test.f90",
            contents="""\
    module c_interop
        use iso_c_binding
        REAL(c_double), bind(c, name="my_c_var") :: fortran_var
    end module c_interop
    """,
        )
        result = extract_module_data([Path("/fake/path/test.f90")])
        module = result[0]
        variables = module["variables"]
        expected = {
            "fortran_var": {
                "description": "",
                "type": "REAL",
                "name": "fortran_var",
                "dimension": None,
                "attributes": [],
                "polymorphism_type": PolymorphismType.NONE,
                "kind": "c_double",
                "initial_value": None,
                "length": None,
                "binding_type": {"type": BindingTypeEnum.BIND_C, "name": '"my_c_var"'}
            }
        }
        self.assertEqual(variables, expected)

if __name__ == "__main__":
    unittest.main()