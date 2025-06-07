import unittest
from unittest import TestCase
from pathlib import Path
from pyfakefs.fake_filesystem_unittest import TestCase
from doc4for.f90.generate_module_tree import extract_module_data
from doc4for.models.common import Expression, ExpressionType
from doc4for.models.dimension_models import ArrayBound, BoundType
from doc4for.models.variable_models import PolymorphismType

def create_dimension_expr(lower, upper):
    return ArrayBound(
        bound_type=BoundType.FIXED,
        lower=Expression(expr_type=ExpressionType.LITERAL, value=str(lower), function_name=None, arguments=None),
        upper=Expression(expr_type=ExpressionType.LITERAL, value=str(upper), function_name=None, arguments=None)
    )

class TestComplexCases(TestCase):
    maxDiff = None

    def setUp(self):
        self.setUpPyfakefs()

    def test_old_style_multiple_variable_types(self):
        self.fs.create_file(
            "/fake/path/test.f90",
            contents="""\
    module test_module
        real x(10, 20), y, z(5)
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
                "dimension": {'dimensions': [
                    create_dimension_expr(1, 10),
                    create_dimension_expr(1, 20)
                ]},
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
                "dimension": {'dimensions': [create_dimension_expr(1, 5)]},
                "polymorphism_type": PolymorphismType.NONE,
                "attributes": [],
                "kind": None,
                "initial_value": None,
                "length": None,
                "binding_type": None
            }
        }
        self.assertEqual(variables, expected)

    def test_multiple_attributes_with_initialization_and_array(self):
        self.fs.create_file(
            "/fake/path/test.f90",
            contents="""\
    module test_module
        real(8), dimension(-5:5), save :: array = 0.0
    end module test_module
    """,
        )
        result = extract_module_data([Path("/fake/path/test.f90")])
        module = result[0]
        variables = module["variables"]
        expected = {
            "array": {
                "description": "",
                "type": "REAL",
                "name": "array",
                "dimension": {'dimensions': [create_dimension_expr(-5, 5)]},
                "polymorphism_type": PolymorphismType.NONE,
                "attributes": ["SAVE"],
                "kind": '8',
                "initial_value": "0.0",
                "length": None,
                "binding_type": None
            }
        }
        self.assertEqual(variables, expected)

    def test_multiple_attributes_with_initialization(self):
        self.fs.create_file(
            "/fake/path/test.f90",
            contents="""\
    module test_module
        real(kind=8), parameter, public :: pi = 3.14159265359
    end module test_module
    """,
        )
        result = extract_module_data([Path("/fake/path/test.f90")])
        module = result[0]
        parameters = module["parameters"]
        expected = {
            "pi": {
                "description": "",
                "type": "REAL",
                "name": "pi",
                "dimension": None,
                "polymorphism_type": PolymorphismType.NONE,
                "attributes": ["PUBLIC"],
                "kind": '8',
                "value": "3.14159265359",
                "length": None,
                "binding_type": None
            }
        }
        self.assertEqual(parameters, expected)

    def test_multiple_variables_different_initializations(self):
        self.fs.create_file(
            "/fake/path/test.f90",
            contents="""\
    module test_module
        integer, parameter :: x = 1, y = 2*x, z = y**2
    end module test_module
    """,
        )
        result = extract_module_data([Path("/fake/path/test.f90")])
        module = result[0]
        parameters = module["parameters"]
        expected = {
            "x": {
                "description": "",
                "type": "INTEGER",
                "name": "x",
                "dimension": None,
                "polymorphism_type": PolymorphismType.NONE,
                "attributes": [],
                "kind": None,
                "value": "1",
                "length": None,
                "binding_type": None
            },
            "y": {
                "description": "",
                "type": "INTEGER",
                "name": "y",
                "dimension": None,
                "polymorphism_type": PolymorphismType.NONE,
                "attributes": [],
                "kind": None,
                "value": "2 * x",
                "length": None,
                "binding_type": None
            },
            "z": {
                "description": "",
                "type": "INTEGER",
                "name": "z",
                "dimension": None,
                "polymorphism_type": PolymorphismType.NONE,
                "attributes": [],
                "kind": None,
                "value": "y ** 2",
                "length": None,
                "binding_type": None
            }
        }
        self.assertEqual(parameters, expected)

    def test_complex_initialization_expression(self):
        self.fs.create_file(
            "/fake/path/test.f90",
            contents="""\
    module test_module
        real :: x = 2.0 * sin(3.14159/2.0) ** 2
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
                "initial_value": "2.0 * SIN(3.14159 / 2.0) ** 2",
                "length": None,
                "binding_type": None
            }
        }
        self.assertEqual(variables, expected)

    def test_mixed_variables_types_and_attributes(self):
        self.fs.create_file(
            "/fake/path/test.f90",
            contents="""\
    module test_module
        contains
        subroutine test_sub(x, y, z)
            real(8), intent(in) :: x, y(10), z
            z = 1.0
        end subroutine test_sub
    end module test_module
    """,
        )
        result = extract_module_data([Path("/fake/path/test.f90")])
        module = result[0]
        subroutine = module["subroutines"]["test_sub"]
        arguments = subroutine["arguments"]
        self.assertEqual(arguments, ["x", "y", "z"])
        expected = {
            "x": {
                "description": "",
                "type": "REAL",
                "dimension": None,
                "polymorphism_type": PolymorphismType.NONE,
                "attributes": [],
                "kind": '8',
                "default_value": None,
                "length": None,
                "interface_name": None,
                "enum_type": None
            },
            "y": {
                "description": "",
                "type": "REAL",
                "dimension": {'dimensions': [create_dimension_expr(1, 10)]},
                "polymorphism_type": PolymorphismType.NONE,
                "attributes": [],
                "kind": '8',
                "default_value": None,
                "length": None,
                "interface_name": None,
                "enum_type": None
            },
            "z": {
                "description": "",
                "type": "REAL",
                "dimension": None,
                "polymorphism_type": PolymorphismType.NONE,
                "attributes": [],
                "kind": '8',
                "default_value": None,
                "length": None,
                "interface_name": None,
                "enum_type": None
            }
        }
        self.assertEqual(subroutine["in"], expected)

    def test_all_features_combined(self):
        self.fs.create_file(
            "/fake/path/test.f90",
            contents="""\
    module test_module
        real(kind=selected_real_kind(15)), dimension(10), public :: x = reshape([1,2,3,4,5,6,7,8,9,10], [10])
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
                "dimension": {'dimensions': [create_dimension_expr(1, 10)]},
                "polymorphism_type": PolymorphismType.NONE,
                "attributes": ["PUBLIC"],
                "kind": 'selected_real_kind(15)',
                "initial_value": "RESHAPE([1, 2, 3, 4, 5, 6, 7, 8, 9, 10], [10])",
                "length": None,
                "binding_type": None
            }
        }
        self.assertEqual(variables, expected)

    def test_multiple_variables_with_different_properties(self):
        self.fs.create_file(
            "/fake/path/test.f90",
            contents="""\
    module test_module
        real*8 x(10,10), y(5), z = 1.0
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
                "dimension": {'dimensions': [
                    create_dimension_expr(1, 10),
                    create_dimension_expr(1, 10)
                ]},
                "polymorphism_type": PolymorphismType.NONE,
                "attributes": [],
                "kind": "8",
                "initial_value": None,
                "length": None,
                "binding_type": None
            },
            "y": {
                "description": "",
                "type": "REAL",
                "name": "y",
                "dimension": {'dimensions': [
                    create_dimension_expr(1, 5),
                ]},
                "polymorphism_type": PolymorphismType.NONE,
                "attributes": [],
                "kind": "8",
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
                "kind": "8",
                "initial_value": "1.0",
                "length": None,
                "binding_type": None
            }
        }
        self.assertEqual(variables, expected)

    def test_mixed_attributes(self):
        self.fs.create_file(
            "/fake/path/test.f90",
            contents="""\
    module test_module
        real(kind=8), dimension(10), public :: array
    end module test_module
    """,
        )
        result = extract_module_data([Path("/fake/path/test.f90")])
        module = result[0]
        variables = module["variables"]
        expected = {
            "array": {
                "description": "",
                "type": "REAL",
                "name": "array",
                "dimension": {'dimensions': [create_dimension_expr(1, 10)]},
                "polymorphism_type": PolymorphismType.NONE,
                "attributes": ["PUBLIC"],
                "kind": "8",
                "initial_value": None,
                "length": None,
                "binding_type": None
            }
        }
        self.assertEqual(variables, expected)

if __name__ == "__main__":
    unittest.main()

