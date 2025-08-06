import unittest
from pathlib import Path
from pyfakefs.fake_filesystem_unittest import TestCase
from doc4for.f90.generate_uses_tree import transform_uses_to_html_references
from doc4for.f90.generate_module_tree import extract_module_data


class TestUsesLinkTransformation(TestCase):
    def setUp(self):
        self.setUpPyfakefs()

    def test_simple_uses(self):
        self.fs.create_file(
            "/fake/path/constants.f90",
            contents="""\
    module constants
    use iso_fortran_env

    real(real64), parameter :: PI = 3.14159
    real(real64), parameter :: E = 2.71828
    real(real64), parameter :: AVOGADROS_NUMBER = 6.022e23
    end module constants
    """)
        self.fs.create_file(
            "/fake/path/maths.f90",
            contents="""\
    module maths
        use iso_fortran_env
        use constants
    end module maths
    """)

        descriptions = extract_module_data([
            Path("/fake/path/constants.f90"),
            Path("/fake/path/maths.f90"),
            ])
        transform_uses_to_html_references(descriptions)
        
        # Verify that the module references are correctly transformed
        for module in descriptions:
            if module["module_name"] == "maths":
                # Check that constants module is linked correctly
                self.assertEqual(
                    module["uses"]["constants"]["module_name"],
                    "constants.html"
                )
                # Check that iso_fortran_env is not linked (external module)
                self.assertEqual(
                    module["uses"]["iso_fortran_env"]["module_name"],
                    ""
                )

    def test_simple_only(self):
        self.fs.create_file(
            "/fake/path/constants.f90",
            contents="""\
    module constants
    use iso_fortran_env

    real(real64), parameter :: PI = 3.14159
    real(real64), parameter :: E = 2.71828
    real(real64), parameter :: AVOGADROS_NUMBER = 6.022e23
    end module constants
    """)
        self.fs.create_file(
            "/fake/path/maths.f90",
            contents="""\
    module maths
        use iso_fortran_env
        use constants, only: PI
    end module maths
    """)

        descriptions = extract_module_data([
            Path("/fake/path/constants.f90"),
            Path("/fake/path/maths.f90"),
            ])
        transform_uses_to_html_references(descriptions)
        
        # Verify that the module references and selections are correctly transformed
        for module in descriptions:
            if module["module_name"] == "maths":
                # Check that constants module is linked correctly
                self.assertEqual(
                    module["uses"]["constants"]["module_name"],
                    "constants.html"
                )
                # Check that the PI parameter is linked correctly
                self.assertEqual(
                    module["uses"]["constants"]["selections"][0],
                    "constants.html#parameter-PI"
                )

    def test_uses_with_renames(self):
        self.fs.create_file(
            "/fake/path/constants.f90",
            contents="""\
    module constants
    use iso_fortran_env

    real(real64), parameter :: PI = 3.14159
    real(real64), parameter :: E = 2.71828
    real(real64), parameter :: AVOGADROS_NUMBER = 6.022e23
    end module constants
    """)
        self.fs.create_file(
            "/fake/path/maths.f90",
            contents="""\
    module maths
        use iso_fortran_env, big_real => real64
        use constants, only: PI, AN => AVOGADROS_NUMBER
    end module maths
    """)

        descriptions = extract_module_data([
            Path("/fake/path/constants.f90"),
            Path("/fake/path/maths.f90"),
            ])
        transform_uses_to_html_references(descriptions)

        # Verify that the module references and renamed selections are correctly transformed
        for module in descriptions:
            if module["module_name"] == "maths":
                # Check that constants module is linked correctly
                self.assertEqual(
                    module["uses"]["constants"]["module_name"],
                    "constants.html"
                )
                # Check that the PI parameter is linked correctly
                self.assertIn(
                    "constants.html#parameter-PI",
                    module["uses"]["constants"]["selections"]
                )
                # Check that the renamed AVOGADROS_NUMBER parameter is linked correctly
                self.assertIn(
                    "constants.html#parameter-avogadros_number",
                    module["uses"]["constants"]["selections"]
                )

    def test_external_module(self):
        self.fs.create_file(
            "/fake/path/maths.f90",
            contents="""\
    module maths
        use external_module
    end module maths
    """)

        descriptions = extract_module_data([
            Path("/fake/path/maths.f90"),
            ])
        transform_uses_to_html_references(descriptions)

        # Verify that external modules are handled correctly
        for module in descriptions:
            if module["module_name"] == "maths":
                # Check that external_module has an empty module_name since it"s not in our descriptions
                self.assertEqual(
                    module["uses"]["external_module"]["module_name"],
                    ""
                )

    def test_different_item_types(self):
        # Create mock module descriptions directly instead of using extract_module_data
        # This avoids the fparser error with interface functions
        types_module = {
            "module_name": "types_module",
            "parameters": {},
            "variables": {"global_var": {}},
            "functions": {"calc_distance": {}},  # Add the function directly
            "subroutines": {"print_point": {}},
            "types": {"point": {}},
            "interfaces": [],
            "uses": {},
            "file_name": "/fake/path/types_module.f90",
            "module_description": "",
            "enums": {}
        }
        
        user_module = {
            "module_name": "user_module",
            "parameters": {},
            "variables": {},
            "functions": {},
            "subroutines": {},
            "types": {},
            "interfaces": [],
            "uses": {
                "types_module": {
                    "module_name": "",
                    "selections": ["point", "global_var", "print_point", "calc_distance"],
                    "description": None
                }
            },
            "file_name": "/fake/path/user_module.f90",
            "module_description": "",
            "enums": {}
        }
        
        descriptions = [types_module, user_module]
        transform_uses_to_html_references(descriptions)

        # Verify that different item types are linked correctly
        self.assertEqual(
            user_module["uses"]["types_module"]["module_name"],
            "types_module.html"
        )
        
        selections = user_module["uses"]["types_module"]["selections"]
        
        # Check that each item type is linked correctly
        self.assertIn("types_module.html#type-point", selections)
        self.assertIn("types_module.html#variable-global_var", selections)
        self.assertIn("types_module.html#subroutine-print_point", selections)
        self.assertIn("types_module.html#function-calc_distance", selections)


if __name__ == "__main__":
    unittest.main()