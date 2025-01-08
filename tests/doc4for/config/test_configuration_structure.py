import unittest
from doc4for.config.configuration import DEFAULT_CONFIG

class TestConfigurationStructure(unittest.TestCase):
    """
    This test class enforces the exact structure of the configuration.
    It will fail if any changes are made to the configuration structure
    without updating this test, serving as a reminder to add proper tests
    for new functionality.
    """

    def test_config_structure(self):
        expected_structure = {
            "output_dir": "docs",
            "extensions": ["f90", "f95", "f03", "f08"],
            "output_formats": {
                "html": {
                    "enabled": True,
                    "templates": {
                        "root_dir": "templates",
                        "static": {
                            "css": "static/css",
                            "images": "static/images"
                        }
                    },
                    "syntax_highlighting": False,
                    "template_dir": None,
                    "single_page": False
                },
                "pdf": {
                    "enabled": False,
                },
                "markdown": {
                    "enabled": False,
                },
            },
            "exclude_dirs": ["docs", ".git", "__pycache__", "build", "dist"],
            "author": None,
            "title": None,
            "version": None,
            "include_private": False
        }

        # Get the actual default configuration
        actual_structure = DEFAULT_CONFIG

        # Compare structures
        self.assert_structure_matches(expected_structure, actual_structure)

    def assert_structure_matches(self, expected, actual, path=""):
        """
        Recursively compare two dictionaries to ensure they have identical structure
        """
        expected_keys = set(expected.keys())
        actual_keys = set(actual.keys())

        # Check for extra keys in actual
        extra_keys = actual_keys - expected_keys
        self.assertFalse(
            extra_keys, 
            f"Unexpected key(s) {extra_keys} found at {path}. "
            "Did you add a new configuration option without adding tests?"
        )

        # Check for missing keys in actual
        missing_keys = expected_keys - actual_keys
        self.assertFalse(
            missing_keys, 
            f"Expected key(s) {missing_keys} missing at {path}"
        )

        for key in expected_keys:
            current_path = f"{path}.{key}" if path else key

            if isinstance(expected[key], dict):
                self.assertIsInstance(
                    actual[key], dict, 
                    f"Type mismatch at {current_path}"
                )
                self.assert_structure_matches(expected[key], actual[key], current_path)
            elif isinstance(expected[key], list):
                self.assertIsInstance(
                    actual[key], list, 
                    f"Type mismatch at {current_path}"
                )
                self.assertEqual(
                    expected[key], actual[key], 
                    f"List mismatch at {current_path}"
                )
            else:
                self.assertEqual(
                    type(expected[key]), type(actual[key]), 
                    f"Type mismatch at {current_path}"
                )

if __name__ == '__main__':
    unittest.main()