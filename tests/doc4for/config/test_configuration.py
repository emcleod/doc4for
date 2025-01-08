import unittest
import json
import copy
from pathlib import Path
from unittest.mock import patch, mock_open
from doc4for.config.configuration import (
    load_configuration,
    validate_config,
    normalize_dirs,
    normalize_extensions,
    normalize_path,
    DEFAULT_CONFIG)


class TestConfiguration(unittest.TestCase):

    def setUp(self):
        """Create a deep copy of the default configuration for each test"""
        self.default_config = copy.deepcopy(DEFAULT_CONFIG)
        self.patcher = patch('doc4for.config.configuration.normalize_path')
        self.mock_normalize = self.patcher.start()
        self.mock_normalize.side_effect = lambda x: f"/mock/path/{
            x}" if not x.startswith('/') else x

    def tearDown(self):
        self.patcher.stop()

    def deep_copy_config(self):
        """Helper method to create a deep copy of the default config"""
        return copy.deepcopy(self.default_config)

    def mock_directory_exists(self, directories):
        """Helper method to create a mock for directory existence"""
        mock_dirs = [f"/mock/path/{d}" for d in directories]

        def mock_is_dir(self):
            return str(self) in mock_dirs
        return mock_is_dir

    def setup_mock_file(self, content):
        """Helper method to set up file mocking"""
        return [
            patch('pathlib.Path.exists', return_value=True),
            patch('builtins.open', mock_open(read_data=content))
        ]

    def run_with_mock_file(self, content):
        """Helper method to run a test with a mock file"""
        mocks = [
            patch('pathlib.Path.exists', return_value=True),
            patch('builtins.open', mock_open(read_data=content)),
            patch('pathlib.Path.is_dir', return_value=True)
        ]
        for mock in mocks:
            mock.start()
        try:
            return load_configuration()
        finally:
            for mock in mocks:
                mock.stop()

    def test_load_configuration_defaults(self):
        """Test that default configuration is used when no file exists"""
        # Get the actual paths from DEFAULT_CONFIG
        root_dir = self.default_config["output_formats"]["html"]["templates"]["root_dir"]
        css_dir = self.default_config["output_formats"]["html"]["templates"]["static"]["css"]
        images_dir = self.default_config["output_formats"]["html"]["templates"]["static"]["images"]

        # Mock these specific paths
        directories = [root_dir, css_dir, images_dir]
        
        def mock_exists(path):
            return str(path) in directories

        def mock_is_dir(path):
            return str(path) in directories

        # Mock that the config file doesn't exist and directories do exist
        with patch('pathlib.Path.exists', new=mock_exists), \
            patch('pathlib.Path.is_dir', new=mock_is_dir):
            config = load_configuration()
            
            # Use a fresh copy of DEFAULT_CONFIG as expected
            expected_config = self.deep_copy_config()
            
            self.maxDiff = None
            self.assertEqual(config, expected_config)
                        
    def test_load_configuration_with_user_file(self):
        """Test that user configuration overrides defaults"""
        user_config = {
            "output_dir": "documentation",
            "output_formats": {
                "html": {
                    "templates": {
                        "root_dir": "custom_templates",
                        "static": {
                            "css": "custom/css/path"
                        }
                    },
                    "single_page": True
                }
            },
            "author": "Test Author",
            "unknown_key": "should be ignored"
        }

        expected_config = self.deep_copy_config()
        expected_config["output_dir"] = normalize_path("documentation")
        expected_config["output_formats"]["html"]["templates"]["root_dir"] = normalize_path(
            "custom_templates")
        expected_config["output_formats"]["html"]["templates"]["static"]["css"] = normalize_path(
            "custom/css/path")
        expected_config["output_formats"]["html"]["single_page"] = True
        expected_config["author"] = "Test Author"

        directories = [
            'custom_templates',
            'custom_templates/custom/css/path',
            'custom_templates/static/images',
            'templates', 'templates/static/css', 'templates/static/images'
        ]
        mock_is_dir = self.mock_directory_exists(directories)

        with patch('pathlib.Path.is_dir', new=mock_is_dir), \
                self.assertLogs(level='WARNING') as cm:
            config = self.run_with_mock_file(json.dumps(user_config))

        self.assertEqual(config, expected_config)
        self.assertIn(
            "Unknown configuration keys found and ignored", cm.output[0])
        self.assertIn("unknown_key", cm.output[0])

        # Assert that non-overridden values remain at defaults
        self.assertEqual(config['extensions'],
                         self.default_config['extensions'])
        self.assertEqual(config['output_formats']['pdf'],
                         self.default_config['output_formats']['pdf'])

        # Verify other HTML settings weren't affected
        self.assertEqual(config['output_formats']['html']['syntax_highlighting'],
                         self.default_config['output_formats']['html']['syntax_highlighting'])

    def test_load_configuration_null_json(self):
        """Test behavior with JSON containing just 'null'"""
        with self.assertLogs(level='WARNING') as cm:
            config = self.run_with_mock_file('null')
            self.assertEqual(config, self.default_config)
            self.assertIn("Configuration file is empty", cm.output[0])

    def test_load_configuration_partial_invalid_json(self):
        """Test behavior with partially valid JSON"""
        partial_json = '{"output_dir": "docs", "invalid": }'
        with self.assertLogs(level='WARNING') as cm:
            config = self.run_with_mock_file(partial_json)
            self.assertEqual(config, self.default_config)
            self.assertIn("Error reading configuration file", cm.output[0])

    def test_load_configuration_invalid_json(self):
        """Test behavior with invalid JSON in configuration file"""
        with self.assertLogs(level='WARNING') as cm:
            config = self.run_with_mock_file('invalid json')
            self.assertEqual(config, self.default_config)
            self.assertIn("Error reading configuration file", cm.output[0])
            self.assertIn("Expecting value", cm.output[0])

    def test_required_fields(self):
        """Test that configuration fails if required fields are missing"""
        required_fields = [
            ("output_dir", {"extensions": [
             "f90"], "output_formats": {}, "exclude_dirs": []}),
            ("extensions", {"output_dir": "docs",
             "output_formats": {}, "exclude_dirs": []}),
            ("output_formats", {"output_dir": "docs",
             "extensions": ["f90"], "exclude_dirs": []}),
            ("exclude_dirs", {"output_dir": "docs",
             "extensions": ["f90"], "output_formats": {}})
        ]

        for missing_field, incomplete_config in required_fields:
            with self.subTest(missing_field=missing_field):
                with self.assertRaisesRegex(ValueError, f"Missing required configuration keys:.*{missing_field}"):
                    validate_config(incomplete_config)

    def test_required_output_formats(self):
        """Test that configuration fails if required output formats are missing"""
        required_formats = ["html", "pdf", "markdown"]

        for format_name in required_formats:
            with self.subTest(format=format_name):
                config = {
                    "output_dir": "docs",
                    "extensions": ["f90"],
                    "exclude_dirs": [],
                    "output_formats": {f: {} for f in required_formats if f != format_name}
                }
                with self.assertRaisesRegex(ValueError, f"Missing required output formats:.*{format_name}"):
                    validate_config(config)

    def test_html_templates_structure(self):
        """Test HTML templates structure is correct"""
        test_cases = [
            ("root_dir", "Missing required templates keys"),
            ("static", "Missing required templates keys"),
            (("static", "css"), "Missing required static configuration keys"),
            (("static", "images"), "Missing required static configuration keys")
        ]

        for field, error_pattern in test_cases:
            with self.subTest(missing_field=field):
                config = self.deep_copy_config()

                if isinstance(field, tuple):
                    parent, child = field
                    del config["output_formats"]["html"]["templates"][parent][child]
                else:
                    del config["output_formats"]["html"]["templates"][field]

                with self.assertRaisesRegex(ValueError, error_pattern):
                    validate_config(config)

    def test_other_format_enabled_field(self):
        """Test that PDF and Markdown formats have 'enabled' field"""
        with patch('pathlib.Path.exists', return_value=True), \
                patch('pathlib.Path.is_dir', return_value=True):
            for format_name in ["pdf", "markdown"]:
                with self.subTest(format=format_name):
                    config = self.deep_copy_config()
                    del config["output_formats"][format_name]["enabled"]

                    with self.assertRaisesRegex(ValueError,
                                                f"{format_name.upper()} format configuration missing 'enabled' field"):
                        validate_config(config)

    def test_load_configuration_with_user_file(self):
        """Test that user configuration overrides defaults"""
        user_config = {
            "output_dir": "documentation",
            "output_formats": {
                "html": {
                    "templates": {
                        "root_dir": "custom_templates",
                        "static": {
                            "css": "custom/css/path"
                        }
                    },
                    "single_page": True
                }
            },
            "author": "Test Author",
            "unknown_key": "should be ignored"
        }

        expected_config = self.deep_copy_config()
        expected_config["output_dir"] = "/mock/path/documentation"
        expected_config["output_formats"]["html"]["templates"]["root_dir"] = "/mock/path/custom_templates"
        expected_config["output_formats"]["html"]["templates"]["static"]["css"] = "/mock/path/custom/css/path"
        expected_config["output_formats"]["html"]["single_page"] = True
        expected_config["author"] = "Test Author"

        directories = ['/mock/path/custom_templates',
                       '/mock/path/custom/css/path',
                       '/mock/path/custom_templates/static/images',
                       '/mock/path/templates',
                       '/mock/path/templates/static/css',
                       '/mock/path/templates/static/images']
        mock_is_dir = self.mock_directory_exists(directories)

        with patch('pathlib.Path.is_dir', new=mock_is_dir), \
                self.assertLogs(level='WARNING') as cm:
            config = self.run_with_mock_file(json.dumps(user_config))

        self.assertEqual(config, expected_config)
        self.assertIn(
            "Unknown configuration keys found and ignored", cm.output[0])
        self.assertIn("unknown_key", cm.output[0])

    def test_load_configuration_empty_file(self):
        """Test that default configuration is used when config file is empty"""
        with self.assertLogs(level='WARNING') as cm:
            config = self.run_with_mock_file('{}')
            self.assertEqual(config, self.default_config)
            self.assertIn("Configuration file is empty", cm.output[0])

    def test_load_configuration_unknown_keys(self):
        """Test that unknown keys are logged and ignored"""
        unknown_config = {
            "unknown_key": "value",
            "another_unknown": "another_value",
            "output_dir": "docs"
        }

        with self.assertLogs(level='WARNING') as cm:
            config = self.run_with_mock_file(json.dumps(unknown_config))

            # Test that unknown keys are not in the final config
            self.assertNotIn("unknown_key", config)
            self.assertNotIn("another_unknown", config)

            # Test that the warning was logged
            self.assertIn(
                "Unknown configuration keys found and ignored", cm.output[0])
            self.assertIn("unknown_key", cm.output[0])
            self.assertIn("another_unknown", cm.output[0])

            # Test that valid keys are still processed
            self.assertEqual(config["output_dir"], "/mock/path/docs")

    def test_css_configuration(self):
        """Test the validation of CSS configuration"""
        config_with_invalid_css = self.deep_copy_config()
        config_with_invalid_css['output_formats']['html']['templates']['static']['css'] = "nonexistent/path"

        def mock_is_dir(self):
            if 'nonexistent/path' in str(self):
                return False
            return True

        with patch('pathlib.Path.exists', return_value=True), \
                patch('pathlib.Path.is_dir', new=mock_is_dir):
            with self.assertRaisesRegex(ValueError, "CSS directory does not exist"):
                validate_config(config_with_invalid_css)

    def test_invalid_template_structure(self):
        """Test various invalid template structures"""
        invalid_structures = [
            ({  # Missing root_dir
                "static": {
                    "css": "static/css",
                    "images": "static/images"
                }
            }, "Missing required templates keys:.*root_dir"),
            ({  # Missing static
                "root_dir": "templates"
            }, "Missing required templates keys:.*static"),
            ({  # Missing css in static
                "root_dir": "templates",
                "static": {
                    "images": "static/images"
                }
            }, "Missing required static configuration keys:.*css")
        ]

        for structure, error_pattern in invalid_structures:
            with self.subTest(structure=structure):
                config = self.deep_copy_config()
                config['output_formats']['html']['templates'] = structure

                with self.assertRaisesRegex(ValueError, error_pattern):
                    validate_config(config)

    def test_empty_images_allowed(self):
        """Test that empty images configuration is allowed"""
        config = self.default_config.copy()
        config['output_formats']['html']['templates'] = {
            "root_dir": "templates",
            "static": {
                "css": "static/css",
                "images": None
            }
        }

        with patch('pathlib.Path.exists', return_value=True), \
                patch('os.path.isdir', return_value=True):
            try:
                validate_config(config)
            except ValueError:
                self.fail(
                    "validate_config() raised ValueError unexpectedly for empty images config")

    def test_empty_images_allowed(self):
        """Test that empty images configuration is allowed"""
        config = self.deep_copy_config()
        config['output_formats']['html']['templates']['static']['images'] = None

        with patch('pathlib.Path.exists', return_value=True), \
                patch('pathlib.Path.is_dir', return_value=True):
            try:
                validate_config(config)
            except ValueError as e:
                self.fail(
                    f"validate_config() raised ValueError unexpectedly for empty images config: {e}")

    def test_case_sensitivity(self):
        """Test case sensitivity in extensions and exclude_dirs"""
        config = self.deep_copy_config()

        # Test extensions normalization
        config['extensions'] = normalize_extensions(["F90", "f95", "F03"])
        self.assertEqual(set(config['extensions']), {"f90", "f95", "f03"})

        # Test exclude_dirs normalization
        config['exclude_dirs'] = normalize_dirs(
            ["Docs", "BUILD", "__pycache__"])
        self.assertEqual(set(config['exclude_dirs']), {
                         "docs", "build", "__pycache__"})

    def test_relative_vs_absolute_paths(self):
        """Test handling of relative vs absolute paths"""
        # Test absolute paths
        config_absolute = self.deep_copy_config()
        config_absolute['output_formats']['html']['templates'] = {
            "root_dir": "/absolute/path/templates",
            "static": {
                "css": "/absolute/path/static/css",
                "images": "/absolute/path/static/images"
            }
        }

        # Test relative paths
        config_relative = self.deep_copy_config()
        config_relative['output_formats']['html']['templates'] = {
            "root_dir": "templates",
            "static": {
                "css": "static/css",
                "images": "static/images"
            }
        }

        mock_paths = [
            '/absolute/path/templates',
            '/absolute/path/static/css',
            '/absolute/path/static/images',
            '/mock/path/templates',
            '/mock/path/static/css',
            '/mock/path/static/images'
        ]

        def mock_is_dir(path):
            return str(path) in mock_paths

        with patch('pathlib.Path.exists', return_value=True), \
                patch('pathlib.Path.is_dir', new=mock_is_dir):
            validate_config(config_absolute)
            validate_config(config_relative)
# TODO: Add tests for future configuration options
# def test_html_syntax_highlighting_config(self):
#     pass

# def test_pdf_specific_options(self):
#     pass

# def test_markdown_specific_options(self):
#     pass


if __name__ == '__main__':
    unittest.main()
