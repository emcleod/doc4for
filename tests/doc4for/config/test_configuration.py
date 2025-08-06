import unittest
import json
import copy
from unittest.mock import patch, mock_open
from doc4for.config.configuration import (
    load_configuration,
    validate_config,
    normalize_dirs,
    normalize_extensions,
    DEFAULT_CONFIG)

class TestConfiguration(unittest.TestCase):

    def setUp(self):
        """Create a deep copy of the default configuration for each test"""
        self.mock_base = "/mock/path"
        self.default_config = copy.deepcopy(DEFAULT_CONFIG)
        self.patcher = patch('doc4for.config.configuration.normalize_path',
                            side_effect=self.mock_normalize_path)
        self.mock_normalize = self.patcher.start()
        
        # Set up default mock directories that should always exist
        self.default_directories = [
            'templates',
            'templates/static/css',
            'templates/static/images'
        ]
        mock_is_dir = self.mock_directory_exists(self.default_directories)
        self.dir_patcher = patch('pathlib.Path.is_dir', new=mock_is_dir)
        self.dir_patcher.start()

    def tearDown(self):
        self.patcher.stop()
        self.dir_patcher.stop()

    def mock_normalize_path(self, path):
        """Helper method to consistently normalize paths in tests"""
        if path and path.startswith('/'):
            return path
        return f"{self.mock_base}/{path}"

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
            patch('builtins.open', mock_open(read_data=content))
        ]
        for mock in mocks:
            mock.start()
        try:
            return load_configuration()
        finally:
            for mock in mocks:
                mock.stop()

    def test_load_configuration_with_user_file(self):
        """Test that user configuration overrides defaults"""
        user_config = {
            "output_dir": "documentation",
            "output_formats": {
                "html": {
                    "templates": {
                        "root_dir": "custom_templates",
                        "static": {
                            "css": "custom/css/path",
                            "images": "static/images"
                        }
                    },
                    "single_page": True
                }
            },
            "author": "Test Author",
            "unknown_key": "should be ignored"
        }

        expected_config = self.deep_copy_config()
        root_dir = f"{self.mock_base}/custom_templates"
        expected_config["output_dir"] = f"{self.mock_base}/documentation"
        expected_config["output_formats"]["html"]["templates"]["root_dir"] = root_dir

        # CSS and images paths should be relative to root_dir
        expected_config["output_formats"]["html"]["templates"]["static"]["css"] = \
            f"{self.mock_base}/custom_templates/custom/css/path"
        expected_config["output_formats"]["html"]["templates"]["static"]["images"] = \
            f"{self.mock_base}/custom_templates/static/images"  # Changed this line

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

    def get_normalized_default_config(self):
        """Helper method to get default config with normalized paths"""
        expected_config = self.deep_copy_config()
        root_dir = f"{self.mock_base}/templates"
        expected_config["output_dir"] = f"{self.mock_base}/docs"
        expected_config["output_formats"]["html"]["templates"]["root_dir"] = root_dir
        expected_config["output_formats"]["html"]["templates"]["static"]["css"] = \
            f"{self.mock_base}/templates/static/css"
        expected_config["output_formats"]["html"]["templates"]["static"]["images"] = \
            f"{self.mock_base}/templates/static/images"
        return expected_config

    def test_load_configuration_null_json(self):
        """Test behavior with JSON containing just 'null'"""
        with self.assertLogs(level='WARNING') as cm:
            config = self.run_with_mock_file('null')
            self.assertEqual(config, self.get_normalized_default_config())
            self.assertIn("Configuration file is empty", cm.output[0])
            
    def test_load_configuration_partial_invalid_json(self):
        """Test behavior with partially valid JSON"""
        partial_json = '{"output_dir": "docs", "invalid": }'
        with self.assertLogs(level='WARNING') as cm:
            config = self.run_with_mock_file(partial_json)
            self.assertEqual(config, self.get_normalized_default_config())
            self.assertIn("Error reading configuration file", cm.output[0])

    def test_load_configuration_invalid_json(self):
        """Test behavior with invalid JSON in configuration file"""
        with self.assertLogs(level='WARNING') as cm:
            config = self.run_with_mock_file('invalid json')
            self.assertEqual(config, self.get_normalized_default_config())
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
            ("core", "Missing required templates keys"),
            (("static", "css"), "Missing required static configuration keys"),
            (("static", "images"), "Missing required static configuration keys"),
            (("core", "dir"), "Missing required core template configuration keys"),
            (("core", "inheritance_tree"), "Missing required core template configuration keys"),
            (("core", "file"), "Missing required core template configuration keys"),
            (("core", "module"), "Missing required core template configuration keys")
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
        display_names = {"pdf": "PDF", "markdown": "Markdown"}
        with patch('pathlib.Path.exists', return_value=True), \
                patch('pathlib.Path.is_dir', return_value=True):
            for format_name in ["pdf", "markdown"]:
                with self.subTest(format=format_name):
                    config = self.deep_copy_config()
                    del config["output_formats"][format_name]["enabled"]

                    with self.assertRaisesRegex(ValueError,
                                                f"{display_names[format_name]} format configuration missing 'enabled' field"):
                        validate_config(config)
        
    def test_load_configuration_empty_file(self):
        """Test that default configuration is used when config file is empty"""
        with self.assertLogs(level='WARNING') as cm:
            config = self.run_with_mock_file('{}')
            self.assertEqual(config, self.get_normalized_default_config())
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
            ({  # Missing root_dir and core
                "static": {
                    "css": "static/css",
                    "images": "static/images"
                }
            }, r"Missing required templates keys:.*(?=.*root_dir)(?=.*core).*"),
            ({  # Missing static and core
                "root_dir": "templates"
            }, r"Missing required templates keys:.*(?=.*static)(?=.*core).*"),
            ({  # Missing css in static and missing core
                "root_dir": "templates",
                "static": {
                    "images": "static/images"
                }
            }, r"Missing required templates keys:.*core.*"),
            ({  # Missing core only
                "root_dir": "templates",
                "static": {
                    "css": "static/css",
                    "images": "static/images"
                }
            }, r"Missing required templates keys:.*core.*"),
            ({  # Missing css in static (with core present)
                "root_dir": "templates",
                "core": {
                    "dir": "html",
                    "inheritance_tree": "inheritance_tree_template.html",
                    "file": "file_template.html",
                    "module": "module_template.html"
                },
                "static": {
                    "images": "static/images"
                }
            }, r"Missing required static configuration keys:.*css.*")
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
            "core": {
                "dir": "html",
                "inheritance_tree": "inheritance_tree_template.html",
                "file": "file_template.html",
                "module": "module_template.html"
            },
            "static": {
                "css": "static/css",
                "images": None
            }
        }

        with patch('pathlib.Path.exists', return_value=True), \
                patch('pathlib.Path.is_dir', return_value=True):
            try:
                validate_config(config)
            except ValueError:
                self.fail(
                    "validate_config() raised ValueError unexpectedly for empty images config")
                
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
            "core": {
                "dir": "/absolute/path/core",
                "inheritance_tree": "/absolute/path/core/inheritance_tree_template.html",
                "file": "/absolute/path/core/file_template.html",
                "module": "/absolute/path/core/module_template.html"
            },
            "static": {
                "css": "/absolute/path/static/css",
                "images": "/absolute/path/static/images"
            }
        }

        # Test relative paths
        config_relative = self.deep_copy_config()
        config_relative['output_formats']['html']['templates'] = {
            "root_dir": "templates",
            "core": {
                "dir": "core",
                "inheritance_tree": "core/inheritance_tree_template.html",
                "file": "core/file_template.html",
                "module": "core/module_template.html"
            },
            "static": {
                "css": "static/css",
                "images": "static/images"
            }
        }

        mock_paths = [
            '/absolute/path/templates',
            '/absolute/path/core',
            '/absolute/path/static/css',
            '/absolute/path/static/images',
            '/mock/path/templates',
            '/mock/path/core',
            '/mock/path/static/css',
            '/mock/path/static/images'
        ]

        def mock_is_dir(path):
            return str(path) in mock_paths

        with patch('pathlib.Path.exists', return_value=True), \
                patch('pathlib.Path.is_dir', new=mock_is_dir):
            validate_config(config_absolute)
            validate_config(config_relative)
    def test_windows_style_paths(self):
        """Test that Windows-style paths are handled correctly"""
        user_config = {
            "output_formats": {
                "html": {
                    "templates": {
                        "root_dir": "custom_templates",
                        "static": {
                            "css": "static\\css\\path",  # Windows separator
                        }
                    }
                }
            }
        }
        # TODO
        # Test should pass with normalized paths regardless of platform

    def test_symlink_paths(self):
        """Test that symlinked paths are handled correctly"""
        pass
        #TODO
        # This might need to be integration test rather than unit test
        # as it requires actual filesystem operations

    def test_directory_traversal_attempt(self):
        """Test that attempting to escape root directory is caught"""
        user_config = {
            "output_formats": {
                "html": {
                    "templates": {
                        "root_dir": "custom_templates",
                        "static": {
                            "css": "../escaped/path",
                        }
                    }
                }
            }
        }
        
        # Add additional directories to the defaults
        directories = self.default_directories + ['custom_templates']
        mock_is_dir = self.mock_directory_exists(directories)
        
        with patch('pathlib.Path.is_dir', new=mock_is_dir), \
                self.assertRaises(ValueError) as cm:
            config = self.run_with_mock_file(json.dumps(user_config))
        self.assertIn("attempts to escape root directory", str(cm.exception))

# TODO: Add tests for future configuration options
# def test_html_syntax_highlighting_config(self):
#     pass

# def test_pdf_specific_options(self):
#     pass

# def test_markdown_specific_options(self):
#     pass


if __name__ == '__main__':
    unittest.main()
