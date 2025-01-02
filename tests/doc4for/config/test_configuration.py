import unittest
import json
from pathlib import Path
from unittest.mock import patch, mock_open
from doc4for.config.configuration import load_configuration, validate_config

class TestConfiguration(unittest.TestCase):

    def setUp(self):
        # Example default configuration
        self.default_config = {
            "output_dir": "docs",
            "extensions": ["f90", "f95", "f03", "f08"],
            "output_formats": {
                "html": {
                    "enabled": True,
                    "use_default_css": True,
                    "custom_css": None,
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
            "version": None
        }

    def test_load_configuration_defaults(self):
        """Test that default configuration is used when no file exists"""
        with patch('pathlib.Path.exists', return_value=False):
            config = load_configuration()
            self.assertEqual(config, self.default_config)

    def test_load_configuration_with_user_file(self):
        """Test that user configuration overrides defaults"""
        user_config = {
            "output_dir": "documentation",
            "output_formats": {
                "html": {
                    "custom_css": "my_styles.css"
                }
            },
            "author": "Test Author"
        }
       
        expected_config = self.default_config.copy()
        expected_config["output_dir"] = "documentation"
        expected_config["output_formats"]["html"]["custom_css"] = "my_styles.css"
        expected_config["author"] = "Test Author"

        mock_file_data = json.dumps(user_config)
       
        with patch('pathlib.Path.exists', return_value=True), \
            patch('builtins.open', mock_open(read_data=mock_file_data)):
            config = load_configuration()
            self.assertEqual(config, expected_config)

    def test_load_configuration_invalid_json(self):
        """Test behavior with invalid JSON in configuration file"""
        with patch('pathlib.Path.exists', return_value=True), \
            patch('builtins.open', mock_open(read_data='invalid json')), \
            patch('logging.Logger.warning') as mock_warning:
            config = load_configuration()
            self.assertEqual(config, self.default_config)
            mock_warning.assert_called_once_with(
                "Error reading configuration file: Expecting value: line 1 column 1 (char 0). Using defaults."
            )

    def test_required_fields(self):
        """Test that configuration fails if required fields are missing"""
        required_fields = [
            {"output_dir": "docs"},  # Missing extensions
            {"extensions": ["f90"]},  # Missing output_dir
            {  # Missing output_formats
                "output_dir": "docs", 
                "extensions": ["f90"],
                "exclude_dirs": []
            },
            {  # Missing exclude_dirs
                "output_dir": "docs", 
                "extensions": ["f90"],
                "output_formats": self.default_config["output_formats"]
            }
        ]

        for incomplete_config in required_fields:
            with self.subTest(config=incomplete_config):
                mock_file_data = json.dumps(incomplete_config)
                with patch('pathlib.Path.exists', return_value=True), \
                    patch('builtins.open', mock_open(read_data=mock_file_data)):
                    # We're testing the entire config after merging with defaults
                    config = load_configuration()
                    # Now remove the key we're testing from the merged config
                    for key in self.default_config:
                        if key not in incomplete_config:
                            del config[key]
                    with self.assertRaises(ValueError):
                        validate_config(config)

    def test_output_formats_structure(self):
        """Test that all output formats have the correct structure"""
        incorrect_formats = [
            {  # Missing html format
                "output_formats": {
                    "pdf": {"enabled": False},
                    "markdown": {"enabled": False}
                }
            },
            {  # Missing enabled field in html
                "output_formats": {
                    "html": {"use_default_css": True},
                    "pdf": {"enabled": False},
                    "markdown": {"enabled": False}
                }
            },
            {  # Missing use_default_css field in html
                "output_formats": {
                    "html": {"enabled": True},
                    "pdf": {"enabled": False},
                    "markdown": {"enabled": False}
                }
            }
        ]

        for incorrect_config in incorrect_formats:
            # Create a full config and then override with incorrect structure
            full_config = self.default_config.copy()
            full_config.update(incorrect_config)
            
            with self.subTest(config=incorrect_config):
                with self.assertRaises(ValueError):
                    validate_config(full_config)

    def test_load_configuration_empty_file(self):
        """Test that default configuration is used when config file is empty"""
        with patch('pathlib.Path.exists', return_value=True), \
            patch('builtins.open', mock_open(read_data='{}')), \
            patch('logging.Logger.warning') as mock_warning:
            config = load_configuration()
            self.assertEqual(config, self.default_config)
            mock_warning.assert_called_once_with("Configuration file is empty. Using defaults.")

    def test_load_configuration_unknown_keys(self):
        """Test that unknown keys are logged and ignored"""
        unknown_config = {
            "unknown_key": "value",
            "output_dir": "docs"
        }
        expected_warning = "Unknown configuration keys found and ignored: {'unknown_key'}"
        
        with patch('pathlib.Path.exists', return_value=True), \
            patch('builtins.open', mock_open(read_data=json.dumps(unknown_config))), \
            patch('logging.Logger.warning') as mock_warning:
            config = load_configuration()
            self.assertNotIn("unknown_key", config)
            mock_warning.assert_called_once_with(expected_warning)
#TODO 
# test case sensitivity in extensions and exclude_dirs
# relative vs absolute paths

   # TODO: Add tests for future configuration options
   # def test_html_syntax_highlighting_config(self):
   #     pass
   
   # def test_pdf_specific_options(self):
   #     pass
   
   # def test_markdown_specific_options(self):
   #     pass

if __name__ == '__main__':
   unittest.main()

