import unittest
import os
from pathlib import Path
from jinja2 import Environment, FileSystemLoader

class BaseTemplateTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        # Get the project root directory
        project_root = Path(__file__).parent.parent.parent
        template_dir = project_root / 'templates'
        
        # Set up Jinja2 environment with whitespace control
        cls.env = Environment(
            loader=FileSystemLoader(str(template_dir)),
            trim_blocks=True,       # Remove first newline after blocks
            lstrip_blocks=True,     # Strip leading spaces/tabs from lines
            keep_trailing_newline=True  # Preserve final newline
        )
    
    def render_template(self, template_path, **context):
        """Helper method to render a template with context"""
        template = self.env.get_template(template_path)
        return template.render(**context)
    
    def assertHTMLEqual(self, actual, expected):
        """Compare HTML strings with normalized whitespace"""
        # Normalize line endings and strip leading/trailing whitespace
        actual_normalized = '\n'.join(line.strip() for line in actual.strip().splitlines() if line.strip())
        expected_normalized = '\n'.join(line.strip() for line in expected.strip().splitlines() if line.strip())
        
        self.assertEqual(actual_normalized, expected_normalized)