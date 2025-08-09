import unittest
from base_template_test import BaseTemplateTest
from doc4for.models.common import UseType

class TestUsesSummary(BaseTemplateTest):
    
    def test_uses_summary_basic(self):
        """Test uses summary with no selections (imports all)"""
        module_data = {
            'uses': {
                'iso_fortran_env': {
                    'module_name': 'iso_fortran_env',
                    'selections': [],
                    'renames': [],
                    'description': 'Standard Fortran environment',
                    'use_type': UseType.NONE
                }
            }
        }
        
        output = self.render_template('../core/html/sections/summary/uses_summary.html', module_data=module_data)
        
        self.assertIn('<section class="uses-summary">', output)
        self.assertIn('<h2>Uses</h2>', output)
        self.assertIn('iso_fortran_env', output)
        self.assertIn('<em>All public items</em>', output)
        self.assertIn('Standard Fortran environment', output)
    
    def test_uses_summary_with_selections_and_links(self):
        """Test uses summary with specific selections and internal module links"""
        module_data = {
            'uses': {
                'constants': {
                    'module_name': 'constants.html',
                    'selections': ['constants.html#parameter-pi', 'constants.html#parameter-e'],
                    'renames': [],
                    'description': 'Mathematical constants',
                    'use_type': UseType.NONE
                },
                'utilities': {
                    'module_name': 'utilities.html',
                    'selections': ['utilities.html#function-sqrt_approx'],
                    'renames': [],
                    'description': '',
                    'use_type': UseType.NONE
                }
            }
        }
        
        output = self.render_template('sections/summary/uses_summary.html', module_data=module_data)
        
        # Check module links
        self.assertIn('<a href="constants.html">constants</a>', output)
        self.assertIn('<a href="utilities.html">utilities</a>', output)
        
        # Check parameter links
        self.assertIn('<a href="constants.html#parameter-pi"><code>pi</code></a>', output)
        self.assertIn('<a href="constants.html#parameter-e"><code>e</code></a>', output)
        
        # Check function link
        self.assertIn('<a href="utilities.html#function-sqrt_approx"><code>sqrt_approx</code></a>', output)
    
    def test_uses_summary_with_renames(self):
        """Test uses summary with renamed imports"""
        module_data = {
            'uses': {
                'constants': {
                    'module_name': 'constants.html',
                    'selections': ['constants.html#parameter-pi', 'constants.html#parameter-e'],
                    'renames': [
                        {'local': 'my_pi', 'original': 'pi'},
                        {'local': 'euler', 'original': 'e'}
                    ],
                    'description': 'Import with renaming',
                    'use_type': UseType.NONE
                }
            }
        }
        
        output = self.render_template('sections/summary/uses_summary.html', module_data=module_data)
        
        # Check renamed items display correctly
        self.assertIn('<code class="use-rename-local">my_pi</code>', output)
        self.assertIn('<span class="use-rename-arrow">→</span>', output)
        self.assertIn('<a href="constants.html#parameter-pi"><code class="use-rename-original">pi</code></a>', output)
        
        self.assertIn('<code class="use-rename-local">euler</code>', output)
        self.assertIn('<a href="constants.html#parameter-e"><code class="use-rename-original">e</code></a>', output)
    
    def test_uses_summary_mixed_renamed_and_regular(self):
        """Test uses summary with mix of renamed and non-renamed imports"""
        module_data = {
            'uses': {
                'iso_fortran_env': {
                    'module_name': 'iso_fortran_env',
                    'selections': ['real32', 'real64', 'int32'],
                    'renames': [
                        {'local': 'sp', 'original': 'real32'},
                        {'local': 'dp', 'original': 'real64'}
                    ],
                    'description': 'Mixed imports',
                    'use_type': UseType.NONE
                }
            }
        }
        
        output = self.render_template('sections/summary/uses_summary.html', module_data=module_data)
        
        # Check renamed items
        self.assertIn('<code class="use-rename-local">sp</code>', output)
        self.assertIn('<code class="use-rename-original">real32</code>', output)
        
        self.assertIn('<code class="use-rename-local">dp</code>', output)
        self.assertIn('<code class="use-rename-original">real64</code>', output)
        
        # Check non-renamed item
        self.assertIn('<code>int32</code>', output)
        # Make sure int32 doesn't have rename styling
        self.assertNotIn('>int32</code></a>', output)  # Not a link
        self.assertNotIn('use-rename-local">int32', output)  # Not styled as rename
    
    def test_uses_summary_empty(self):
        """Test that uses summary section doesn't appear when no uses"""
        module_data = {
            'uses': {}
        }
        
        output = self.render_template('sections/summary/uses_summary.html', module_data=module_data)
        
        # Should be empty when no uses
        self.assertEqual(output.strip(), '')

if __name__ == "__main__":
    unittest.main()        