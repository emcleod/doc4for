import unittest
import os
import tempfile
import shutil
from pathlib import Path
from pyfakefs.fake_filesystem_unittest import TestCase
from doc4for.f90.generate_module_tree import extract_module_data
from doc4for.f90.generate_module_diagram import generate_module_diagram, generate_module_usage_diagram

class TestModuleDiagram(TestCase):
    def setUp(self):
        self.setUpPyfakefs()
        self.temp_dir = tempfile.mkdtemp()
        
    def tearDown(self):
        shutil.rmtree(self.temp_dir, ignore_errors=True)
        
    def test_generate_module_diagram(self):
        # Create test Fortran files
        self.fs.create_file(
            '/fake/path/constants.f90',
            contents='''\
    module constants
    use iso_fortran_env

    real(real64), parameter :: PI = 3.14159
    real(real64), parameter :: E = 2.71828
    real(real64), parameter :: AVOGADROS_NUMBER = 6.022e23
    end module constants
    ''')
        self.fs.create_file(
            '/fake/path/maths.f90',
            contents='''\
    module maths
        use iso_fortran_env
        use constants
    end module maths
    ''')
        self.fs.create_file(
            '/fake/path/physics.f90',
            contents='''\
    module physics
        use constants, only: PI
        use maths
    end module physics
    ''')
            
        # Extract module data
        fortran_files = [
            Path('/fake/path/constants.f90'),
            Path('/fake/path/maths.f90'),
            Path('/fake/path/physics.f90')
        ]
        modules = extract_module_data(fortran_files)
        
        # Generate the dependency diagram
        output_dir = '/fake/output'
        self.fs.create_dir(output_dir)
        
        diagram_path = generate_module_diagram(modules, output_dir)
        
        # Check that the HTML file was created
        self.assertTrue(os.path.exists(diagram_path))
        self.assertTrue(diagram_path.endswith('.html'))
        
        # Check the content of the HTML file
        with open(diagram_path, 'r') as f:
            content = f.read()
            self.assertIn('Module Dependencies', content)
            self.assertIn('<table>', content)
            self.assertIn('constants', content)
            self.assertIn('maths', content)
            self.assertIn('physics', content)
        
    def test_generate_module_usage_diagram(self):
        # Create test Fortran files
        self.fs.create_file(
            '/fake/path/constants.f90',
            contents='''\
    module constants
    use iso_fortran_env

    real(real64), parameter :: PI = 3.14159
    real(real64), parameter :: E = 2.71828
    real(real64), parameter :: AVOGADROS_NUMBER = 6.022e23
    end module constants
    ''')
        self.fs.create_file(
            '/fake/path/maths.f90',
            contents='''\
    module maths
        use iso_fortran_env
        use constants
    end module maths
    ''')
        self.fs.create_file(
            '/fake/path/physics.f90',
            contents='''\
    module physics
        use constants, only: PI
        use maths
    end module physics
    ''')
            
        # Extract module data
        fortran_files = [
            Path('/fake/path/constants.f90'),
            Path('/fake/path/maths.f90'),
            Path('/fake/path/physics.f90')
        ]
        modules = extract_module_data(fortran_files)
        
        # Generate the usage diagram
        output_dir = '/fake/output'
        self.fs.create_dir(output_dir)
        
        # Test overall usage diagram
        usage_diagram_path = generate_module_usage_diagram(modules, output_dir)
        self.assertTrue(os.path.exists(usage_diagram_path))
        
        # Check the content of the HTML file
        with open(usage_diagram_path, 'r') as f:
            content = f.read()
            self.assertIn('Module Usage', content)
            self.assertIn('<table>', content)
            self.assertIn('Used By', content)
            self.assertIn('Items Used', content)
        
        # Test specific module usage diagram
        module_usage_path = generate_module_usage_diagram(
            modules, 
            output_dir,
            module_name='constants'
        )
        self.assertTrue(os.path.exists(module_usage_path))
        self.assertTrue('constants' in module_usage_path)
        
        # Check the content of the specific module HTML file
        with open(module_usage_path, 'r') as f:
            content = f.read()
            self.assertIn('Usage of constants', content)
            self.assertIn('<table>', content)
            self.assertIn('maths', content)
            self.assertIn('physics', content)

if __name__ == '__main__':
    unittest.main() 