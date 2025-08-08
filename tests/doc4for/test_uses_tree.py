import unittest
from pathlib import Path
from pyfakefs.fake_filesystem_unittest import TestCase
from doc4for.process.generate_uses_tree import generate_imports_tree, ALL
from doc4for.f90.generate_module_tree import extract_module_data


class TestUsesTreePopulation(TestCase):
    def setUp(self):
        self.setUpPyfakefs()

    def test_populate_used_by_tree_import_all(self):
        self.fs.create_file(
            '/fake/path/module_a.f90',
            contents='''\
module a 
  use iso_fortran_env
  use b
  use c
end module a
''')
        self.fs.create_file(
            '/fake/path/module_bc.f90',
            contents='''\
module b
  real, parameter :: PI = 3.14159
end module b

module c
  use iso_fortran_env
  use b
  use d
end module c
''')
        self.fs.create_file(
            '/fake/path/module_def.f90',
            contents='''\
module d
    use iso_fortran_env
end module d

module e
    use iso_fortran_env
    use a
    use b
    use d
    use x
    use y
    use z
end module e

module f
    use iso_fortran_env
    use a
    use b
    use c
    use d
    use e
    use x
    use y
    use z
end module f
''')

        descriptions = extract_module_data([
            Path('/fake/path/module_a.f90'),
            Path('/fake/path/module_bc.f90'),
            Path('/fake/path/module_def.f90'),
            ])
        use_tree = generate_imports_tree(descriptions)

        self.assertIn('a', use_tree)
        self.assertIn('b', use_tree)
        self.assertIn('c', use_tree)
        self.assertIn('d', use_tree)
        self.assertIn('e', use_tree)
        self.assertIn('f', use_tree)
        self.assertIn('x', use_tree)
        self.assertIn('y', use_tree)
        self.assertIn('z', use_tree)

        self.assertEqual(use_tree['a'][ALL], ['e', 'f'])
        self.assertEqual(use_tree['b'][ALL], ['a', 'c', 'e', 'f'])
        self.assertEqual(use_tree['c'][ALL], ['a', 'f'])
        self.assertEqual(use_tree['d'][ALL], ['c', 'e', 'f'])
        self.assertEqual(use_tree['e'][ALL], ['f'])
        self.assertEqual(use_tree['f'], {})
        self.assertEqual(use_tree['x'][ALL], ['e', 'f'])
        self.assertEqual(use_tree['x'][ALL], ['e', 'f'])
        self.assertEqual(use_tree['x'][ALL], ['e', 'f'])

            
    def test_populate_used_by_tree_import_only(self):
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
        use iso_fortran_env
        use constants, only: PI, E
    end module physics
    ''')
        self.fs.create_file(
            '/fake/path/chemistry.f90',
            contents='''\
    module chemistry
        use iso_fortran_env
        use constants, only: AVOGADROS_NUMBER
    end module chemistry
    ''')

        descriptions = extract_module_data([
            Path('/fake/path/constants.f90'),
            Path('/fake/path/maths.f90'),
            Path('/fake/path/physics.f90'),
            Path('/fake/path/chemistry.f90'),
            ])
        use_tree = generate_imports_tree(descriptions)
        
        self.assertEqual(use_tree['constants'][ALL], ['maths'])        
        self.assertEqual(use_tree['constants']['pi'], ['physics']) #fparser converts names to lower case
        self.assertEqual(use_tree['constants']['e'], ['physics'])
        self.assertEqual(use_tree['constants']['avogadros_number'], ['chemistry'])
        self.assertEqual(use_tree['maths'], {})
        self.assertEqual(use_tree['physics'], {})
        self.assertEqual(use_tree['chemistry'], {})
        self.assertEqual(use_tree['iso_fortran_env'][ALL], ['constants', 'maths', 'physics', 'chemistry'])

        
if __name__ == '__main__':
    unittest.main()