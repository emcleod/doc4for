import unittest
from pathlib import Path
from pyfakefs.fake_filesystem_unittest import TestCase
from doc4for.f90.generate_file_tree import extract_file_data

class TestModuleExtraction(TestCase):
    def setUp(self):
        self.setUpPyfakefs()

    def test_module_names_extracted(self):
        self.fs.create_file(
            '/fake/path/modules.f90',
            contents='''\
module module1
    implicit none
    ! Module 1 code
end module module1

module module2
    implicit none
    ! Module 2 code
end module module2
''')
        result = extract_file_data([Path('/fake/path/modules.f90')])
        self.assertEqual(len(result), 1)
        file_data = result[0]
        self.assertEqual(file_data['file_name'], '/fake/path/modules.f90')
        self.assertEqual(len(file_data['modules']), 2)
        self.assertIn('module1', file_data['modules'])
        self.assertIn('module2', file_data['modules'])

if __name__ == '__main__':
    unittest.main()