import unittest
from pathlib import Path
from pyfakefs.fake_filesystem_unittest import TestCase
from doc4for.utils.file_utils import find_files_by_extensions

class TestFindF90Files(TestCase):

    def setUp(self):
        self.setUpPyfakefs()

    def test_find_f90_files(self):        
        # Simulate the directory structure:
        # .
        # ├── dir1
        # │   ├── file1.f90
        # │   └── subdir
        # │       └── file2.f90
        # ├── dir2
        # │   ├── file3.py
        # │   └── file4.txt
        # └── file5.f90
        self.fs.create_dir('dir1')
        self.fs.create_file('dir1/file1.f90')
        self.fs.create_dir('dir1/subdir')
        self.fs.create_file('dir1/subdir/file2.f90')
        self.fs.create_dir('dir2')
        self.fs.create_file('dir2/file3.py')
        self.fs.create_file('dir2/file4.txt')
        self.fs.create_file('file5.f90')
        expected_files = [
            'file5.f90',
            'dir1/file1.f90',
            'dir1/subdir/file2.f90'
        ]
        current_dir = '.'
        result = find_files_by_extensions(current_dir)
        self.assertCountEqual(result, [Path(file) for file in expected_files])

    def test_find_f90_files_no_f90(self):
        # Simulate a directory structure with no .f90 files:
        # .
        # ├── dir1
        # │   ├── file1.py
        # │   └── file2.txt
        self.fs.create_dir('dir1')
        self.fs.create_file('dir1/file1.py')
        self.fs.create_dir('dir1/file2.txt')
        expected_files = []
        current_dir = '.'
        result = find_files_by_extensions(current_dir)
        self.assertCountEqual(result, expected_files)

    def test_find_f90_files_empty_dir(self):
        # Simulate an empty directory:
        # .
        # ├── dir1
        self.fs.create_dir('dir1')
        expected_files = []
        current_dir = '.'
        result = find_files_by_extensions(current_dir)
        self.assertCountEqual(result, expected_files)

#TODO test other extensions
if __name__ == '__main__':
    unittest.main()