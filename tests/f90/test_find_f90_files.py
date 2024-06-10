import unittest
from unittest.mock import patch
import os
import sys

# Add the project root to the Python path
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), '..', '..')))
from generate_module_tree import find_f90_files

class TestFindF90Files(unittest.TestCase):

    @patch('os.walk')
    def test_find_f90_files(self, mock_walk):
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

        mock_walk.return_value = [
            ('.', ['dir1', 'dir2'], ['file5.f90']),
            ('./dir1', ['subdir'], ['file1.f90']),
            ('./dir1/subdir', [], ['file2.f90']),
            ('./dir2', [], ['file3.py', 'file4.txt']),
        ]

        expected_files = ['file5.f90', 'dir1/file1.f90', 'dir1/subdir/file2.f90']
        current_dir = '.'

        result = find_f90_files(current_dir)

        self.assertListEqual(sorted(result), sorted(expected_files))
        mock_walk.assert_called_once_with(current_dir)

    @patch('os.walk')
    def test_find_f90_files_no_f90(self, mock_walk):
        # Simulate a directory structure with no .f90 files:
        # .
        # ├── dir1
        # │   ├── file1.py
        # │   └── file2.txt

        mock_walk.return_value = [
            ('.', ['dir1'], []),
            ('./dir1', [], ['file1.py', 'file2.txt']),
        ]

        current_dir = '.'

        result = find_f90_files(current_dir)

        self.assertListEqual(result, [])
        mock_walk.assert_called_once_with(current_dir)

    @patch('os.walk')
    def test_find_f90_files_empty_dir(self, mock_walk):
        # Simulate an empty directory

        mock_walk.return_value = [('.', [], [])]

        current_dir = '.'

        result = find_f90_files(current_dir)

        self.assertListEqual(result, [])
        mock_walk.assert_called_once_with(current_dir)

if __name__ == '__main__':
    unittest.main()