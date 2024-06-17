import unittest
from pathlib import Path
from pyfakefs.fake_filesystem_unittest import TestCase
from f90.generate_file_tree import build_directory_tree  # Import your function

class TestBuildDirectoryTree(TestCase):
    def setUp(self):
        self.setUpPyfakefs()

    def test_empty_list(self):
        """Test with an empty list of files."""
        result = build_directory_tree([])
        self.assertEqual(result, {})

    def test_single_file(self):
        """Test with a single file in the path directory."""
        self.fs.create_file('/path/file.f90')
        result = build_directory_tree(['/path/file.f90'])
        expected = {'/': {
                'path': {
                    'file.f90': '/path/file.f90'
                }
            }
        }
        self.assertEqual(result, expected)

    def test_multiple_files_same_directory(self):
        """Test with multiple files in the same directory."""
        self.fs.create_file('/path/file1.f90')
        self.fs.create_file('/path/file2.f90')
        result = build_directory_tree(['/path/file1.f90', '/path/file2.f90'])
        expected = {'/': {
                'path': {
                    'file1.f90': '/path/file1.f90',
                    'file2.f90': '/path/file2.f90'
                }
            }
        }
        self.assertEqual(result, expected)

    def test_nested_directories(self):
        """Test with files in nested directories."""
        self.fs.create_file('/path/dir1/file1.f90')
        self.fs.create_file('/path/dir1/subdir/file2.f90')
        self.fs.create_file('/path/dir2/file3.f90')
        files = [
            '/path/dir1/file1.f90',
            '/path/dir1/subdir/file2.f90',
            '/path/dir2/file3.f90'
        ]
        result = build_directory_tree(files)
        expected = {'/': {
                'path': {
                    'dir1': {
                        'file1.f90': '/path/dir1/file1.f90',
                        'subdir': {
                            'file2.f90': '/path/dir1/subdir/file2.f90'
                        }
                    },
                    'dir2': {
                        'file3.f90': '/path/dir2/file3.f90'
                    }
                }
            }
        }
        self.assertEqual(result, expected)

    def test_files_with_same_name(self):
        """Test with files having the same name in different directories."""
        self.fs.create_file('/path/dir1/file.f90')
        self.fs.create_file('/path/dir2/file.f90')
        files = ['/path/dir1/file.f90', '/path/dir2/file.f90']
        result = build_directory_tree(files)
        expected = {'/': {
                'path': {
                    'dir1': {'file.f90': '/path/dir1/file.f90'},
                    'dir2': {'file.f90': '/path/dir2/file.f90'}
                }
            }
        }
        self.assertEqual(result, expected)

    def test_non_f90_files(self):
        """Test that non-f90 files are handled correctly."""
        self.fs.create_file('/path/file.f90')
        self.fs.create_file('/path/file.txt')
        files = ['/path/file.f90', '/path/file.txt']
        result = build_directory_tree(files)
        expected = {'/': {
                'path': {
                    'file.f90': '/path/file.f90',
                    'file.txt': '/path/file.txt'
                }
            }
        }
        self.assertEqual(result, expected)

    def test_absolute_and_relative_paths(self):
        """Test with a mix of absolute and relative paths."""
        self.fs.create_file('/abs/path/file1.f90')
        self.fs.create_file('rel/path/file2.f90')
        files = ['/abs/path/file1.f90', 'rel/path/file2.f90']
        result = build_directory_tree(files)
        expected = {'/': {
                'abs': {
                    'path': {'file1.f90': '/abs/path/file1.f90'}
                }
            },
            'rel': {
                'path': {'file2.f90': 'rel/path/file2.f90'}
            }
        }
        self.assertEqual(result, expected)

    def test_empty_directories(self):
        """Test that empty directories are not included."""
        self.fs.create_file('/path/dir1/file.f90')
        self.fs.create_dir('/path/empty_dir')
        files = ['/path/dir1/file.f90']
        result = build_directory_tree(files)
        expected = {'/': {
                'path': {
                    'dir1': {'file.f90': '/path/dir1/file.f90'}
                }
            }
        }
        self.assertEqual(result, expected)

    def test_mixed_separators(self):
        """Test with file paths using different separators."""
        self.fs.create_file('/path/dir1/file1.f90')
        self.fs.create_file('/path/dir2/file2.f90')
        files = [
            '/path/dir1/file1.f90',
            r'\path\dir2\file2.f90'  # Using Windows-style path
        ]
        result = build_directory_tree(files)
        expected = {
            '/': {
                'path': {
                    'dir1': {'file1.f90': str(Path('/path/dir1/file1.f90'))},
                    'dir2': {'file2.f90': str(Path('/path/dir2/file2.f90'))}  # Updated expected path
                }
            }
        }
        self.assertEqual(result, expected)
    # def test_mixed_separators(self):
    #     """Test with file paths using different separators."""
    #     self.fs.create_file('/path/dir1/file1.f90')
    #     self.fs.create_file('/path/dir2/file2.f90')
    #     files = [
    #         '/path/dir1/file1.f90',
    #         r'\path\dir2\file2.f90'  # Using Windows-style path
    #     ]
    #     result = build_directory_tree(files)
    #     expected = {'/': {
    #             'path': {
    #                 'dir1': {'file1.f90': str(Path('/path/dir1/file1.f90'))},
    #                 'dir2': {'file2.f90': str(Path(r'\path\dir2\file2.f90'))}
    #             }
    #         }
    #     }
    #     self.assertEqual(result, expected)
    # def test_mixed_separators(self):
    #     """Test with file paths using different separators."""
    #     self.fs.create_file('/path/dir1/file1.f90')
    #     self.fs.create_file('/path/dir2/file2.f90')
    #     files = [
    #         '/path/dir1/file1.f90',
    #         r'\path\dir2\file2.f90'  # Using Windows-style path
    #     ]
    #     result = build_directory_tree(files)
    #     expected = {
    #         'path': {
    #             'dir1': {'file1.f90': os.path.normpath('/path/dir1/file1.f90')},
    #             'dir2': {'file2.f90': os.path.normpath(r'\path\dir2\file2.f90')}
    #         }
    #     }
    #     self.assertEqual(result, expected)

if __name__ == '__main__':
    unittest.main()