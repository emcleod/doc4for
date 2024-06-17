import unittest
from pathlib import Path
from pyfakefs.fake_filesystem_unittest import TestCase
from f90.generate_file_tree import build_directory_tree, DirectoryTree

class TestBuildDirectoryTree(TestCase):
    def setUp(self):
        self.setUpPyfakefs()

    def test_empty_list(self):
        """Test with an empty list of files."""
        result = build_directory_tree([])
        expected = DirectoryTree('')
        
        self.assertEqual(list(result.walk()), list(expected.walk()))

    def test_single_file(self):
        """Test with a single file in the path directory."""
        self.fs.create_file('/path/file.f90')
        result = build_directory_tree(['/path/file.f90'])
        expected = DirectoryTree('')
        root = DirectoryTree('/', expected)
        expected.children.append(root)
        path_dir = DirectoryTree('path', root)
        root.children.append(path_dir)
        path_dir.children.append('/path/file.f90')
        actual_paths = list(result.walk())
        expected_paths = list(expected.walk())

        self.assertEqual(actual_paths, expected_paths)

    def test_multiple_files_same_directory(self):
        """Test with multiple files in the same directory."""
        self.fs.create_file('/path/file1.f90')
        self.fs.create_file('/path/file2.f90')
        result = build_directory_tree(['/path/file1.f90', '/path/file2.f90'])
        expected = DirectoryTree('')
        root = DirectoryTree('/', expected)
        expected.children.append(root)
        path_dir = DirectoryTree('path', root)
        root.children.append(path_dir)
        path_dir.children.append('/path/file1.f90')
        path_dir.children.append('/path/file2.f90')

        actual_paths = list(result.walk())
        expected_paths = list(expected.walk())

        self.assertEqual(actual_paths, expected_paths)

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

        expected = DirectoryTree('')
        root = DirectoryTree('/', expected)
        expected.children.append(root)
        
        path = DirectoryTree('path', root)
        root.children.append(path)
        
        dir1 = DirectoryTree('dir1', path)
        path.children.append(dir1)
        dir1.children.append('/path/dir1/file1.f90')
        
        subdir = DirectoryTree('subdir', dir1)
        dir1.children.append(subdir)
        subdir.children.append('/path/dir1/subdir/file2.f90')
        
        dir2 = DirectoryTree('dir2', path)
        path.children.append(dir2)
        dir2.children.append('/path/dir2/file3.f90')

        actual_paths = list(result.walk())
        expected_paths = list(expected.walk())

        self.assertEqual(actual_paths, expected_paths)

    def test_files_with_same_name(self):
        """Test with files having the same name in different directories."""
        self.fs.create_file('/path/dir1/file.f90')
        self.fs.create_file('/path/dir2/file.f90')
        files = ['/path/dir1/file.f90', '/path/dir2/file.f90']
        result = build_directory_tree(files)

        expected = DirectoryTree('')
        root = DirectoryTree('/', expected)
        expected.children.append(root)
        path = DirectoryTree('path', root)
        root.children.append(path)
        dir1 = DirectoryTree('dir1', path)
        dir2 = DirectoryTree('dir2', path)
        path.children.extend([dir1, dir2])
        dir1.children.append('/path/dir1/file.f90')
        dir2.children.append('/path/dir2/file.f90')

        self.assertEqual(list(result.walk()), list(expected.walk()))

    def test_non_f90_files(self):
        """Test that non-f90 files are handled correctly."""
        self.fs.create_file('/path/file.f90')
        self.fs.create_file('/path/file.txt')
        files = ['/path/file.f90', '/path/file.txt']
        result = build_directory_tree(files)

        expected = DirectoryTree('')
        root = DirectoryTree('/', expected)
        expected.children.append(root)
        path = DirectoryTree('path', root)
        root.children.append(path)
        path.children.extend(['/path/file.f90', '/path/file.txt'])

        self.assertEqual(list(result.walk()), list(expected.walk()))

    def test_absolute_and_relative_paths(self):
        """Test with a mix of absolute and relative paths."""
        self.fs.create_file('/abs/path/file1.f90')
        self.fs.create_file('rel/path/file2.f90')
        files = ['/abs/path/file1.f90', 'rel/path/file2.f90']
        result = build_directory_tree(files)

        expected = DirectoryTree('')
        root = DirectoryTree('/', expected)
        expected.children.append(root)
        abs_dir = DirectoryTree('abs', root)
        root.children.append(abs_dir)
        abs_path = DirectoryTree('path', abs_dir)
        abs_dir.children.append(abs_path)
        abs_path.children.append('/abs/path/file1.f90')

        rel = DirectoryTree('rel', expected)
        expected.children.append(rel)
        rel_path = DirectoryTree('path', rel)
        rel.children.append(rel_path)
        rel_path.children.append('rel/path/file2.f90')

        self.assertEqual(list(result.walk()), list(expected.walk()))

    def test_empty_directories(self):
        """Test that empty directories are not included."""
        self.fs.create_file('/path/dir1/file.f90')
        self.fs.create_dir('/path/empty_dir')
        files = ['/path/dir1/file.f90']
        result = build_directory_tree(files)

        expected = DirectoryTree('')
        root = DirectoryTree('/', expected)
        expected.children.append(root)
        path = DirectoryTree('path', root)
        root.children.append(path)
        dir1 = DirectoryTree('dir1', path)
        path.children.append(dir1)
        dir1.children.append('/path/dir1/file.f90')

        self.assertEqual(list(result.walk()), list(expected.walk()))

    def test_mixed_separators(self):
        """Test with file paths using different separators."""
        self.fs.create_file('/path/dir1/file1.f90')
        self.fs.create_file('/path/dir2/file2.f90')
        files = [
            '/path/dir1/file1.f90',
            r'\path\dir2\file2.f90'  # Using Windows-style path
        ]
        result = build_directory_tree(files)

        expected = DirectoryTree('')
        root = DirectoryTree('/', expected)
        expected.children.append(root)
        path = DirectoryTree('path', root)
        root.children.append(path)
        dir1 = DirectoryTree('dir1', path)
        dir2 = DirectoryTree('dir2', path)
        path.children.extend([dir1, dir2])
        dir1.children.append('/path/dir1/file1.f90')
        dir2.children.append('/path/dir2/file2.f90')

        self.assertEqual(list(result.walk()), list(expected.walk()))

if __name__ == '__main__':
    unittest.main()