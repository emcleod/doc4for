import unittest
from pathlib import Path
from pyfakefs.fake_filesystem_unittest import TestCase
from doc4for.f90.generate_file_tree import build_directory_tree, DirectoryTree

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
        result = build_directory_tree([Path('/path/file.f90')])
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
        result = build_directory_tree([Path('/path/file1.f90'), Path('/path/file2.f90')])
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
            Path('/path/dir1/file1.f90'),
            Path('/path/dir1/subdir/file2.f90'),
            Path('/path/dir2/file3.f90')
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
        files = [Path('/path/dir1/file.f90'), Path('/path/dir2/file.f90')]
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
        files = [Path('/path/file.f90'), Path('/path/file.txt')]
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
        files = [Path('/abs/path/file1.f90'), Path('rel/path/file2.f90')]
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
        files = [Path('/path/dir1/file.f90')]
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
            Path('/path/dir1/file1.f90'),
            Path(r'\path\dir2\file2.f90')  # Using Windows-style path
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

    def test_special_characters_in_filenames(self):
        """Test with file names containing special characters excluding back and forward slashes."""
        files = [
            'file with spaces.txt',
            'file-with-hyphens.txt',
            'file_with_underscores.txt',
            'file.with.periods.txt',
            'file(with)parentheses.txt',
            'file[with]brackets.txt',
            'file{with}braces.txt',
            'file#with#hashes.txt',
            'file%with%percent.txt',
            'file&with&ampersand.txt',
            'file+with+plus.txt',
            'file=with=equal.txt',
            'file@with@at.txt',
            'file!with!exclamation.txt',
            'file*with*asterisk.txt',
            'file|with|pipe.txt',
            'file:with:colons.txt',
            'file;with;semicolons.txt',
            'file\'with\'singlequotes.txt',
            'file"with"doublequotes.txt',
            'file<with<lessthan.txt',
            'file>with>greaterthan.txt',
            'file?with?questionmark.txt',
            'file,with,commas.txt',
        ]

        for file in files:
            self.fs.create_file(file)

        result = build_directory_tree(list(map(lambda file: Path(file), files)))

        expected = DirectoryTree('')
        for file in files:
            path = Path(file)
            current = expected
            for part in path.parts[:-1]:
                child = next((c for c in current.children if isinstance(c, DirectoryTree) and c.name == part), None)
                if child is None:
                    child = DirectoryTree(part, current)
                    current.children.append(child)
                current = child
            current.children.append(str(path))

        self.assertEqual(list(result.walk()), list(expected.walk()))

    def test_whitespace_in_filenames(self):
        """Test with file names containing whitespace."""
        files = [
            'file with spaces.txt',
            'file\twith\ttabs.txt',
            'file\nwith\nnewlines.txt',
            'file\rwith\rcarriagereturns.txt',
        ]

        for file in files:
            self.fs.create_file(file)

        result = build_directory_tree(list(map(lambda file: Path(file), files)))

        expected = DirectoryTree('')
        for file in files:
            path = Path(file)
            current = expected
            for part in path.parts[:-1]:
                child = next((c for c in current.children if isinstance(c, DirectoryTree) and c.name == part), None)
                if child is None:
                    child = DirectoryTree(part, current)
                    current.children.append(child)
                current = child
            current.children.append(str(path))

        self.assertEqual(list(result.walk()), list(expected.walk()))

    def test_mixed_cases(self):
        """Test with file names having mixed cases."""
        files = [
            'FileWithMixedCases.txt',
            'file/with/MixedCases/file.txt',
            'MixedCases/file.txt',
        ]

        for file in files:
            self.fs.create_file(file)

        result = build_directory_tree(list(map(lambda file: Path(file), files)))

        expected = DirectoryTree('')
        for file in files:
            path = Path(file)
            current = expected
            for part in path.parts[:-1]:
                child = next((c for c in current.children if isinstance(c, DirectoryTree) and c.name == part), None)
                if child is None:
                    child = DirectoryTree(part, current)
                    current.children.append(child)
                current = child
            current.children.append(str(path))

        self.assertEqual(list(result.walk()), list(expected.walk()))

if __name__ == '__main__':
    unittest.main()