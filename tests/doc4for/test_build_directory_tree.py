import unittest
from unittest.mock import patch, call
import os
from pathlib import Path
import shutil
import tempfile
from doc4for.utils.file_utils import find_files_by_extensions, check_write_permissions, create_docs_directory, PROJECT_ROOT

class TestBuildDirectory(unittest.TestCase):

    def setUp(self):
        # Create a temporary directory outside the project root
        self.test_dir = tempfile.mkdtemp()
        self.temp_dir = os.path.join(self.test_dir, 'project_root')
        os.makedirs(self.temp_dir)

        self.original_project_root = PROJECT_ROOT
        # Patch PROJECT_ROOT to use the temporary directory
        patcher = patch('doc4for.utils.file_utils.PROJECT_ROOT', self.temp_dir)
        patcher.start()
        self.addCleanup(patcher.stop)

    def tearDown(self):
        # Remove the temporary directory and its parent directory
        shutil.rmtree(self.test_dir)
        # Restore the original PROJECT_ROOT
        globals()['PROJECT_ROOT'] = self.original_project_root

    def test_find_files_by_extensions(self):
        # Create test files
        Path(self.temp_dir, 'file1.f90').touch()
        Path(self.temp_dir, 'file2.f95').touch()
        Path(self.temp_dir, 'subdir').mkdir()
        Path(self.temp_dir, 'subdir', 'file3.f90').touch()
        Path(self.temp_dir, 'file4.txt').touch()

        # Test with default extension
        files = find_files_by_extensions(self.temp_dir)
        self.assertEqual(len(files), 2)
        self.assertIn(Path('file1.f90'), files)
        self.assertIn(Path('subdir/file3.f90'), files)

        # Test with multiple extensions
        files = find_files_by_extensions(self.temp_dir, extensions={'f90', 'f95'})
        self.assertEqual(len(files), 3)
        self.assertIn(Path('file1.f90'), files)
        self.assertIn(Path('file2.f95'), files)
        self.assertIn(Path('subdir/file3.f90'), files)

    def test_check_write_permissions(self):
        # Test with writable directory
        self.assertTrue(check_write_permissions(self.temp_dir))

        # Test with non-writable directory
        non_writable_dir = Path(self.temp_dir, 'non_writable')
        non_writable_dir.mkdir()
        os.chmod(non_writable_dir, 0o555)  # Read and execute permissions only
        self.assertFalse(check_write_permissions(non_writable_dir))

    @patch('builtins.input', return_value='y')
    @patch('os.makedirs')
    def test_create_docs_directory_new(self, mock_makedirs, mock_input):
        with patch('os.path.exists', return_value=False):
            result = create_docs_directory()
            self.assertTrue(result)
            mock_makedirs.assert_has_calls([
                call(os.path.join(self.temp_dir, 'docs')),
                call(os.path.join(self.temp_dir, 'docs', 'static'))
            ], any_order=True)

    @patch('builtins.input', return_value='y')
    @patch('os.path.islink')
    @patch('os.path.isfile')
    @patch('os.path.isdir')
    @patch('os.unlink')
    @patch('shutil.rmtree')
    @patch('os.walk')
    @patch('os.path.exists')
    @patch('os.path.join', side_effect=os.path.join)
    def test_create_docs_directory_existing(self, mock_join, mock_exists, mock_walk, mock_rmtree, mock_unlink, 
                                            mock_isdir, mock_isfile, mock_islink, mock_input):
        mock_exists.return_value = True
        mock_walk.return_value = [
            ('docs', ['subdir'], ['file1.txt']),
            ('docs/subdir', [], ['file2.txt'])
        ]
        mock_islink.return_value = False
        mock_isfile.side_effect = lambda x: not x.endswith('subdir')
        mock_isdir.side_effect = lambda x: x.endswith('subdir')

        result = create_docs_directory()

        self.assertTrue(result)
        mock_unlink.assert_has_calls([
            call(os.path.join('docs', 'file1.txt')),
            call(os.path.join('docs', 'subdir', 'file2.txt'))
        ], any_order=True)
        mock_rmtree.assert_called_once_with(os.path.join('docs', 'subdir'))

    @patch('builtins.input', return_value='n')
    def test_create_docs_directory_cancel(self, mock_input):
        result = create_docs_directory()
        self.assertFalse(result)

    @patch('builtins.input', return_value='y')
    @patch('os.makedirs', side_effect=[OSError, None, None])  # First call raises OSError, next two succeed
    @patch('time.sleep')
    def test_create_docs_directory_retry(self, mock_sleep, mock_makedirs, mock_input):
        with patch('os.path.exists', return_value=False):
            result = create_docs_directory(max_retries=2)
            self.assertTrue(result)
            self.assertEqual(mock_makedirs.call_count, 3)  # Two calls for 'docs', one for 'static'
            mock_sleep.assert_called_once()

    def test_special_characters_in_filenames(self):
        special_files = [
            'file with spaces.f90',
            'file-with-hyphens.f90',
            'file_with_underscores.f90',
            'file.with.periods.f90',
            'file(with)parentheses.f90',
            'file[with]brackets.f90',
            'file{with}braces.f90',
            'file#with#hashes.f90',
            'file%with%percent.f90',
            'file&with&ampersand.f90',
            'file+with+plus.f90',
            'file=with=equal.f90',
            'file@with@at.f90',
            'file!with!exclamation.f90',
            'file*with*asterisk.f90',
            'file|with|pipe.f90',
            'file:with:colons.f90',
            'file;with;semicolons.f90',
            'file\'with\'singlequotes.f90',
            'file"with"doublequotes.f90',
            'file<with<lessthan.f90',
            'file>with>greaterthan.f90',
            'file?with?questionmark.f90',
            'file,with,commas.f90',
        ]

        for file in special_files:
            Path(self.temp_dir, file).touch()

        files = find_files_by_extensions(self.temp_dir)
        self.assertEqual(len(files), len(special_files))
        for file in special_files:
            self.assertIn(Path(file), files)

    def test_whitespace_in_filenames(self):
        whitespace_files = [
            'file with spaces.f90',
            'file\twith\ttabs.f90',
            'file\nwith\nnewlines.f90',
            'file\rwith\rcarriagereturns.f90',
        ]

        for file in whitespace_files:
            Path(self.temp_dir, file).touch()

        files = find_files_by_extensions(self.temp_dir)
        self.assertEqual(len(files), len(whitespace_files))
        for file in whitespace_files:
            self.assertIn(Path(file), files)

    def test_mixed_cases(self):
        mixed_case_files = [
            'FileWithMixedCases.f90',
            'UPPERCASEFILE.F90',
            'lowercase.f90',
        ]

        for file in mixed_case_files:
            Path(self.temp_dir, file).touch()

        files = find_files_by_extensions(self.temp_dir)
        self.assertEqual(len(files), len(mixed_case_files))
        for file in mixed_case_files:
            self.assertIn(Path(file), files)

    def test_nested_directories(self):
        nested_structure = [
            'dir1/file1.f90',
            'dir1/subdir/file2.f90',
            'dir2/file3.f90'
        ]

        for file_path in nested_structure:
            full_path = Path(self.temp_dir, file_path)
            full_path.parent.mkdir(parents=True, exist_ok=True)
            full_path.touch()

        files = find_files_by_extensions(self.temp_dir)
        self.assertEqual(len(files), len(nested_structure))
        for file_path in nested_structure:
            self.assertIn(Path(file_path), files)

    def test_non_f90_files(self):
        mixed_files = [
            'file1.f90',
            'file2.txt',
            'file3.py',
            'file4.f95'
        ]

        for file in mixed_files:
            Path(self.temp_dir, file).touch()

        files = find_files_by_extensions(self.temp_dir)
        self.assertEqual(len(files), 1)
        self.assertIn(Path('file1.f90'), files)

        files = find_files_by_extensions(self.temp_dir, extensions={'f90', 'f95'})
        self.assertEqual(len(files), 2)
        self.assertIn(Path('file1.f90'), files)
        self.assertIn(Path('file4.f95'), files)

    def test_empty_directory(self):
        files = find_files_by_extensions(self.temp_dir)
        self.assertEqual(len(files), 0)

    def test_absolute_and_relative_paths(self):
        abs_path = Path(self.temp_dir, 'abs', 'file1.f90')
        rel_path = Path('rel', 'file2.f90')

        abs_path.parent.mkdir(parents=True, exist_ok=True)
        abs_path.touch()

        rel_dir = Path(self.temp_dir, rel_path.parent)
        rel_dir.mkdir(parents=True, exist_ok=True)
        (rel_dir / rel_path.name).touch()

        files = find_files_by_extensions(self.temp_dir)
        self.assertEqual(len(files), 2)
        self.assertIn(Path('abs/file1.f90'), files)
        self.assertIn(Path('rel/file2.f90'), files)

    @patch('doc4for.utils.file_utils.os.access')
    def test_check_write_permissions_mock(self, mock_access):
        mock_access.return_value = True
        self.assertTrue(check_write_permissions('/some/path'))
        mock_access.assert_called_once_with(Path('/some/path'), os.W_OK)

        mock_access.reset_mock()

        mock_access.return_value = False
        self.assertFalse(check_write_permissions('/some/other/path'))
        mock_access.assert_called_once_with(Path('/some/other/path'), os.W_OK)

    @patch('builtins.input', return_value='y')
    @patch('os.makedirs')
    @patch('os.path.exists', side_effect=[False, True])
    def test_create_docs_directory_race_condition(self, mock_exists, mock_makedirs, mock_input):
        mock_makedirs.side_effect = [FileExistsError, None, None]  # Add a third None for the 'static' directory
        result = create_docs_directory(max_retries=2)
        self.assertTrue(result)
        self.assertEqual(mock_makedirs.call_count, 3)  # Expect 3 calls: 2 for 'docs' and 1 for 'static'
