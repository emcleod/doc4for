import unittest
from unittest.mock import patch
import os
from pyfakefs.fake_filesystem_unittest import TestCase
from f90.generate_file_tree import create_docs_directory, check_write_permissions

class TestCreateDocsDirectory(TestCase):
    def setUp(self):
        self.setUpPyfakefs()

    def test_create_new_docs_directory(self):
        """Test creating a new docs directory when it doesn't exist."""
        create_docs_directory()
        self.assertTrue(os.path.exists('docs'))
        self.assertEqual(os.listdir('docs'), ['static'])

    def test_clear_existing_docs_directory(self):
        """Test clearing an existing docs directory."""
        # Create a docs directory with some content
        os.makedirs('docs')
        with open('docs/test_file.txt', 'w') as f:
            f.write('test content')
        os.makedirs('docs/test_dir')
        
        create_docs_directory()
        
        self.assertTrue(os.path.exists('docs'))
        self.assertEqual(os.listdir('docs'), [])

    def test_preserve_static_directory(self):
        """Test that the static directory is preserved when clearing docs."""
        os.makedirs('docs/static')
        with open('docs/test_file.txt', 'w') as f:
            f.write('test content')
        
        create_docs_directory()
        
        self.assertTrue(os.path.exists('docs'))
        self.assertEqual(os.listdir('docs'), ['static'])

    @patch('shutil.rmtree')
    @patch('os.unlink')
    def test_exception_handling(self, mock_unlink, mock_rmtree):
        """Test that exceptions are properly raised."""
        mock_unlink.side_effect = OSError("Permission denied")
        mock_rmtree.side_effect = OSError("Permission denied")

        os.makedirs('docs')
        with open('docs/test_file.txt', 'w') as f:
            f.write('test content')
        os.makedirs('docs/test_dir')

        with self.assertRaises(OSError):
            create_docs_directory()

    def test_no_write_permissions(self):
        """Test that PermissionError is raised when there are no write permissions."""
        # Mock the check_write_permissions function to always return False
        with patch('f90.generate_file_tree.check_write_permissions', return_value=False):
            with self.assertRaises(PermissionError):
                create_docs_directory()

    def test_check_write_permissions(self):
        """Test the check_write_permissions function."""
        # Create a directory with write permissions
        os.makedirs('writable_dir', mode=0o755)
        self.assertTrue(check_write_permissions('writable_dir'))

        # Create a directory without write permissions
        os.makedirs('non_writable_dir', mode=0o555)
        self.assertFalse(check_write_permissions('non_writable_dir'))

    def test_handle_symlinks(self):
        """Test that symlinks are correctly handled."""
        os.makedirs('docs')
        os.makedirs('external_dir')
        
        # Create a file symlink
        with open('docs/real_file.txt', 'w') as f:
            f.write('content')
        os.symlink('real_file.txt', 'docs/file_link.txt')
        
        # Create a directory symlink
        os.symlink('external_dir', 'docs/dir_link')
        
        # Create a broken symlink
        os.symlink('non_existent', 'docs/broken_link')

        create_docs_directory()

        self.assertTrue(os.path.exists('docs'))
        self.assertFalse(os.path.exists('docs/file_link.txt'))
        self.assertFalse(os.path.exists('docs/dir_link'))
        self.assertFalse(os.path.exists('docs/broken_link'))
        self.assertTrue(os.path.exists('external_dir'))  # Should not be deleted

    def test_unicode_filenames(self):
        """Test handling of Unicode filenames."""
        os.makedirs('docs')
        
        # Create files with Unicode names
        unicode_names = ['文件.txt', 'フアイル.txt', 'Århus.txt', 'Москва.txt']
        for name in unicode_names:
            with open(os.path.join('docs', name), 'w') as f:
                f.write('content')

        create_docs_directory()

        self.assertTrue(os.path.exists('docs'))
        for name in unicode_names:
            self.assertFalse(os.path.exists(os.path.join('docs', name)))

if __name__ == '__main__':
    unittest.main()