import unittest
import os
from unittest.mock import patch, MagicMock
from pyfakefs.fake_filesystem_unittest import TestCase as FakefsTestCase
from jinja2 import Environment, FileSystemLoader
from doc4for.file_utils import find_f90_files
from doc4for.f90.generate_file_tree import ( 
                                    build_directory_tree,
                                    create_docs_directory,
                                    generate_file_pages)

class TestClass(FakefsTestCase):
    @classmethod
    def setUpClass(cls):
        # Call the parent class's setUpClass method
        super().setUpClass()
        # Read the real template file content
        cls.real_project_root = os.path.abspath(os.path.join(os.path.dirname(__file__), '..', '..'))
        real_template_file = os.path.join(cls.real_project_root, 'templates', 'file_template.html')
        with open(real_template_file, 'r') as f:
            cls.template_content = f.read()

    def setUp(self):
        # Set up the fake file system
        self.setUpPyfakefs()
        # Set up fake project root and template directory
        self.fake_project_root = '/fake_project'
        fake_template_dir = os.path.join(self.fake_project_root, 'templates')
        self.fs.create_dir(fake_template_dir)
        
        # Create the template file in the fake file system
        fake_template_file = os.path.join(fake_template_dir, 'file_template.html')
        self.fs.create_file(fake_template_file, contents=self.template_content)

        # Create the directory structure and files
        self.fs.create_dir('/fake/dir1')
        self.fs.create_dir('/fake/dir2')

        self.fs.create_file(
            '/fake/dir1/simple1.f90',
            contents='''\
module simple1_module
implicit none
contains
  subroutine simple1_sub
    print *, "This is simple1"
  end subroutine simple1_sub
end module simple1_module
            '''
        )

        self.fs.create_file(
            '/fake/dir1/simple2.f90',
            contents='''\
module simple2_module
implicit none
contains
  function simple2_func(x) result(y)
    real, intent(in) :: x
    real :: y
    y = x * 2
  end function simple2_func
end module simple2_module
            '''
        )

        self.fs.create_file(
            '/fake/dir2/simple3.f90',
            contents='''\
module simple3_module
implicit none
contains
  subroutine simple3_sub(a, b)
    integer, intent(in) :: a
    integer, intent(out) :: b
    b = a + 1
  end subroutine simple3_sub
end module simple3_module
            '''
        )

        self.fs.create_file(
            '/fake/main.f90',
            contents='''\
program main
  use simple1_module
  use simple2_module
  use simple3_module
  implicit none
  
  call simple1_sub()
  print *, simple2_func(3.0)
  call simple3_sub(5, b)
  print *, b
end program main
            '''
        )
        # Set up actual directory tree
        self.f90_files = find_f90_files('/fake')
        self.directory_tree = build_directory_tree(self.f90_files)
        self.f90_files = find_f90_files('/fake')
        self.directory_tree = build_directory_tree(self.f90_files)

# The next two tests check that the fake file system is set up properly, not
# the generation of the HTML
    def test_file_structure(self):
        self.assertTrue(self.fs.exists('/fake/dir1'))
        self.assertTrue(self.fs.exists('/fake/dir2'))
        self.assertTrue(self.fs.exists('/fake/dir1/simple1.f90'))
        self.assertTrue(self.fs.exists('/fake/dir1/simple2.f90'))
        self.assertTrue(self.fs.exists('/fake/dir2/simple3.f90'))
        self.assertTrue(self.fs.exists('/fake/main.f90'))

    @patch('f90.generate_file_tree.check_write_permissions')
    def test_create_docs_directory(self, mock_check_permissions):
        mock_check_permissions.return_value = True
        
        # Test case 1: 'docs' directory doesn't exist
        create_docs_directory()
        self.assertTrue(self.fs.exists('docs'))
        self.assertTrue(self.fs.exists('docs/static'))

        # Test case 2: 'docs' directory exists
        self.fs.create_dir('docs/existing_dir')
        create_docs_directory()
        self.assertTrue(self.fs.exists('docs'))
        self.assertTrue(self.fs.exists('docs/static'))
        self.assertFalse(self.fs.exists('docs/existing_dir'))

    @patch('f90.generate_file_tree.Environment', autospec=True)
    @patch('f90.generate_file_tree.FileSystemLoader', autospec=True)
    def test_generate_html_files(self, mock_loader, mock_env):
        template_dir = os.path.join(self.fake_project_root, 'templates')        
        # Set up the Environment mock to use the fake file system
        mock_env_instance = mock_env.return_value
        mock_env_instance.get_template.side_effect = lambda x: Environment(loader=FileSystemLoader(template_dir)).get_template(x)

        # Call the function with actual directory tree, template directory, and base directory
        generate_file_pages(self.directory_tree, template_dir=template_dir, base_dir='/fake')        
        # Check if HTML files were generated
        self.assertTrue(self.fs.exists('docs/dir1/simple1.html'))
        self.assertTrue(self.fs.exists('docs/dir1/simple2.html'))
        self.assertTrue(self.fs.exists('docs/dir2/simple3.html'))
        self.assertTrue(self.fs.exists('docs/main.html'))
        self.assertTrue(self.fs.exists('docs/index.html'))



if __name__ == '__main__':
    unittest.main()