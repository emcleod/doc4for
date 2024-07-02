import unittest
from pathlib import Path
from pyfakefs.fake_filesystem_unittest import TestCase
from doc4for.f90.generate_file_tree import extract_file_data

class TestDescriptionExtraction(TestCase):
    def setUp(self):
        self.setUpPyfakefs()

    def test_no_description(self):
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
        self.assertEqual(file_data['file_description'], '')

    def test_comment_but_not_description(self):
        self.fs.create_file(
            '/fake/path/modules.f90',
            contents='''\
module module1
    implicit none
    ! Module 1 code
end module module1

!!*
! Here is a comment that is not part of the file description and should not be included.
!*!
''')
        result = extract_file_data([Path('/fake/path/modules.f90')])
        self.assertEqual(len(result), 1)
        file_data = result[0]
        self.assertEqual(file_data['file_name'], '/fake/path/modules.f90')
        self.assertEqual(file_data['file_description'], '')

    def test_single_line_comment(self):
        self.fs.create_file(
            '/fake/path/modules.f90',
            contents='''\
!!* Here is the description of the purpose of this file. *!
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
        self.assertEqual(file_data['file_description'], 'Here is the description of the purpose of this file.\n')

    def test_multiline_comment(self):
        self.fs.create_file(
            '/fake/path/modules.f90',
            contents='''\
!!*
! Here is the description of the purpose of this file. It is a multi-line
! comment because I want to test that.
!*!
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
        self.assertEqual(file_data['file_description'], '\nHere is the description of the purpose of this file. \
It is a multi-line\ncomment because I want to test that.\n\n')

    def test_not_doc4for_comment(self):
        self.fs.create_file(
            '/fake/path/modules.f90',
            contents='''\
!!
! Here is the description of the purpose of this file. It is a multi-line
! comment because I want to test that. However, it is not surrounded by
! !!* !*! and so is not part of the description.
!!
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
        self.assertEqual(file_data['file_description'], '')

if __name__ == '__main__':
    unittest.main()