import unittest
from pathlib import Path
from pyfakefs.fake_filesystem_unittest import TestCase
from doc4for.f90.generate_module_tree import extract_module_data


class TestModules(TestCase):
    def setUp(self):
        self.setUpPyfakefs()

    def test_find_modules_with_single_module(self):
        self.fs.create_file(
            "/fake/path/fortran_file.f90",
            contents="""\
!!*
! Test comment
!*!
module test
end module test
""",
        )
        result = extract_module_data([Path("/fake/path/fortran_file.f90")])
        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(module["module_name"], "test")
        self.assertEqual(module["file_name"], "/fake/path/fortran_file.f90")
        self.assertEqual(module["module_description"], "Test comment\n")
        self.assertEqual(module["parameters"], {})
        self.assertEqual(module["functions"], {})
        self.assertEqual(module["subroutines"], {})

    def test_find_modules_with_multiple_modules(self):
        self.fs.create_file(
            "/fake/path/file1.f90",
            contents="""\
!!*
! Module 1
!*!
module test1
end module test1
        """,
        )

        self.fs.create_file(
            "/fake/path/file2.f90",
            contents="""\
!!*
! Module 2
!*!
module test2
end module test2
        """,
        )
        result = extract_module_data([Path("/fake/path/file1.f90"), Path("/fake/path/file2.f90")])
        self.assertEqual(len(result), 2)
        self.assertEqual(result[0]["module_name"], "test1")
        self.assertEqual(result[0]["file_name"], "/fake/path/file1.f90")
        self.assertEqual(result[0]["module_description"], "Module 1\n")
        self.assertEqual(result[1]["module_name"], "test2")
        self.assertEqual(result[1]["file_name"], "/fake/path/file2.f90")
        self.assertEqual(result[1]["module_description"], "Module 2\n")

    def test_find_modules_with_no_modules(self):
        self.fs.create_file(
            "/fake/path/no_modules.f90",
            contents="""\
! Just some code, no modules
print *, "Hello, world!"
        """,
        )
        result = extract_module_data([Path("/fake/path/no_modules.f90")])
        self.assertEqual(len(result), 0)

if __name__ == "__main__":
    unittest.main()
