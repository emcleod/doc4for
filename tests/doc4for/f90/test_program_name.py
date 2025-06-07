import unittest
from pathlib import Path
from pyfakefs.fake_filesystem_unittest import TestCase
from doc4for.f90.generate_file_tree import extract_file_data

class TestProgramName(TestCase):
    def setUp(self):
        self.setUpPyfakefs()

    def test_single_program_name(self):
        self.fs.create_file(
            "/fake/path/main.f90",
            contents="""\
program my_program
    implicit none
    ! Program code
end program my_program
""")
        result = extract_file_data([Path("/fake/path/main.f90")])
        self.assertEqual(len(result), 1)
        file_data = result[0]
        self.assertEqual(file_data["file_name"], "/fake/path/main.f90")
        self.assertEqual(len(file_data["programs"]), 1)
        program_details = file_data["programs"]["my_program"]
        self.assertEqual(program_details["program_name"], "my_program")

    def test_multiple_program_names(self):
        self.fs.create_file(
            "/fake/path/main.f90",
            contents="""\
program my_program
    implicit none
    ! Program code
end program my_program

program other_program
end program other_program

program main
end program main
""")
        result = extract_file_data([Path("/fake/path/main.f90")])
        self.assertEqual(len(result), 1)
        file_data = result[0]
        self.assertEqual(file_data["file_name"], "/fake/path/main.f90")
        self.assertEqual(len(file_data["programs"]), 3)
        program_details = file_data["programs"]["my_program"]
        self.assertEqual(program_details["program_name"], "my_program")
        program_details = file_data["programs"]["other_program"]
        self.assertEqual(program_details["program_name"], "other_program")
        program_details = file_data["programs"]["main"]
        self.assertEqual(program_details["program_name"], "main")

if __name__ == "__main__":
    unittest.main()