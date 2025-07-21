import unittest
from pathlib import Path
from pyfakefs.fake_filesystem_unittest import TestCase
from doc4for.f90.generate_file_tree import extract_file_data

class TestProgramDocumentation(TestCase):
    maxDiff = None

    def setUp(self):
        self.setUpPyfakefs()

    def test_program_documentation(self):
        self.fs.create_file(
            "/fake/path/programs.f90",
            contents="""\
    
    subroutine test
    end subroutine test
    !!*
    ! Main program that processes climate data
    ! Reads input files, calculates averages, and generates reports
    ! @version 1.2.3
    ! @author Jane Smith
    ! @date 2024-01-20
    !*!
    program process_climate_data
        implicit none
        ! Program contents...
    end program process_climate_data

    ! Regular comment, not documentation
    program no_doc_program
        implicit none
        ! Another regular comment
    end program no_doc_program

    !!*
    ! Simple utility program
    ! Just a basic example
    !*!
    program utility
        implicit none
    end program utility

    subroutine helper()
        ! This should be ignored
    end subroutine helper
    """
        )
        result = extract_file_data([Path("/fake/path/programs.f90")])
        self.assertEqual(len(result), 1)
        file_data = result[0]
        self.assertEqual(len(file_data["programs"]), 3)

        # Documented program with metadata
        prog_doc = file_data["programs"]["process_climate_data"]
        expected_doc = {
            "program_name": "process_climate_data",
            "file_name": "/fake/path/programs.f90",
            "program_description": 'Main program that processes climate data\nReads '\
                'input files, calculates averages, and generates reports\n@version'\
                     ' 1.2.3\n@author Jane Smith\n@date 2024-01-20\n',
            "uses": {},
            "external_procedures": {}
        }
        self.assertEqual(prog_doc, expected_doc)

        # Undocumented program
        prog_no_doc = file_data["programs"]["no_doc_program"]
        expected_no_doc = {
            "program_name": "no_doc_program",
            "file_name": "/fake/path/programs.f90",
            "uses": {},
            "program_description": "",
            "external_procedures": {}
        }
        self.assertEqual(prog_no_doc, expected_no_doc)

        # Simple documented program without metadata
        prog_simple = file_data["programs"]["utility"]
        expected_simple = {
            "program_name": "utility",
            "file_name": "/fake/path/programs.f90",
            "uses": {},
            "program_description": "Simple utility program\nJust a basic example\n",
            "external_procedures": {}
        }
        self.assertEqual(prog_simple, expected_simple)


if __name__ == '__main__':
    unittest.main()
