import unittest
from pathlib import Path
from pyfakefs.fake_filesystem_unittest import TestCase
from doc4for.f90.generate_file_tree import extract_file_data

class TestProgramUses(TestCase):
    def setUp(self):
        self.setUpPyfakefs()

    def test_program_single_use(self):
        self.fs.create_file(
            "/fake/path/main.f90",
            contents="""\
program my_program
    implicit none
    use other_module
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
        self.assertEqual(len(program_details["uses"]), 1)
        self.assertEqual(program_details["uses"]["other_module"]["module_name"], "other_module")
        self.assertEqual(program_details["uses"]["other_module"]["selections"], [])

    def test_program_multiple_use(self):
        self.fs.create_file(
            "/fake/path/main.f90",
            contents="""\
program my_program
    implicit none
    use other_module
    use test_module
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
        self.assertEqual(len(program_details["uses"]), 2)
        self.assertEqual(program_details["uses"]["other_module"]["module_name"], "other_module")
        self.assertEqual(program_details["uses"]["other_module"]["selections"], [])
        self.assertEqual(program_details["uses"]["test_module"]["module_name"], "test_module")
        self.assertEqual(program_details["uses"]["test_module"]["selections"], [])

    def test_program_use_only_single(self):
        self.fs.create_file(
            "/fake/path/main.f90",
            contents="""\
program my_program
    implicit none
    use iso_fortran_env, only: real64
end program my_program
""")
        result = extract_file_data([Path("/fake/path/main.f90")])
        self.assertEqual(len(result), 1)
        file_data = result[0]
        self.assertEqual(file_data["file_name"], "/fake/path/main.f90")
        self.assertEqual(len(file_data["programs"]), 1)
        program_details = file_data["programs"]["my_program"]
        self.assertEqual(len(program_details["uses"]), 1)
        self.assertEqual(program_details["uses"]["iso_fortran_env"]["module_name"], "iso_fortran_env")
        self.assertEqual(program_details["uses"]["iso_fortran_env"]["selections"], ["real64"])

    def test_program_use_only_multiple(self):
        self.fs.create_file(
            "/fake/path/main.f90",
            contents="""\
program my_program
    implicit none
    use iso_fortran_env, only: real64, int32
end program my_program
""")
        result = extract_file_data([Path("/fake/path/main.f90")])
        self.assertEqual(len(result), 1)
        file_data = result[0]
        self.assertEqual(file_data["file_name"], "/fake/path/main.f90")
        self.assertEqual(len(file_data["programs"]), 1)
        program_details = file_data["programs"]["my_program"]
        self.assertEqual(len(program_details["uses"]), 1)
        self.assertEqual(program_details["uses"]["iso_fortran_env"]["module_name"], "iso_fortran_env")
        self.assertEqual(program_details["uses"]["iso_fortran_env"]["selections"], ["real64", "int32"])

    def test_program_use_many_only(self):
        self.fs.create_file(
            "/fake/path/main.f90",
            contents="""\
program my_program
    implicit none
    use iso_fortran_env, only: real64, int32
    use my_module, only: test1
end program my_program
""")
        self.fs.create_file(
            "/fake/path/my_module.f90",
            contents="""\
module my_module
contains
    subroutine test1()
        print *, "You are here"
    end subroutine test1
    subroutine test2()
        print *, "You are here"
    end subroutine test2
end module my_module
"""
        )
        result = extract_file_data([Path("/fake/path/main.f90")])
        self.assertEqual(len(result), 1)
        file_data = result[0]
        self.assertEqual(file_data["file_name"], "/fake/path/main.f90")
        self.assertEqual(len(file_data["programs"]), 1)
        program_details = file_data["programs"]["my_program"]
        self.assertEqual(len(program_details["uses"]), 2)
        self.assertEqual(program_details["uses"]["iso_fortran_env"]["module_name"], "iso_fortran_env")
        self.assertEqual(program_details["uses"]["iso_fortran_env"]["selections"], ["real64", "int32"])
        self.assertEqual(program_details["uses"]["my_module"]["selections"], ["test1"])

    def test_program_use_many_only_repeated_declarations(self):
        self.fs.create_file(
            "/fake/path/main.f90",
            contents="""\
program my_program
    implicit none
    use iso_fortran_env, only: real64, int32
    use my_module, only: test1
    use iso_fortran_env, only: real32
end program my_program
""")
        self.fs.create_file(
            "/fake/path/my_module.f90",
            contents="""\
module my_module
contains
    subroutine test1()
        print *, "You are here"
    end subroutine test1
    subroutine test2()
        print *, "You are here"
    end subroutine test2
end module my_module
"""
        )
        result = extract_file_data([Path("/fake/path/main.f90")])
        self.assertEqual(len(result), 1)
        file_data = result[0]
        self.assertEqual(file_data["file_name"], "/fake/path/main.f90")
        self.assertEqual(len(file_data["programs"]), 1)
        program_details = file_data["programs"]["my_program"]
        self.assertEqual(len(program_details["uses"]), 2)
        self.assertEqual(program_details["uses"]["iso_fortran_env"]["module_name"], "iso_fortran_env")
        self.assertEqual(program_details["uses"]["iso_fortran_env"]["selections"], ["real64", "int32", "real32"])
        self.assertEqual(program_details["uses"]["my_module"]["module_name"], "my_module")
        self.assertEqual(program_details["uses"]["my_module"]["selections"], ["test1"])

    def test_program_repeated_use_removes_selections(self):
        self.fs.create_file(
            "/fake/path/main.f90",
            contents="""\
program my_program
    implicit none
    use iso_fortran_env, only: real64, int32
    use my_module, only: test1
    use iso_fortran_env
end program my_program
""")
        self.fs.create_file(
            "/fake/path/my_module.f90",
            contents="""\
module my_module
contains
    subroutine test1()
        print *, "You are here"
    end subroutine test1
end module my_module
"""
        )
        result = extract_file_data([Path("/fake/path/main.f90")])
        self.assertEqual(len(result), 1)
        file_data = result[0]
        self.assertEqual(file_data["file_name"], "/fake/path/main.f90")
        self.assertEqual(len(file_data["programs"]), 1)
        program_details = file_data["programs"]["my_program"]
        self.assertEqual(len(program_details["uses"]), 2)
        self.assertEqual(program_details["uses"]["iso_fortran_env"]["module_name"], "iso_fortran_env")
        self.assertEqual(program_details["uses"]["iso_fortran_env"]["selections"], [])
        self.assertEqual(program_details["uses"]["my_module"]["module_name"], "my_module")
        self.assertEqual(program_details["uses"]["my_module"]["selections"], ["test1"])

    def test_program_use_with_renames(self):
        self.fs.create_file(
            "/fake/path/main.f90",
            contents="""\
program my_program
    implicit none
    use iso_fortran_env, only: big_real => real64, medium_int => int32
    use my_module, only: print_test => test1
end program my_program
""")
        self.fs.create_file(
            "/fake/path/my_module.f90",
            contents="""\
module my_module
contains
    subroutine test1()
        print *, "You are here"
    end subroutine test1
end module my_module
"""
        )
        result = extract_file_data([Path("/fake/path/main.f90")])
        self.assertEqual(len(result), 1)
        file_data = result[0]
        self.assertEqual(file_data["file_name"], "/fake/path/main.f90")
        self.assertEqual(len(file_data["programs"]), 1)
        program_details = file_data["programs"]["my_program"]
        self.assertEqual(len(program_details["uses"]), 2)
        
        # Check iso_fortran_env imports with renames
        iso_uses = program_details["uses"]["iso_fortran_env"]
        self.assertEqual(iso_uses["module_name"], "iso_fortran_env")
        self.assertEqual(len(iso_uses["selections"]), 2)
        
        # Check that the renamed imports are correctly stored
        # First renamed import: big_real => real64
        self.assertIn(("big_real", "real64"), iso_uses["selections"])
        # Second renamed import: medium_int => int32
        self.assertIn(("medium_int", "int32"), iso_uses["selections"])
        
        # Check my_module imports with renames
        my_module_uses = program_details["uses"]["my_module"]
        self.assertEqual(my_module_uses["module_name"], "my_module")
        self.assertEqual(len(my_module_uses["selections"]), 1)
        self.assertIn(("print_test", "test1"), my_module_uses["selections"])

if __name__ == "__main__":
    unittest.main()