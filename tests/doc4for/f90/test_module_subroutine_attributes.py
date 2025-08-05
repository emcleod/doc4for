import unittest
from pathlib import Path
from pyfakefs.fake_filesystem_unittest import TestCase
from doc4for.f90.generate_module_tree import extract_module_data

class TestSubroutines(TestCase):
    def setUp(self):
        self.setUpPyfakefs()

    def test_find_pure_subroutines(self):
        self.fs.create_file(
            "/fake/path/pure.f90",
            contents="""\
module pure_subroutines
contains
!!*
! A test pure subroutine
!*!
pure subroutine test()
end subroutine test
end module pure_subroutines
                            """,
        )
        result = extract_module_data([Path("/fake/path/pure.f90")])

        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(module["module_name"], "pure_subroutines")
        self.assertEqual(module["file_name"], "/fake/path/pure.f90")
        self.assertIn("module_description", module)
        self.assertEqual(len(module["subroutines"]), 1)
        self.assertIn("test", module["subroutines"])
        subroutine = module["subroutines"]["test"]
        self.assertIn("attributes", subroutine)
        self.assertEqual(subroutine["description"], "A test pure subroutine\n")
        attributes = subroutine["attributes"]
        self.assertCountEqual(attributes, ["PURE", "PUBLIC"])

    def test_find_elemental_subroutines(self):
        self.fs.create_file(
            "/fake/path/elemental.f90",
            contents="""\
    module elemental_subroutines
    contains
    !!*
    ! An elemental subroutine
    !*!
    elemental subroutine elem_sub(x, y)
        real, intent(in) :: x
        real, intent(out) :: y
        y = x * 2
    end subroutine elem_sub
    end module elemental_subroutines
                            """,
        )
        result = extract_module_data([Path("/fake/path/elemental.f90")])

        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(module["module_name"], "elemental_subroutines")
        self.assertIn("module_description", module)
        self.assertEqual(len(module["subroutines"]), 1)
        self.assertIn("elem_sub", module["subroutines"])
        subroutine = module["subroutines"]["elem_sub"]
        self.assertIn("attributes", subroutine)
        attributes = subroutine["attributes"]
        self.assertEqual(attributes, ["ELEMENTAL", "PUBLIC"])

    def test_find_recursive_subroutines(self):
        self.fs.create_file(
            "/fake/path/recursive.f90",
            contents="""\
    module recursive_subroutines
    contains
    !!*
    ! A recursive subroutine
    !*!
    recursive subroutine fact(n, res)
        integer, intent(in) :: n
        integer, intent(out) :: res
        if (n <= 1) then
            res = 1
        else
            call fact(n-1, res)
            res = n * res
        end if
    end subroutine fact
    end module recursive_subroutines
                            """,
        )
        result = extract_module_data([Path("/fake/path/recursive.f90")])

        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertIn("module_description", module)
        self.assertEqual(module["module_name"], "recursive_subroutines")
        self.assertEqual(len(module["subroutines"]), 1)
        self.assertIn("fact", module["subroutines"])
        subroutine = module["subroutines"]["fact"]
        self.assertIn("attributes", subroutine)
        attributes = subroutine["attributes"]
        self.assertEqual(attributes, ["RECURSIVE", "PUBLIC"])

    def test_find_combined_subroutines(self):
        self.fs.create_file(
            "/fake/path/combined.f90",
            contents="""\
    module combined_subroutines
    contains
    !!*
    ! A pure elemental subroutine
    !*!
    pure elemental subroutine square(x, y)
        real, intent(in) :: x
        real, intent(out) :: y
        y = x * x
    end subroutine square

    !!*
    ! A recursive subroutine
    !*!
    recursive subroutine fact(n, res)
        integer, intent(in) :: n
        integer, intent(out) :: res
        if (n <= 1) then
            res = 1
        else
            call fact(n-1, res)
            res = n * res
        end if
    end subroutine fact
    end module combined_subroutines
                            """,
        )
        result = extract_module_data([Path("/fake/path/combined.f90")])

        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertIn("module_description", module)
        self.assertEqual(module["module_name"], "combined_subroutines")
        self.assertEqual(len(module["subroutines"]), 2)
        self.assertIn("square", module["subroutines"])
        square_subroutine = module["subroutines"]["square"]
        self.assertIn("attributes", square_subroutine)
        square_attributes = square_subroutine["attributes"]
        self.assertEqual(square_attributes, ["PURE", "ELEMENTAL", "PUBLIC"])

        self.assertIn("fact", module["subroutines"])
        fact_subroutine = module["subroutines"]["fact"]
        self.assertIn("attributes", fact_subroutine)
        fact_attributes = fact_subroutine["attributes"]
        self.assertEqual(fact_attributes, ["RECURSIVE", "PUBLIC"])

if __name__ == "__main__":
    unittest.main()