import unittest
from pathlib import Path
from pyfakefs.fake_filesystem_unittest import TestCase
from doc4for.f90.generate_module_tree import extract_module_data

class TestFunctions(TestCase):
    def setUp(self):
        self.setUpPyfakefs()

    def test_find_pure_functions(self):
        self.fs.create_file(
            "/fake/path/pure.f90",
            contents="""\
module pure_functions
contains
!!*
! A test pure function
!*!
pure real function test()
end function test
end module pure_functions
""",
        )
        result = extract_module_data([Path("/fake/path/pure.f90")])

        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(module["module_name"], "pure_functions")
        self.assertEqual(module["file_name"], "/fake/path/pure.f90")
        self.assertIn("module_description", module)
        self.assertEqual(len(module["functions"]), 1)
        self.assertIn("test", module["functions"])
        function = module["functions"]["test"]
        self.assertIn("attributes", function)
        self.assertEqual(function["description"], "A test pure function\n")
        attributes = function["attributes"]
        self.assertCountEqual(attributes, ["PURE", "PUBLIC"])
        return_value = function["return"]
        self.assertEqual(return_value["type"], "REAL")        

    def test_find_elemental_functions(self):
        self.fs.create_file(
            "/fake/path/elemental.f90",
            contents="""\
    module elemental_functions
    contains
    !!*
    ! An elemental function
    !*!
    elemental function elem_func()
        elem_func = 42
    end function elem_func
    end module elemental_functions
                            """,
        )
        result = extract_module_data([Path("/fake/path/elemental.f90")])

        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(module["module_name"], "elemental_functions")
        self.assertIn("module_description", module)
        self.assertEqual(len(module["functions"]), 1)
        self.assertIn("elem_func", module["functions"])
        function = module["functions"]["elem_func"]
        self.assertIn("attributes", function)
        attributes = function["attributes"]
        self.assertCountEqual(attributes, ["ELEMENTAL", "PUBLIC"])


    def test_find_recursive_functions(self):
        self.fs.create_file(
            "/fake/path/recursive.f90",
            contents="""\
    module recursive_functions
    contains
    !!*
    ! A recursive function
    !*!
    recursive function fact() result(res)
        integer :: res
        res = 1
    end function fact
    end module recursive_functions
                            """,
        )
        result = extract_module_data([Path("/fake/path/recursive.f90")])

        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertIn("module_description", module)
        self.assertEqual(module["module_name"], "recursive_functions")
        self.assertEqual(len(module["functions"]), 1)
        self.assertIn("fact", module["functions"])
        function = module["functions"]["fact"]
        self.assertIn("attributes", function)
        attributes = function["attributes"]
        self.assertCountEqual(attributes, ["RECURSIVE", "PUBLIC"])

    def test_find_combined_functions(self):
        self.fs.create_file(
            "/fake/path/combined.f90",
            contents="""\
    module combined_functions
    contains
    !!*
    ! A pure elemental function
    !*!
    pure elemental function square()
        square = 4
    end function square

    !!*
    ! A recursive function
    !*!
    recursive impure function fact() result(res)
        integer :: res
        res = 1
    end function fact
    end module combined_functions
                            """,
        )
        result = extract_module_data([Path("/fake/path/combined.f90")])

        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertIn("module_description", module)
        self.assertEqual(module["module_name"], "combined_functions")
        self.assertEqual(len(module["functions"]), 2)
        self.assertIn("square", module["functions"])
        square_function = module["functions"]["square"]
        self.assertIn("attributes", square_function)
        square_attributes = square_function["attributes"]
        self.assertCountEqual(square_attributes, ["PURE", "ELEMENTAL", "PUBLIC"])

        self.assertIn("fact", module["functions"])
        fact_function = module["functions"]["fact"]
        self.assertIn("attributes", fact_function)
        fact_attributes = fact_function["attributes"]
        self.assertCountEqual(fact_attributes, ["RECURSIVE", "IMPURE", "PUBLIC"])

if __name__ == "__main__":
    unittest.main()
