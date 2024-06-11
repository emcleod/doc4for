import unittest
from pyfakefs.fake_filesystem_unittest import TestCase
import os
import sys

# Add the project root to the Python path
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..")))
from generate_module_tree import process_modules

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
pure function test()
end function test
end module pure_functions
                            """,
        )
        result = process_modules(["/fake/path/pure.f90"])

        self.assertEqual(len(result), 1)
        module = result[0]
        print (module)
        self.assertEqual(module["module_name"], "pure_functions")
        self.assertEqual(module["file_name"], "/fake/path/pure.f90")
        #self.assertEqual(module["module_description"], "")
        # self.assertEqual(module[])

if __name__ == "__main__":
    unittest.main()
