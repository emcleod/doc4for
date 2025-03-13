import unittest
from pathlib import Path
from pyfakefs.fake_filesystem_unittest import TestCase
from doc4for.models.common import Expression, ExpressionType
from doc4for.f90.generate_module_tree import extract_module_data

class DummyCode(TestCase):

    def setUp(self):
        self.setUpPyfakefs()

    def test(self):
        self.fs.create_file(
            '/fake/path/test.f90',
            contents='''\
module enum_examples
    use iso_fortran_env, other_module, another_module
    implicit none
    
end module enum_examples

program test
    write (5, *) "Hello, World!"
end program test    

        ''',
        )

        result = extract_module_data([Path('/fake/path/test.f90')])
        placeholder = 1
        
if __name__ == "__main__":
    unittest.main()