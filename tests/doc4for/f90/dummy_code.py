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
module test_mod
    implicit none

    !integer :: x[*]
    !real, allocatable :: cube(:,:,:)
    real :: x(10, 20, 40)

end module test_mod
        ''',
        )

        result = extract_module_data([Path('/fake/path/test.f90')])
        placeholder = 1
        
if __name__ == "__main__":
    unittest.main()