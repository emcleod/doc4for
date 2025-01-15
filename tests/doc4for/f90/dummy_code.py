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

    ! Define some constants to use in the derived types
    real :: DEFAULT_X = 0.0
    real :: DEFAULT_Y = 0.0
    real :: PI = 3.14159

    type :: point
        real :: x, y
    end type point

    type :: line
        type(point) :: start, end
    end type line

    ! Using named constants in derived type parameters
    type(point) :: ORIGIN = point(DEFAULT_X, DEFAULT_Y)
    
    ! Using expressions in derived type parameters
    type(point) :: UNIT_X = point(1.0, 0.0)
    type(point) :: UNIT_Y = point(0.0, 1.0)
    
    ! Array of derived type parameters with expressions
    type(point), parameter :: CORNERS(4) = [ &
        point(-1.0, -1.0), &
        point( 1.0, -1.0), &
        point( 1.0,  1.0), &
        point(-1.0,  1.0)  &
    ]
    
    ! Nested derived type parameter using other parameters
    type(line) :: X_AXIS = line(ORIGIN, UNIT_X)
    
    ! Parameter using mathematical expressions
    type(point) :: CIRCLE_POINT = point(cos(PI/4), sin(PI/4))

end module test_mod
        ''',
        )

        result = extract_module_data([Path('/fake/path/test.f90')])

        
if __name__ == "__main__":
    unittest.main()