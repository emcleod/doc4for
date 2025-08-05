import unittest
from pathlib import Path
from typing import Optional
from pyfakefs.fake_filesystem_unittest import TestCase
from doc4for.f90.generate_module_tree import extract_module_data
from doc4for.f90.generate_file_tree import extract_file_data
from doc4for.models.file_models import ProgramDescription
from doc4for.models.module_models import ModuleDescription, BlockDataDescription

class TestUseStatementsInDifferentContexts(TestCase):
    maxDiff = None

    def setUp(self):
        self.setUpPyfakefs()
        
        # Create a utility module that will be used everywhere
        self.fs.create_file(
            "/fake/path/constants.f90",
            contents="""\
!!*
! Physical and mathematical constants module
! Contains commonly used constants for scientific calculations
!*!
module constants
    implicit none
    
    real, parameter :: pi = 3.14159265359
    real, parameter :: e = 2.71828182846
    integer, parameter :: max_iterations = 1000
    
end module constants
"""
        )

    def test_use_statement_at_module_level(self):
        """Test USE statements at module level (outside procedures)."""
        self.fs.create_file(
            "/fake/path/geometry.f90",
            contents="""\
!!*
! Geometry calculations module
! Provides basic geometric functions and constants
!*!
module geometry
    !!* Import mathematical constants for geometry calculations *!
    use constants, only: pi
    implicit none
    
    public :: circle_area
    
contains
    !!*
    ! Calculate area of a circle
    ! @in radius Circle radius
    ! @return Circle area
    !*!
    function circle_area(radius) result(area)
        real, intent(in) :: radius
        real :: area
        
        area = pi * radius**2
    end function
end module geometry
"""
        )
        
        result = extract_module_data([
            Path("/fake/path/constants.f90"),
            Path("/fake/path/geometry.f90")
        ])
        
        geometry_module: Optional[ModuleDescription] = next((m for m in result if m["module_name"] == "geometry"), None)
        assert geometry_module is not None

        # Check module-level USE statement
        self.assertEqual(len(geometry_module["uses"]), 1)
        self.assertIn("constants", geometry_module["uses"])
        self.assertEqual(geometry_module["uses"]["constants"]["selections"], ["pi"])
        self.assertEqual(geometry_module["uses"]["constants"]["description"], "Import mathematical constants for geometry calculations\n")

    def test_use_statement_in_function(self):
        """Test USE statements within functions."""
        self.fs.create_file(
            "/fake/path/math_functions.f90",
            contents="""\
!!*
! Mathematical functions module
! Collection of advanced mathematical operations
!*!
module math_functions
    implicit none
    
    public :: exponential_series
    
contains
    !!*
    ! Calculate exponential using series expansion
    ! @in x Input value
    ! @return Exponential of x
    !*!
    function exponential_series(x) result(exp_x)
        !!* Import Euler's number and iteration limit for series calculation *!
        use constants, only: e, max_iterations
        
        real, intent(in) :: x
        real :: exp_x
        integer :: i
        
        exp_x = 1.0
        do i = 1, max_iterations
            exp_x = exp_x + x**i / factorial(i)
        end do
    end function
    
    function factorial(n) result(fact)
        integer, intent(in) :: n
        integer :: fact, i
        fact = 1
        do i = 2, n
            fact = fact * i
        end do
    end function
end module math_functions
"""
        )
        
        result = extract_module_data([
            Path("/fake/path/constants.f90"),
            Path("/fake/path/math_functions.f90")
        ])
        
        math_module = next((m for m in result if m["module_name"] == "math_functions"), None)
        assert math_module is not None
        
        # Check function-level USE statement
        exp_func = math_module["functions"]["exponential_series"]
        self.assertEqual(len(exp_func["uses"]), 1)
        self.assertIn("constants", exp_func["uses"])
        self.assertEqual(set(exp_func["uses"]["constants"]["selections"]), {"e", "max_iterations"})
        # Note: description should be empty for procedure body comments
        self.assertEqual(exp_func["uses"]["constants"]["description"], "")

    def test_use_statement_in_subroutine(self):
        """Test USE statements within subroutines."""
        self.fs.create_file(
            "/fake/path/utilities.f90",
            contents="""\
!!*
! Utility subroutines module
! Helper routines for various calculations
!*!
module utilities
    implicit none
    
    public :: print_convergence_info
    
contains
    !!*
    ! Print convergence information
    ! @in iteration Current iteration number
    ! @in error Current error value
    !*!
    subroutine print_convergence_info(iteration, error)
        !!* Import maximum iterations for convergence checking *!
        use constants, only: max_iterations
        
        integer, intent(in) :: iteration
        real, intent(in) :: error
        
        print *, 'Iteration:', iteration, '/', max_iterations
        print *, 'Error:', error
        
        if (iteration >= max_iterations) then
            print *, 'Maximum iterations reached!'
        end if
    end subroutine
end module utilities
"""
        )
        
        result = extract_module_data([
            Path("/fake/path/constants.f90"),
            Path("/fake/path/utilities.f90")
        ])
        
        utilities_module: Optional[ModuleDescription] = next((m for m in result if m["module_name"] == "utilities"), None)
        assert utilities_module is not None
        
        # Check subroutine-level USE statement
        print_sub = utilities_module["subroutines"]["print_convergence_info"]
        self.assertEqual(len(print_sub["uses"]), 1)
        self.assertIn("constants", print_sub["uses"])
        self.assertEqual(print_sub["uses"]["constants"]["selections"], ["max_iterations"])
        # Note: description should be empty for procedure body comments
        self.assertEqual(print_sub["uses"]["constants"]["description"], "")

    def test_use_statement_in_program(self):
        """Test USE statements within program units."""
        self.fs.create_file(
            "/fake/path/main_program.f90",
            contents="""\
!!*
! Main calculation program
! Demonstrates usage of various mathematical modules
!*!
program main_calculation
    !!* Import all constants for main program calculations *!
    use constants
    implicit none
    
    real :: radius, area
    
    print *, 'Enter radius:'
    read *, radius
    
    area = pi * radius**2
    print *, 'Area:', area
    
end program main_calculation
"""
        )
        
        result = extract_file_data([
            Path("/fake/path/constants.f90"),
            Path("/fake/path/main_program.f90")
        ])
        
        # Programs should be captured in the result
        program_data: ProgramDescription
        for item in result:
            if item.get("file_name") == "/fake/path/main_program.f90":
                program_data = item["programs"]["main_calculation"]
                break
        
        self.assertIsNotNone(program_data)
        
        # Check program-level USE statement
        self.assertEqual(len(program_data["uses"]), 1)
        self.assertIn("constants", program_data["uses"])
        self.assertEqual(program_data["uses"]["constants"]["selections"], [])  # No 'only' clause
        # Note: description should be empty for program body comments
        self.assertEqual(program_data["uses"]["constants"]["description"], "")

    def test_use_statement_in_block_data(self):
        """Test USE statements within block data units."""
        self.fs.create_file(
            "/fake/path/common_data.f90",
            contents="""\
!!*
! Common data initialization
! Initializes common block variables using constants
!*!
block data common_init
    !!* Import constants for common block initialization *!
    use constants, only: pi, e
    implicit none
    
    common /math_constants/ pi_val, e_val
    real :: pi_val, e_val
    
    data pi_val /pi/
    data e_val /e/
    
end block data common_init
"""
        )
        
        result = extract_file_data([
            Path("/fake/path/constants.f90"),
            Path("/fake/path/common_data.f90")
        ])
        
        # Block data should be captured in the result
        block_data: Optional[BlockDataDescription]
        for item in result:
            if item.get("file_name") == "/fake/path/common_data.f90":
                block_data = item["block_data"]["common_init"]
                break
        assert block_data is not None
        
        # Check block data USE statement
        self.assertEqual(len(block_data["uses"]), 1)
        self.assertIn("constants", block_data["uses"])
        self.assertEqual(set(block_data["uses"]["constants"]["selections"]), {"pi", "e"})
        self.assertEqual(block_data["uses"]["constants"]["description"], "Import constants for common block initialization\n")

    def test_multiple_use_statements_same_context(self):
        """Test multiple USE statements in the same context."""
        # Create another utility module
        self.fs.create_file(
            "/fake/path/string_utils.f90",
            contents="""\
!!*
! String utility functions
! Helper functions for string manipulation
!*!
module string_utils
    implicit none
    
    character(len=*), parameter :: version = "1.0.0"
    
end module string_utils
"""
        )
        
        self.fs.create_file(
            "/fake/path/complex_calculation.f90",
            contents="""\
!!*
! Complex calculation module
! Demonstrates multiple imports
!*!
module complex_calculation
    implicit none
    
    public :: complex_function
    
contains
    !!*
    ! Perform complex calculation using multiple modules
    ! @in x Input value
    ! @return Calculated result
    !*!
    function complex_function(x) result(y)
        !!* Import mathematical constants *!
        use constants, only: pi, e
        !!* Import string utilities for version info *!
        use string_utils, only: version
        
        real, intent(in) :: x
        real :: y
        
        y = pi * exp(e * x)
        print *, 'Calculation done with version:', version
    end function
end module complex_calculation
"""
        )
        
        result = extract_module_data([
            Path("/fake/path/constants.f90"),
            Path("/fake/path/string_utils.f90"),
            Path("/fake/path/complex_calculation.f90")
        ])
        
        complex_module: Optional[ModuleDescription] = next((m for m in result if m["module_name"] == "complex_calculation"), None)
        assert complex_module is not None

        # Check multiple USE statements in function
        complex_func = complex_module["functions"]["complex_function"]
        self.assertEqual(len(complex_func["uses"]), 2)
        
        # Check constants import
        self.assertIn("constants", complex_func["uses"])
        self.assertEqual(set(complex_func["uses"]["constants"]["selections"]), {"pi", "e"})
        
        # Check string_utils import
        self.assertIn("string_utils", complex_func["uses"])
        self.assertEqual(complex_func["uses"]["string_utils"]["selections"], ["version"])
        
        # Both should have empty descriptions (procedure body comments)
        self.assertEqual(complex_func["uses"]["constants"]["description"], "")
        self.assertEqual(complex_func["uses"]["string_utils"]["description"], "")

if __name__ == '__main__':
    unittest.main()