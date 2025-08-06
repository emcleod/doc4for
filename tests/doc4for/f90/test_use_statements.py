import unittest
from pathlib import Path
from typing import Optional
from pyfakefs.fake_filesystem_unittest import TestCase
from doc4for.f90.generate_module_tree import extract_module_data
from doc4for.f90.generate_file_tree import extract_file_data
from doc4for.models.file_models import ProgramDescription
from doc4for.models.module_models import ModuleDescription, BlockDataDescription
from doc4for.models.common import UseType

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
        self.assertEqual(geometry_module["uses"]["constants"]["use_type"], UseType.NONE)

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
        self.assertEqual(exp_func["uses"]["constants"]["use_type"], UseType.NONE)

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
        self.assertEqual(print_sub["uses"]["constants"]["use_type"], UseType.NONE)

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
        self.assertEqual(program_data["uses"]["constants"]["use_type"], UseType.NONE)

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
        self.assertEqual(block_data["uses"]["constants"]["use_type"], UseType.NONE)

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
        self.assertEqual(complex_func["uses"]["constants"]["use_type"], UseType.NONE)
        
        # Check string_utils import
        self.assertIn("string_utils", complex_func["uses"])
        self.assertEqual(complex_func["uses"]["string_utils"]["selections"], ["version"])
        self.assertEqual(complex_func["uses"]["string_utils"]["use_type"], UseType.NONE)
        
        # Both should have empty descriptions (procedure body comments)
        self.assertEqual(complex_func["uses"]["constants"]["description"], "")
        self.assertEqual(complex_func["uses"]["string_utils"]["description"], "")

    def test_use_statement_with_intrinsic(self):
        """Test USE statements with intrinsic modules."""
        self.fs.create_file(
            "/fake/path/precision_module.f90",
            contents="""\
    !!*
    ! Precision and environment module
    ! Defines precision parameters using intrinsic modules
    !*!
    module precision_module
        !!* Import standard Fortran environment parameters *!
        use, intrinsic :: iso_fortran_env, only: int32, int64, real32, real64
        !!* Import C interoperability types *!
        use, intrinsic :: iso_c_binding, only: c_int, c_double
        implicit none
        
        integer, parameter :: sp = real32
        integer, parameter :: dp = real64
        
    contains
        !!*
        ! Get compiler version information
        ! @return Compiler version string
        !*!
        function get_compiler_info() result(info)
            !!* Import compiler version from intrinsic module *!
            use, intrinsic :: iso_fortran_env, only: compiler_version
            
            character(len=:), allocatable :: info
            
            info = compiler_version()
        end function
    end module precision_module
    """
        )
        
        # Also test non_intrinsic explicitly
        self.fs.create_file(
            "/fake/path/user_module.f90",
            contents="""\
    !!*
    ! User module that explicitly marks as non-intrinsic
    !*!
    module user_module
        !!* Explicitly import user constants as non-intrinsic *!
        use, non_intrinsic :: constants, only: pi
        implicit none
        
        real :: circle_constant = pi
    end module user_module
    """
        )
        
        result = extract_module_data([
            Path("/fake/path/constants.f90"),
            Path("/fake/path/precision_module.f90"),
            Path("/fake/path/user_module.f90")
        ])
        
        # Check precision module
        precision_module = next((m for m in result if m["module_name"] == "precision_module"), None)
        assert precision_module is not None
        
        # Check module-level intrinsic USE statements
        self.assertEqual(len(precision_module["uses"]), 2)
        
        # Check iso_fortran_env
        self.assertIn("iso_fortran_env", precision_module["uses"])
        iso_fortran_use = precision_module["uses"]["iso_fortran_env"]
        self.assertEqual(set(iso_fortran_use["selections"]), {"int32", "int64", "real32", "real64"})
        self.assertEqual(iso_fortran_use["use_type"], UseType.INTRINSIC)
        
        # Check iso_c_binding
        self.assertIn("iso_c_binding", precision_module["uses"])
        iso_c_use = precision_module["uses"]["iso_c_binding"]
        self.assertEqual(set(iso_c_use["selections"]), {"c_int", "c_double"})
        self.assertEqual(iso_c_use["use_type"], UseType.INTRINSIC)
        
        # Check function-level intrinsic USE
        get_info_func = precision_module["functions"]["get_compiler_info"]
        self.assertIn("iso_fortran_env", get_info_func["uses"])
        func_iso_use = get_info_func["uses"]["iso_fortran_env"]
        self.assertEqual(func_iso_use["selections"], ["compiler_version"])
        self.assertEqual(func_iso_use["use_type"], UseType.INTRINSIC)
        
        # Check non-intrinsic module
        user_module = next((m for m in result if m["module_name"] == "user_module"), None)
        assert user_module is not None
        
        self.assertIn("constants", user_module["uses"])
        constants_use = user_module["uses"]["constants"]
        self.assertEqual(constants_use["use_type"], UseType.NON_INTRINSIC)

    def test_use_type_merging(self):
        """Test merging of USE statements with different use types."""
        self.fs.create_file(
            "/fake/path/merge_test.f90",
            contents="""\
    !!*
    ! Module to test USE type merging
    ! Tests various combinations of intrinsic, non-intrinsic, and unspecified
    !*!
    module merge_test
        ! First: unspecified nature
        use iso_fortran_env, only: int32
        ! Second: explicit intrinsic (should set the module as intrinsic)
        use, intrinsic :: iso_fortran_env, only: int64
        ! Third: unspecified again (should remain intrinsic)
        use iso_fortran_env, only: real32
        
        ! For constants: first non-intrinsic, then unspecified
        use, non_intrinsic :: constants, only: pi
        use constants, only: e
        
        ! Test intrinsic + intrinsic merging
        use, intrinsic :: iso_c_binding, only: c_int
        use, intrinsic :: iso_c_binding, only: c_double
        
        implicit none
        
    contains
        !!*
        ! Function to test conflicting use types
        !*!
        function test_conflict() result(val)
            ! This would be an error in real Fortran, but we should handle it gracefully
            ! First mark as intrinsic
            use, intrinsic :: iso_fortran_env, only: compiler_version
            ! Then try to mark as non-intrinsic (conflict!)
            use, non_intrinsic :: iso_fortran_env, only: compiler_options
            
            real :: val
            val = 1.0
        end function
        
        !!*
        ! Subroutine to test NONE -> explicit type
        !*!
        subroutine test_none_to_explicit()
            ! Start with unspecified
            use constants, only: max_iterations
            ! Then explicit non-intrinsic
            use, non_intrinsic :: constants, only: pi
            
            print *, 'Testing merge'
        end subroutine
        
        !!*
        ! Subroutine to test non-intrinsic + non-intrinsic
        !*!
        subroutine test_non_intrinsic_merge()
            ! Multiple non-intrinsic uses of same module
            use, non_intrinsic :: constants, only: pi
            use, non_intrinsic :: constants, only: e
            use, non_intrinsic :: constants, only: max_iterations
            
            print *, 'Testing non-intrinsic merge'
        end subroutine
    end module merge_test
    """
        )
        
        result = extract_module_data([
            Path("/fake/path/constants.f90"),
            Path("/fake/path/merge_test.f90")
        ])
        
        merge_module = next((m for m in result if m["module_name"] == "merge_test"), None)
        assert merge_module is not None
        
        # Check module-level USE merging
        # iso_fortran_env: NONE -> INTRINSIC -> NONE should result in INTRINSIC
        self.assertIn("iso_fortran_env", merge_module["uses"])
        iso_use = merge_module["uses"]["iso_fortran_env"]
        self.assertEqual(iso_use["use_type"], UseType.INTRINSIC)
        self.assertEqual(set(iso_use["selections"]), {"int32", "int64", "real32"})
        
        # constants: NON_INTRINSIC -> NONE should remain NON_INTRINSIC
        self.assertIn("constants", merge_module["uses"])
        constants_use = merge_module["uses"]["constants"]
        self.assertEqual(constants_use["use_type"], UseType.NON_INTRINSIC)
        self.assertEqual(set(constants_use["selections"]), {"pi", "e"})
        
        # iso_c_binding: INTRINSIC + INTRINSIC should remain INTRINSIC
        self.assertIn("iso_c_binding", merge_module["uses"])
        iso_c_use = merge_module["uses"]["iso_c_binding"]
        self.assertEqual(iso_c_use["use_type"], UseType.INTRINSIC)
        self.assertEqual(set(iso_c_use["selections"]), {"c_int", "c_double"})
        
        # Check function with conflicting types
        test_func = merge_module["functions"]["test_conflict"]
        self.assertIn("iso_fortran_env", test_func["uses"])
        conflict_use = test_func["uses"]["iso_fortran_env"]
        # Should keep the first explicit type (INTRINSIC) and log a warning
        self.assertEqual(conflict_use["use_type"], UseType.INTRINSIC)
        self.assertEqual(set(conflict_use["selections"]), {"compiler_version", "compiler_options"})
        
        # Check subroutine NONE -> NON_INTRINSIC
        test_sub = merge_module["subroutines"]["test_none_to_explicit"]
        self.assertIn("constants", test_sub["uses"])
        sub_constants_use = test_sub["uses"]["constants"]
        self.assertEqual(sub_constants_use["use_type"], UseType.NON_INTRINSIC)
        self.assertEqual(set(sub_constants_use["selections"]), {"max_iterations", "pi"})
        
        # Check subroutine with NON_INTRINSIC + NON_INTRINSIC
        test_non_sub = merge_module["subroutines"]["test_non_intrinsic_merge"]
        self.assertIn("constants", test_non_sub["uses"])
        non_intrinsic_use = test_non_sub["uses"]["constants"]
        self.assertEqual(non_intrinsic_use["use_type"], UseType.NON_INTRINSIC)
        self.assertEqual(set(non_intrinsic_use["selections"]), {"pi", "e", "max_iterations"})

    def test_use_statement_with_renaming(self):
        """Test USE statements with renaming (=> syntax)."""
        self.fs.create_file(
            "/fake/path/renamed_uses.f90",
            contents="""\
    !!*
    ! Module demonstrating USE statement renaming
    ! Shows various renaming scenarios
    !*!
    module renamed_uses
        !!* Import with simple renaming *!
        use constants, only: my_pi => pi, my_e => e
        !!* Import with mix of renamed and non-renamed *!
        use iso_fortran_env, only: sp => real32, dp => real64, int32
        implicit none
        
        real :: circle_constant = my_pi
        real :: euler_constant = my_e
        
    contains
        !!*
        ! Function using renamed imports
        !*!
        function calculate_something(x) result(y)
            !!* Import with function-level renaming *!
            use constants, only: iterations => max_iterations
            !!* Multiple renames from same module *!
            use iso_c_binding, only: my_int => c_int, my_double => c_double, my_float => c_float
            
            real, intent(in) :: x
            real :: y
            integer :: i
            
            y = 0.0
            do i = 1, iterations
                y = y + x / real(i)
            end do
        end function
    end module renamed_uses
    """
        )
        
        result = extract_module_data([
            Path("/fake/path/constants.f90"),
            Path("/fake/path/renamed_uses.f90")
        ])
        
        renamed_module = next((m for m in result if m["module_name"] == "renamed_uses"), None)
        assert renamed_module is not None
        
        # Check module-level renames
        self.assertIn("constants", renamed_module["uses"])
        constants_use = renamed_module["uses"]["constants"]
        self.assertEqual(len(constants_use["renames"]), 2)
        # Check first rename
        pi_rename = next((r for r in constants_use["renames"] if r["local"] == "my_pi"), None)
        assert pi_rename is not None 
        self.assertEqual(pi_rename["original"], "pi")
        # Check second rename
        e_rename = next((r for r in constants_use["renames"] if r["local"] == "my_e"), None)
        assert e_rename is not None 
        self.assertEqual(e_rename["original"], "e")
        # Should have no regular selections when using renames
        self.assertEqual(constants_use["selections"], [])
        
        # Check mixed renamed and non-renamed
        self.assertIn("iso_fortran_env", renamed_module["uses"])
        iso_use = renamed_module["uses"]["iso_fortran_env"]
        self.assertEqual(len(iso_use["renames"]), 2)
        self.assertEqual(iso_use["selections"], ["int32"])  # Non-renamed import
        sp_rename = next((r for r in iso_use["renames"] if r["local"] == "sp"), None)
        assert sp_rename is not None 
        self.assertEqual(sp_rename["original"], "real32")
        
        # Check function-level renames
        calc_func = renamed_module["functions"]["calculate_something"]
        self.assertIn("constants", calc_func["uses"])
        func_constants_use = calc_func["uses"]["constants"]
        self.assertEqual(len(func_constants_use["renames"]), 1)
        iter_rename = func_constants_use["renames"][0]
        self.assertEqual(iter_rename["local"], "iterations")
        self.assertEqual(iter_rename["original"], "max_iterations")
        
        # Check multiple renames in function
        self.assertIn("iso_c_binding", calc_func["uses"])
        c_binding_use = calc_func["uses"]["iso_c_binding"]
        self.assertEqual(len(c_binding_use["renames"]), 3)
        rename_map = {r["local"]: r["original"] for r in c_binding_use["renames"]}
        self.assertEqual(rename_map["my_int"], "c_int")
        self.assertEqual(rename_map["my_double"], "c_double")
        self.assertEqual(rename_map["my_float"], "c_float")

    def test_use_statement_with_operators(self):
        """Test USE statements importing operators and assignment."""
        self.fs.create_file(
            "/fake/path/operator_module.f90",
            contents="""\
    !!*
    ! Module defining custom operators
    !*!
    module operator_module
        implicit none
        
        interface operator(.cross.)
            module procedure cross_product
        end interface
        
        interface operator(+)
            module procedure add_custom_type
        end interface
        
        interface assignment(=)
            module procedure assign_custom_type
        end interface
        
        type :: vector3d
            real :: x, y, z
        end type
        
    contains
        function cross_product(a, b) result(c)
            type(vector3d), intent(in) :: a, b
            type(vector3d) :: c
            c%x = a%y * b%z - a%z * b%y
            c%y = a%z * b%x - a%x * b%z
            c%z = a%x * b%y - a%y * b%x
        end function
        
        function add_custom_type(a, b) result(c)
            type(vector3d), intent(in) :: a, b
            type(vector3d) :: c
            c%x = a%x + b%x
            c%y = a%y + b%y
            c%z = a%z + b%z
        end function
        
        subroutine assign_custom_type(a, b)
            type(vector3d), intent(out) :: a
            type(vector3d), intent(in) :: b
            a%x = b%x
            a%y = b%y
            a%z = b%z
        end subroutine
    end module operator_module
    """
        )
        
        self.fs.create_file(
            "/fake/path/operator_user.f90",
            contents="""\
    !!*
    ! Module using custom operators
    !*!
    module operator_user
        !!* Import custom operators and type *!
        use operator_module, only: vector3d, operator(.cross.), operator(+), assignment(=)
        implicit none
        
    contains
        !!*
        ! Calculate cross product using imported operator
        !*!
        function use_operators(v1, v2) result(v3)
            type(vector3d), intent(in) :: v1, v2
            type(vector3d) :: v3, temp
            
            v3 = v1 .cross. v2
            temp = v1 + v2
        end function
    end module operator_user
    """
        )
        
        result = extract_module_data([
            Path("/fake/path/operator_module.f90"),
            Path("/fake/path/operator_user.f90")
        ])
        
        user_module = next((m for m in result if m["module_name"] == "operator_user"), None)
        assert user_module is not None
        
        # Check operator imports
        self.assertIn("operator_module", user_module["uses"])
        op_use = user_module["uses"]["operator_module"]
        # Operators should be in selections
        self.assertIn("operator(.cross.)", op_use["selections"])
        self.assertIn("operator(+)", op_use["selections"])
        self.assertIn("assignment(=)", op_use["selections"])
        self.assertIn("vector3d", op_use["selections"])

    def test_use_statement_in_interface_block(self):
        """Test USE statements within interface blocks."""
        self.fs.create_file(
            "/fake/path/interface_uses.f90",
            contents="""\
    !!*
    ! Module with USE statements in interface blocks
    !*!
    module interface_uses
        implicit none
        
        !!*
        ! Interface for external procedures that use modules
        !*!
        interface
            !!*
            ! External function using constants
            !*!
            function external_calc(x) result(y)
                !!* Import constants for external function interface *!
                use constants, only: pi
                real, intent(in) :: x
                real :: y
            end function
            
            !!*
            ! External subroutine using iso_c_binding
            !*!
            subroutine c_interface_sub(ptr, size)
                !!* Import C types for interface *!
                use, intrinsic :: iso_c_binding, only: c_ptr, c_int
                type(c_ptr), intent(in) :: ptr
                integer(c_int), intent(in) :: size
            end subroutine
        end interface
        
    contains
        !!*
        ! Use the external procedures
        !*!
        subroutine test_externals()
            real :: result
            result = external_calc(3.14)
        end subroutine
    end module interface_uses
    """
        )
        
        result = extract_module_data([
            Path("/fake/path/constants.f90"),
            Path("/fake/path/interface_uses.f90")
        ])
        
        interface_module = next((m for m in result if m["module_name"] == "interface_uses"), None)
        assert interface_module is not None
        
        # Check interface block
        self.assertEqual(len(interface_module["interfaces"]), 1)
        interface_block = interface_module["interfaces"][0]
                
        # Check uses in the interface procedures
        self.assertIn("external_calc", interface_block["procedures"])
        external_calc = interface_block["procedures"]["external_calc"]
        self.assertIn("uses", external_calc)
        self.assertIn("constants", external_calc["uses"])
        self.assertEqual(external_calc["uses"]["constants"]["selections"], ["pi"])
        self.assertEqual(external_calc["uses"]["constants"]["use_type"], UseType.NONE)
        
        self.assertIn("c_interface_sub", interface_block["procedures"])
        c_interface_sub = interface_block["procedures"]["c_interface_sub"]
        self.assertIn("uses", c_interface_sub)
        self.assertIn("iso_c_binding", c_interface_sub["uses"])
        self.assertEqual(set(c_interface_sub["uses"]["iso_c_binding"]["selections"]), {"c_ptr", "c_int"})
        self.assertEqual(c_interface_sub["uses"]["iso_c_binding"]["use_type"], UseType.INTRINSIC)

    def test_rename_conflicts_and_merging(self):
        """Test handling of rename conflicts when merging USE statements."""
        self.fs.create_file(
            "/fake/path/rename_conflicts.f90",
            contents="""\
    !!*
    ! Module to test rename conflict scenarios
    !*!
    module rename_conflicts
        ! First use with rename
        use constants, only: p => pi
        ! Second use with different rename for same item (conflict!)
        use constants, only: pie => pi
        ! Third use with rename of different item
        use constants, only: exp_const => e
        
        implicit none
        
    contains
        !!*
        ! Test merging renames in procedures
        !*!
        subroutine test_merge()
            ! Multiple uses with different renames
            use iso_fortran_env, only: i32 => int32
            use iso_fortran_env, only: i64 => int64
            use iso_fortran_env, only: int32  ! Also import without rename
            
            integer(i32) :: small_int
            integer(i64) :: big_int
            integer(int32) :: regular_int
        end subroutine
    end module rename_conflicts
    """
        )
        
        result = extract_module_data([
            Path("/fake/path/constants.f90"),
            Path("/fake/path/rename_conflicts.f90")
        ])
        
        conflict_module = next((m for m in result if m["module_name"] == "rename_conflicts"), None)
        assert conflict_module is not None
        
        # Check how conflicts are handled at module level
        self.assertIn("constants", conflict_module["uses"])
        constants_use = conflict_module["uses"]["constants"]
        
        # Should have all renames, even conflicting ones
        # Your merge function might need to handle this
        self.assertEqual(len(constants_use["renames"]), 3)
        rename_locals = [r["local"] for r in constants_use["renames"]]
        self.assertIn("p", rename_locals)
        self.assertIn("pie", rename_locals)
        self.assertIn("exp_const", rename_locals)
        
        # Check procedure level merging
        test_sub = conflict_module["subroutines"]["test_merge"]
        iso_use = test_sub["uses"]["iso_fortran_env"]
        
        # Should have both renames and the non-renamed import
        self.assertEqual(len(iso_use["renames"]), 2)
        self.assertEqual(iso_use["selections"], ["int32"])
        rename_map = {r["local"]: r["original"] for r in iso_use["renames"]}
        self.assertEqual(rename_map["i32"], "int32")
        self.assertEqual(rename_map["i64"], "int64")


if __name__ == '__main__':
    unittest.main()