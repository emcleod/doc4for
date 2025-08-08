import unittest
from pyfakefs.fake_filesystem_unittest import TestCase

class TestProgramUses(TestCase):
    def setUp(self):
        self.setUpPyfakefs()

    def test_procedure_array_arguments(self):
        """Test arrays used as procedure arguments with special features."""
        self.fs.create_file(
            "/fake/path/proc_arrays.f90",
            contents="""
    module proc_arrays
        implicit none
        
        interface
            ! Assumed-size array
            subroutine process_assumed_size(arr)
                real :: arr(*)
            end subroutine
            
            ! Multi-dim assumed-size
            subroutine process_multi_dim(matrix)
                real :: matrix(10,*)
            end subroutine
            
            ! Assumed-shape array
            subroutine process_assumed_shape(arr)
                real :: arr(:)
            end subroutine
            
            ! Assumed-rank array (F2018)
            subroutine process_assumed_rank(arr)
                real :: arr(..)
            end subroutine
            
            ! Pointer array
            subroutine process_pointer(arr)
                real, pointer :: arr(:)
            end subroutine
            
            ! Allocatable array
            subroutine modify_allocatable(arr)
                real, allocatable :: arr(:)
            end subroutine
        end interface
        
    contains
        
        ! Implementation of these subroutines could include:
        subroutine array_sections()
            real :: full_array(100)
            real :: section1(10)
            real :: section2(10)
            
            ! Stride usage
            section1 = full_array(1:20:2)
            
            ! Negative stride
            section2 = full_array(20:1:-2)
        end subroutine
        
        ! Zero-sized array demonstration
        subroutine zero_sized()
            real :: empty(10:1)
            integer :: size_of_empty
            
            size_of_empty = size(empty)  ! Will be 0
        end subroutine
        
    end module proc_arrays
            """
        )
        
if __name__ == "__main__":
    unittest.main()