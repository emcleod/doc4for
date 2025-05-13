#TODO get test from test_arrays

# module coarray_procedures
#     implicit none
# contains
#     ! Coarray as dummy argument
#     subroutine process_coarray(data)
#         real, codimension[*], intent(inout) :: data
#         ! Process the coarray
#         data = data + 1.0
#     end subroutine
    
#     ! Array coarray as argument
#     subroutine process_array_coarray(matrix)
#         real, dimension(:,:), codimension[*], intent(in) :: matrix
#         ! Process the array coarray
#     end subroutine
    
#     ! 2D coarray grid as argument
#     subroutine exchange_boundaries(grid)
#         real, codimension[3,*], intent(inout) :: grid
#         ! Exchange boundary data between neighbors
#     end subroutine
    
#     ! Function returning coarray data
#     real function get_neighbor_value(data, image_num)
#         real, codimension[*], intent(in) :: data
#         integer, intent(in) :: image_num
#         get_neighbor_value = data[image_num]
#     end function
# end module