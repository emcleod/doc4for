#TODO need to match procedure(a) :: dummy-argument 
# currently relying on position-based matching, which is fine as a failsafe
# where there isn't an explicit declaration like that.
# procedure_argument_parser.update_arguments_with_parsed_data()


# module optimization_mod
#     implicit none

#     ! First, define abstract interfaces
#     abstract interface
#         function objective_func(x)
#             real, intent(in) :: x
#             real :: objective_func
#         end function objective_func
        
#         function gradient_func(x)
#             real, intent(in) :: x
#             real :: gradient_func
#         end function gradient_func
#     end interface

#     ! The optimization interface
#     abstract interface
#         function minimize(obj, grad, x0) result(xmin)
#             import :: objective_func, gradient_func
#             implicit none
#             procedure(objective_func) :: obj
#             procedure(gradient_func) :: grad
#             real, intent(in) :: x0
#             real :: xmin
#         end function minimize
#     end interface

# end module optimization_mod




# module callback_breaking_example
#     implicit none

#     abstract interface
#         subroutine matrix_ops(transform1, transform2, data)
#             implicit none
            
#             ! First declare all procedure arguments
#             procedure(matrix_func) :: transform1, transform2
#             real, dimension(:,:), intent(inout) :: data
            
#             ! Now define interfaces, but in reverse order!
#             interface
#                 function matrix_func(x)
#                     real, dimension(:,:), intent(in) :: x
#                     real, dimension(size(x,1), size(x,2)) :: matrix_func
#                 end function matrix_func
#             end interface
            
#         end subroutine matrix_ops
#     end interface

# end module callback_breaking_example




# module mixed_interface_mod
#     implicit none

#     abstract interface
#         subroutine complex_processor(func1, data, func2, settings, func3)
#             implicit none
            
#             ! Declare procedure arguments first
#             procedure(scalar_func) :: func1
#             procedure(vector_func) :: func2
#             procedure(matrix_func) :: func3
            
#             real, dimension(:), intent(inout) :: data
#             integer, intent(in) :: settings
            
#             ! Interface blocks not in order of arguments
#             interface  ! This is for func3, not func1
#                 function matrix_func(x)
#                     real, dimension(:,:), intent(in) :: x
#                     real, dimension(size(x,1), size(x,2)) :: matrix_func
#                 end function matrix_func
#             end interface

#             interface  ! This is for func1, not func2
#                 function scalar_func(x)
#                     real, intent(in) :: x
#                     real :: scalar_func
#                 end function scalar_func
#             end interface
            
#             interface  ! This is for func2, not func3
#                 function vector_func(x)
#                     real, dimension(:), intent(in) :: x
#                     real, dimension(size(x)) :: vector_func
#                 end function vector_func
#             end interface
            
#         end subroutine complex_processor
#     end interface

# end module mixed_interface_mod



# module truly_breaking_example
#     implicit none

#     abstract interface
#         subroutine process_data(callback1, callback2, data)
#             implicit none
            
#             ! Declare the dummy arguments first
#             ! These are the formal parameters
#             real, dimension(:), intent(inout) :: data
            
#             ! Define interfaces in REVERSE order from how they appear in argument list
#             interface
#                 ! This is the interface for the 2nd argument (callback2)
#                 subroutine second_callback(x) 
#                     real, dimension(:), intent(inout) :: x
#                 end subroutine second_callback
#             end interface
            
#             interface 
#                 ! This is the interface for the 1st argument (callback1)
#                 subroutine first_callback(x)
#                     real, dimension(:), intent(inout) :: x
#                 end subroutine first_callback
#             end interface
            
#             ! Now declare which interface applies to which argument
#             procedure(first_callback) :: callback1
#             procedure(second_callback) :: callback2
            
#         end subroutine process_data
#     end interface
# end module truly_breaking_example