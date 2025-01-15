# def test_module_with_submodules(self):
#     # Parent module
#     self.fs.create_file("/fake/path/parent_module.f90",
#                         contents="""\
# module geometry
#     implicit none
    
#     ! Public types and variables
#     type :: point
#         real :: x, y
#     end type point
    
#     real, parameter :: PI = 3.14159265359
    
#     ! MODULE PROCEDURE declaration for submodule procedures
#     interface
#         module function distance(p1, p2) result(d)
#             type(point), intent(in) :: p1, p2
#             real :: d
#         end function distance
        
#         module subroutine print_point(p)
#             type(point), intent(in) :: p
#         end subroutine print_point
#     end interface

# end module geometry
# """)

#     # First submodule
#     self.fs.create_file("/fake/path/geometry_distance.f90",
#                         contents="""\
# submodule (geometry) geometry_distance
# contains
#     module function distance(p1, p2) result(d)
#         type(point), intent(in) :: p1, p2
#         real :: d
#         d = sqrt((p2%x - p1%x)**2 + (p2%y - p1%y)**2)
#     end function distance
# end submodule geometry_distance
# """)

#     # Second submodule
#     self.fs.create_file("/fake/path/geometry_io.f90",
#                         contents="""\
# submodule (geometry) geometry_io
# contains
#     module subroutine print_point(p)
#         type(point), intent(in) :: p
#         print *, 'Point at (', p%x, ',', p%y, ')'
#     end subroutine print_point
# end submodule geometry_io
# """)

#     result = extract_file_data([
#         Path("/fake/path/parent_module.f90"),
#         Path("/fake/path/geometry_distance.f90"),
#         Path("/fake/path/geometry_io.f90")
#     ])

#     # Verify parent module
#     self.assertEqual(len(result), 3)
#     parent_file = next(f for f in result if f["file_name"].endswith("parent_module.f90"))
#     self.assertIn("geometry", parent_file["modules"])
    
#     module_data = parent_file["modules"]["geometry"]
#     self.assertIn("point", module_data["types"])
#     self.assertIn("PI", module_data["variables"])
#     self.assertEqual(module_data["variables"]["PI"]["initial_value"], "3.14159265359")

#     # Check that the interface procedures are documented
#     self.assertIn("distance", module_data["functions"])
#     self.assertIn("print_point", module_data["subroutines"])

#     # Optionally verify submodule information if your system extracts it
#     submodule_files = [f for f in result if "geometry_" in f["file_name"]]
#     self.assertEqual(len(submodule_files), 2)



#     #TODO add a private type to a submodule to make sure we get the public/private 