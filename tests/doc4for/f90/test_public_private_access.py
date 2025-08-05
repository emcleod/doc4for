# Modules
# Nested modules 
# Derived Types
# Interface blocks (within modules) - generic, abstract
# Submodules 
# Block constructs 
# Sequence
# Protected
# Sequence
# bind(c)

import unittest
from pathlib import Path
from pyfakefs.fake_filesystem_unittest import TestCase
from doc4for.f90.generate_module_tree import extract_module_data

class TestTypes(TestCase):
    def setUp(self):
        self.setUpPyfakefs()

    def test_private_all(self):
        self.fs.create_file(
            "/fake/path/types.f90",
            contents="""\
module shapes
    implicit none
    private

    type :: shape
        real :: area
    end type shape

    type, extends(shape) :: rectangle
        real :: length
        real :: width
    end type rectangle

end module shapes
""",
        )
        result = extract_module_data([Path("/fake/path/types.f90")])
        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(module["module_name"], "shapes")
        types = module["types"]
        self.assertEqual(len(types), 2)
        shape = types["shape"]
        self.assertEqual(shape["type_name"], "shape")
        self.assertEqual(shape["attributes"], ["PUBLIC"])
        self.assertEqual(len(shape["data_components"]), 1)
        shape_area = shape["data_components"]["area"]
        self.assertEqual(shape_area["attributes"], [])
        self.assertEqual(shape_area["description"], "")
        self.assertIsNone(shape_area["dimension"])
        self.assertEqual(shape["description"], "")
        self.assertEqual(shape["generic_interfaces"], {})
        self.assertEqual(shape["procedures"], {})
        self.assertIsNone(shape["extends"])
        rectangle = types["rectangle"]
        self.assertEqual(rectangle["type_name"], "rectangle")
        self.assertEqual(rectangle["attributes"], ["public"])
        self.assertEqual(len(rectangle["data_components"]), 2)
        rectangle_width = rectangle["data_components"]["width"]
        self.assertEqual(rectangle_width["attributes"], [])
        self.assertEqual(rectangle_width["description"], "")
        self.assertIsNone(rectangle_width["dimension"])
        rectangle_length = rectangle["data_components"]["length"]
        self.assertEqual(rectangle_length["attributes"], [])
        self.assertEqual(rectangle_length["description"], "")
        self.assertIsNone(rectangle_length["dimension"])
        self.assertEqual(rectangle["description"], "")
        self.assertEqual(rectangle["generic_interfaces"], {})
        self.assertEqual(rectangle["procedures"], {})
        self.assertEqual(rectangle["extends"], "shape")

#     def test_private_all_2(self):
#         self.fs.create_file(
#             "/fake/path/types.f90",
#             contents="""\
# module shapes
#     implicit none

#     type, private :: Rectangle
#         real :: length 
#         real :: width
#     end type Rectangle

#     type, private :: Circle
#         real :: radius
#     end type Circle

# end module shapes
# """,
#         )
#         result = extract_module_data([Path("/fake/path/types.f90")])
#         self.assertEqual(len(result), 1)
#         module = result[0]
#         self.assertEqual(module["module_name"], "shapes")
#         types = module["types"]
#         self.assertEqual(len(types), 2)
#         shape = types["shape"]
#         self.assertEqual(shape["type_name"], "shape")
#         self.assertEqual(shape["attributes"], ["public"])
#         self.assertEqual(len(shape["data_components"]), 1)
#         shape_area = shape["data_components"]["area"]
#         self.assertEqual(shape_area["attributes"], [])
#         self.assertEqual(shape_area["description"], "")
#         self.assertIsNone(shape_area["dimension"])
#         self.assertEqual(shape["description"], "")
#         self.assertEqual(shape["generic_interfaces"], {})
#         self.assertEqual(shape["procedures"], {})
#         self.assertIsNone(shape["extends"])
#         rectangle = types["rectangle"]
#         self.assertEqual(rectangle["type_name"], "rectangle")
#         self.assertEqual(rectangle["attributes"], ["public"])
#         self.assertEqual(len(rectangle["data_components"]), 2)
#         rectangle_width = rectangle["data_components"]["width"]
#         self.assertEqual(rectangle_width["attributes"], [])
#         self.assertEqual(rectangle_width["description"], "")
#         self.assertIsNone(rectangle_width["dimension"])
#         rectangle_length = rectangle["data_components"]["length"]
#         self.assertEqual(rectangle_length["attributes"], [])
#         self.assertEqual(rectangle_length["description"], "")
#         self.assertIsNone(rectangle_length["dimension"])
#         self.assertEqual(rectangle["description"], "")
#         self.assertEqual(rectangle["generic_interfaces"], {})
#         self.assertEqual(rectangle["procedures"], {})
#         self.assertEqual(rectangle["extends"], "shape")

#     def test_public_all_1(self):
#         self.fs.create_file(
#             "/fake/path/types.f90",
#             contents="""\
# module shapes
#     implicit none
#     private

#     type :: shape
#         real :: area
#     end type shape

#     type, extends(shape) :: rectangle
#         real :: length
#         real :: width
#     end type rectangle

# end module shapes
# """,
#         )
#         result = extract_module_data([Path("/fake/path/types.f90")])
#         self.assertEqual(len(result), 1)
#         module = result[0]
#         self.assertEqual(module["module_name"], "shapes")
#         types = module["types"]
#         self.assertEqual(len(types), 2)
#         shape = types["shape"]
#         self.assertEqual(shape["type_name"], "shape")
#         self.assertEqual(shape["attributes"], ["public"])
#         self.assertEqual(len(shape["data_components"]), 1)
#         shape_area = shape["data_components"]["area"]
#         self.assertEqual(shape_area["attributes"], [])
#         self.assertEqual(shape_area["description"], "")
#         self.assertIsNone(shape_area["dimension"])
#         self.assertEqual(shape["description"], "")
#         self.assertEqual(shape["generic_interfaces"], {})
#         self.assertEqual(shape["procedures"], {})
#         self.assertIsNone(shape["extends"])
#         rectangle = types["rectangle"]
#         self.assertEqual(rectangle["type_name"], "rectangle")
#         self.assertEqual(rectangle["attributes"], ["public"])
#         self.assertEqual(len(rectangle["data_components"]), 2)
#         rectangle_width = rectangle["data_components"]["width"]
#         self.assertEqual(rectangle_width["attributes"], [])
#         self.assertEqual(rectangle_width["description"], "")
#         self.assertIsNone(rectangle_width["dimension"])
#         rectangle_length = rectangle["data_components"]["length"]
#         self.assertEqual(rectangle_length["attributes"], [])
#         self.assertEqual(rectangle_length["description"], "")
#         self.assertIsNone(rectangle_length["dimension"])
#         self.assertEqual(rectangle["description"], "")
#         self.assertEqual(rectangle["generic_interfaces"], {})
#         self.assertEqual(rectangle["procedures"], {})
#         self.assertEqual(rectangle["extends"], "shape")

#     def test_public_all_2(self):
#         self.fs.create_file(
#             "/fake/path/types.f90",
#             contents="""\
# module shapes
#     implicit none

#     type, private :: Rectangle
#         real :: length 
#         real :: width
#     end type Rectangle

#     type, private :: Circle
#         real :: radius
#     end type Circle

# end module shapes
# """,
#         )
#         result = extract_module_data([Path("/fake/path/types.f90")])
#         self.assertEqual(len(result), 1)
#         module = result[0]
#         self.assertEqual(module["module_name"], "shapes")
#         types = module["types"]
#         self.assertEqual(len(types), 2)
#         shape = types["shape"]
#         self.assertEqual(shape["type_name"], "shape")
#         self.assertEqual(shape["attributes"], ["public"])
#         self.assertEqual(len(shape["data_components"]), 1)
#         shape_area = shape["data_components"]["area"]
#         self.assertEqual(shape_area["attributes"], [])
#         self.assertEqual(shape_area["description"], "")
#         self.assertIsNone(shape_area["dimension"])
#         self.assertEqual(shape["description"], "")
#         self.assertEqual(shape["generic_interfaces"], {})
#         self.assertEqual(shape["procedures"], {})
#         self.assertIsNone(shape["extends"])
#         rectangle = types["rectangle"]
#         self.assertEqual(rectangle["type_name"], "rectangle")
#         self.assertEqual(rectangle["attributes"], ["public"])
#         self.assertEqual(len(rectangle["data_components"]), 2)
#         rectangle_width = rectangle["data_components"]["width"]
#         self.assertEqual(rectangle_width["attributes"], [])
#         self.assertEqual(rectangle_width["description"], "")
#         self.assertIsNone(rectangle_width["dimension"])
#         rectangle_length = rectangle["data_components"]["length"]
#         self.assertEqual(rectangle_length["attributes"], [])
#         self.assertEqual(rectangle_length["description"], "")
#         self.assertIsNone(rectangle_length["dimension"])
#         self.assertEqual(rectangle["description"], "")
#         self.assertEqual(rectangle["generic_interfaces"], {})
#         self.assertEqual(rectangle["procedures"], {})
#         self.assertEqual(rectangle["extends"], "shape")

# multiple on line
# module shapes
#    implicit none
#    private  ! Everything private by default
#    public :: shape, rectangle, square  ! This specific type is public

#    type :: shape
#        real :: area
#    end type shape

# default private specific public
# module shapes
#    implicit none
#    private  ! Everything private by default
#    public :: shape  ! This specific type is public

#    type :: shape
#        real :: area
#    end type shape

# default public specific private
# module shapes
#    implicit none
#    public   ! Everything public by default
#    private :: hiddentype  ! This specific type is private

#    type :: hiddentype
#        real :: secretdata
#    end type hiddentype

#    type :: shape
#        real :: area    ! This will be public
#    end type shape

# protected
# module shapes
#    implicit none

#    type :: shape
#        real, private :: hidden_area
#        real, protected :: semi_hidden_area   ! Can be accessed, but not modified outside
#        real, public :: visible_area
#    end type shape


# module complex_shapes
#     implicit none
#     private  ! Default: everything"s private

#     ! A public parameter
#     real, public, parameter :: PI = 3.14159

#     ! A private type
#     type :: hidden_utility
#         real :: data
#     end type

#     ! A public type with mixed-access components
#     type, public :: polygon
#         real, public    :: area
#         real, private   :: secret_calculation
#         real, protected :: perimeter  ! Can be read, not set
#         integer, public  :: sides
#     end type polygon

#     ! A private type that extends a public type
#     type, extends(polygon), private :: triangle
#         real :: base
#         real :: height
#     end type triangle

#     ! Explicit privacy/publicity for procedures
#     private :: calculate_area   ! Keep this algorithm hidden
#     public  :: make_square     ! This.is.your.API.

# contains
#     function calculate_area(length, width) result(area)
#         real, intent(in) :: length, width
#         real :: area
#         area = length * width
#     end function

#     function make_square(side_length) result(new_square)
#         real, intent(in) :: side_length
#         type(polygon) :: new_square
#         new_square%sides = 4
#         new_square%area  = calculate_area(side_length, side_length)
#         new_square%perimeter = 4 * side_length
#     end function make_square

# end module complex_shapes

# module complex_shapes
#     implicit none
#     private  ! Default everything to private

#     ! Public types
#     public :: base_shape, rectangle, publicly_accessible_circle

#     ! Public procedures
#     public :: calculate_area, get_shape_name

#     ! Protected variable
#     real, protected :: pi = 3.14159

#     ! Private variable
#     real, private :: secret_number = 42.0

#     ! Base type
#     type :: base_shape
#         private  ! Default components to private
#         character(len=20) :: name
#         real, public :: area  ! Explicitly public component
#     contains
#         procedure, public :: get_name => get_shape_name
#     end type base_shape

#     ! Derived type, public
#     type, extends(base_shape) :: rectangle
#         real, private :: length
#         real, private :: width
#     contains
#         procedure, public :: set_dimensions
#         procedure, public :: calc_area => rectangle_area
#     end type rectangle

#     ! Another derived type, public
#     type, extends(base_shape), public :: publicly_accessible_circle
#         real :: radius
#     contains
#         procedure :: calc_area => circle_area
#     end type publicly_accessible_circle

#     ! Private type
#     type, private :: hidden_shape
#         real :: secret_area
#     end type hidden_shape

# contains
#     ! Public function
#     function calculate_area(shape) result(area)
#         class(base_shape), intent(in) :: shape
#         real :: area
#         area = shape%area
#     end function calculate_area

#     ! Private function
#     function get_shape_name(this) result(name)
#         class(base_shape), intent(in) :: this
#         character(len=20) :: name
#         name = this%name
#     end function get_shape_name

#     ! Public subroutine for rectangle
#     subroutine set_dimensions(this, length, width)
#         class(rectangle), intent(inout) :: this
#         real, intent(in) :: length, width
#         this%length = length
#         this%width = width
#     end subroutine set_dimensions

#     ! Private function for rectangle
#     function rectangle_area(this) result(area)
#         class(rectangle), intent(in) :: this
#         real :: area
#         area = this%length * this%width
#     end function rectangle_area

#     ! Private function for circle
#     function circle_area(this) result(area)
#         class(publicly_accessible_circle), intent(in) :: this
#         real :: area
#         area = pi * this%radius**2
#     end function circle_area

# end module complex_shapes

#procedure-is-public-but-its-type-is-private
#    type, private :: hidden_thing
#       integer :: data
#    end type
   
#    interface    
#       function public_maker() result(h)
#          type(hidden_thing) :: h
#       end function
#    end interface

#    type, public :: base
#       integer, public :: x
#    end type

#    type, private :: derived
#       type(base) :: b
#    end type

# module edge_case
#    implicit none
#    private

#    type :: hidden
#       integer :: value
#    end type

#    public :: get_hidden   ! This procedure will be public

# contains
#    function get_hidden() result(h)  ! This returns a hidden type!
#       type(hidden) :: h
#       h%value = 42
#    end function
# 1) Could have a public function that returns TYPE(derived)
# 2) Could have a public variable of TYPE(derived)
# In this case, get_hidden() is public, but it returns a private type. A user of the module could:
# Call get_hidden()   allowed
# Print the result    not allowed 
# Access any methods  not allowed 

    def test_types_default_public_access(self):
        self.fs.create_file(
            "/fake/path/complex_access.f90",
            contents="""\
    module complex_access_mod
        implicit none
        
        ! Module starts with default public
        
        type :: tricky_access_type
            ! Components with changing defaults
            integer :: default_public_1
            private  ! Switch to private
            real :: now_private_1
            real :: now_private_2
            public  ! Switch back to public
            integer :: now_public_1
            character(len=10) :: now_public_2
            integer, private :: explicit_private  ! Overrides current default
            private  ! Switch to private again
            logical :: private_again
            
        contains
            ! Type-bound procedures with changing defaults
            procedure :: default_access_proc  ! Should be public (Fortran default)
            private  ! Switch default to private
            procedure :: private_by_default_1
            procedure :: private_by_default_2
            procedure, public :: explicit_public_1  ! Overrides default
            public  ! Switch default to public
            procedure :: public_by_default_1
            procedure :: public_by_default_2
            procedure, private :: explicit_private_proc  ! Overrides default
            private  ! Switch back to private
            procedure :: private_final
            generic :: assignment(=) => private_final  ! Private generic
            public  ! Switch to public
            generic, private :: write(formatted) => private_final  ! Explicit private overrides
        end type tricky_access_type
        
        ! Type that starts with private components
        type :: private_first_type
            private  ! Start with private
            real :: secret_data
            integer :: hidden_value
            public  ! Switch to public
            real :: visible_data
            procedure(some_interface), pointer :: public_proc_ptr => null()
            private  ! Back to private
            procedure(some_interface), pointer :: private_proc_ptr => null()
        contains
            ! No initial statement, so default is public
            procedure :: initially_public
            private  ! Now private
            procedure :: becomes_private
        end type private_first_type

        abstract interface
            subroutine some_interface()
            end subroutine
        end interface

    contains
        
        ! Implementation stubs
        subroutine default_access_proc(this)
            class(tricky_access_type) :: this
        end subroutine
        
        subroutine private_by_default_1(this)
            class(tricky_access_type) :: this
        end subroutine
        
        subroutine private_by_default_2(this)  
            class(tricky_access_type) :: this
        end subroutine
        
        subroutine explicit_public_1(this)
            class(tricky_access_type) :: this
        end subroutine
        
        subroutine public_by_default_1(this)
            class(tricky_access_type) :: this
        end subroutine
        
        subroutine public_by_default_2(this)
            class(tricky_access_type) :: this
        end subroutine
        
        subroutine explicit_private_proc(this)
            class(tricky_access_type) :: this
        end subroutine
        
        subroutine private_final(this)
            class(tricky_access_type) :: this
        end subroutine
        
        subroutine initially_public(this)
            class(private_first_type) :: this
        end subroutine
        
        subroutine becomes_private(this)
            class(private_first_type) :: this
        end subroutine

    end module complex_access_mod
    """
        )
        
        result = extract_module_data([Path("/fake/path/complex_access.f90")], False)
        module = result[0]
        
        # Test tricky_access_type
        tricky_type = module["types"]["tricky_access_type"]
        
        # Check data components
        # default_public_1 should be public (no access statement yet)
        self.assertNotIn("PRIVATE", tricky_type["data_components"]["default_public_1"]["attributes"])
        # Could check for PUBLIC if your code adds it for clarity
        
        # After first "private" statement
        self.assertIn("PRIVATE", tricky_type["data_components"]["now_private_1"]["attributes"])
        self.assertIn("PRIVATE", tricky_type["data_components"]["now_private_2"]["attributes"])
        
        # After "public" statement  
        self.assertIn("PUBLIC", tricky_type["data_components"]["now_public_1"]["attributes"])
        self.assertIn("PUBLIC", tricky_type["data_components"]["now_public_2"]["attributes"])
        
        # Explicit overrides current default
        self.assertIn("PRIVATE", tricky_type["data_components"]["explicit_private"]["attributes"])
        
        # After second "private" statement
        self.assertIn("PRIVATE", tricky_type["data_components"]["private_again"]["attributes"])
        
        # Check procedures
        # Default is public for procedures
        self.assertNotIn("PRIVATE", tricky_type["procedures"]["default_access_proc"]["attributes"])
        
        # After "private" in contains section
        self.assertIn("PRIVATE", tricky_type["procedures"]["private_by_default_1"]["attributes"])
        self.assertIn("PRIVATE", tricky_type["procedures"]["private_by_default_2"]["attributes"])
        
        # Explicit public overrides
        self.assertIn("PUBLIC", tricky_type["procedures"]["explicit_public_1"]["attributes"])
        
        # After "public" statement
        self.assertIn("PUBLIC", tricky_type["procedures"]["public_by_default_1"]["attributes"])
        self.assertIn("PUBLIC", tricky_type["procedures"]["public_by_default_2"]["attributes"])
        
        # Explicit private overrides
        self.assertIn("PRIVATE", tricky_type["procedures"]["explicit_private_proc"]["attributes"])
        
        # After final "private"
        self.assertIn("PRIVATE", tricky_type["procedures"]["private_final"]["attributes"])
        
        # Generic interfaces
        self.assertIn("PRIVATE", tricky_type["generic_interfaces"]["assignment(=)"]["attributes"])
        self.assertIn("PRIVATE", tricky_type["generic_interfaces"]["write(formatted)"]["attributes"])
        
        # Test private_first_type
        private_type = module["types"]["private_first_type"]
        
        # Components after initial "private"
        self.assertIn("PRIVATE", private_type["data_components"]["secret_data"]["attributes"])
        self.assertIn("PRIVATE", private_type["data_components"]["hidden_value"]["attributes"])
        
        # After "public"
        self.assertIn("PUBLIC", private_type["data_components"]["visible_data"]["attributes"])
        self.assertIn("PUBLIC", private_type["procedures"]["public_proc_ptr"]["attributes"])
        
        # After second "private"
        self.assertIn("PRIVATE", private_type["procedures"]["private_proc_ptr"]["attributes"])
        
        # Procedures - default is public when no initial statement
        self.assertNotIn("PRIVATE", private_type["procedures"]["initially_public"]["attributes"])
        
        # After "private" in contains
        self.assertIn("PRIVATE", private_type["procedures"]["becomes_private"]["attributes"])

    def test_private_types_from_module_level(self):
        self.fs.create_file(
            "/fake/path/tricky_access.f90",
            contents="""\
    module tricky_access_mod
        implicit none
        private  ! Module default is private

        type :: access_chaos_type
            ! No initial access statement - inherits module default (private)
            integer :: default_private_1
            
            public  ! Switch to public
            real :: public_real
            integer :: public_int
            
            private  ! Back to private
            complex :: private_complex
            
            ! Explicit access overrides current default
            real, public :: explicit_public_in_private_section
            
            public  ! Switch to public again
            character(len=10) :: public_string
            
            ! Multiple access changes
            private
            logical :: private_flag
            public
            double precision :: public_double

        contains
            ! No initial statement - what"s the default here?
            procedure :: mystery_access_1
            
            private  ! Set to private
            procedure :: private_proc_1
            procedure :: private_proc_2
            
            public  ! Switch to public
            procedure :: public_proc_1
            procedure, private :: explicit_private_in_public
            procedure :: public_proc_2
            
            ! Another switch
            private
            procedure :: private_proc_3
            
            ! Generic with current default
            generic :: assignment(=) => private_proc_3
            
            public
            ! Public generic with private specific
            generic :: write(formatted) => write_impl
            procedure, private :: write_impl
        end type access_chaos_type

        type, public :: mixed_type  ! Type is public but has module-default components
            ! No access statement - uses module default (private)
            integer :: should_be_private
            
            public
            real :: should_be_public
            
        contains
            ! No access statement - what default?
            procedure :: unclear_access
            
            private
            procedure :: definitely_private
        end type mixed_type

    contains
        ! Implementation stubs
        subroutine mystery_access_1(this)
            class(access_chaos_type), intent(in) :: this
        end subroutine
        
        subroutine private_proc_1(this)
            class(access_chaos_type), intent(in) :: this
        end subroutine
        
        subroutine private_proc_2(this)
            class(access_chaos_type), intent(in) :: this
        end subroutine
        
        subroutine public_proc_1(this)
            class(access_chaos_type), intent(in) :: this
        end subroutine
        
        subroutine explicit_private_in_public(this)
            class(access_chaos_type), intent(in) :: this
        end subroutine
        
        subroutine public_proc_2(this)
            class(access_chaos_type), intent(in) :: this
        end subroutine
        
        subroutine private_proc_3(this, other)
            class(access_chaos_type), intent(out) :: this
            class(access_chaos_type), intent(in) :: other
        end subroutine
        
        subroutine write_impl(dtv, unit, iotype, v_list, iostat, iomsg)
            class(access_chaos_type), intent(in) :: dtv
            integer, intent(in) :: unit
            character(*), intent(in) :: iotype
            integer, intent(in) :: v_list(:)
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
        
        subroutine unclear_access(this)
            class(mixed_type), intent(in) :: this
        end subroutine
        
        subroutine definitely_private(this)
            class(mixed_type), intent(in) :: this
        end subroutine

    end module tricky_access_mod
    """
        )
        
        result = extract_module_data([Path("/fake/path/tricky_access.f90")], False)
        module = result[0]
        
        # Check module-level default
        self.assertIn("PRIVATE", module["attributes"])
        
        # Test access_chaos_type
        chaos_type = module["types"]["access_chaos_type"]
        
        # Components - checking actual access based on the active default when declared
        self.assertIn("PRIVATE", chaos_type["data_components"]["default_private_1"]["attributes"])
        self.assertIn("PUBLIC", chaos_type["data_components"]["public_real"]["attributes"])
        self.assertIn("PUBLIC", chaos_type["data_components"]["public_int"]["attributes"])
        self.assertIn("PRIVATE", chaos_type["data_components"]["private_complex"]["attributes"])
        self.assertIn("PUBLIC", chaos_type["data_components"]["explicit_public_in_private_section"]["attributes"])
        self.assertIn("PUBLIC", chaos_type["data_components"]["public_string"]["attributes"])
        self.assertIn("PRIVATE", chaos_type["data_components"]["private_flag"]["attributes"])
        self.assertIn("PUBLIC", chaos_type["data_components"]["public_double"]["attributes"])
        
        # Procedures - the tricky part!
        # mystery_access_1: No access statement in contains section - what"s the default?
        # This is the ambiguous case - it might inherit from component section (public) 
        # or reset to module default (private) or Fortran default (public)
        self.assertIn("PRIVATE", chaos_type["procedures"]["mystery_access_1"]["attributes"])
        
        self.assertIn("PRIVATE", chaos_type["procedures"]["private_proc_1"]["attributes"])
        self.assertIn("PRIVATE", chaos_type["procedures"]["private_proc_2"]["attributes"])
        self.assertIn("PUBLIC", chaos_type["procedures"]["public_proc_1"]["attributes"])
        self.assertIn("PRIVATE", chaos_type["procedures"]["explicit_private_in_public"]["attributes"])
        self.assertIn("PUBLIC", chaos_type["procedures"]["public_proc_2"]["attributes"])
        self.assertIn("PRIVATE", chaos_type["procedures"]["private_proc_3"]["attributes"])
        
        # Generic interfaces
        assignment_generic = chaos_type["generic_interfaces"]["assignment(=)"]
        self.assertIn("PRIVATE", assignment_generic["attributes"])
        
        write_generic = chaos_type["generic_interfaces"]["write(formatted)"]
        self.assertIn("PUBLIC", write_generic["attributes"])
        
        # write_impl should be private
        self.assertIn("PRIVATE", chaos_type["procedures"]["write_impl"]["attributes"])
        
        # Test mixed_type
        mixed_type = module["types"]["mixed_type"]
        self.assertIn("PUBLIC", mixed_type["attributes"])  # Type itself is public
        
        # Components inherit module default since no access statement in type
        self.assertIn("PRIVATE", mixed_type["data_components"]["should_be_private"]["attributes"])
        self.assertIn("PUBLIC", mixed_type["data_components"]["should_be_public"]["attributes"])
        
        # Procedures
        self.assertIn("PRIVATE", mixed_type["procedures"]["unclear_access"]["attributes"])
        self.assertIn("PRIVATE", mixed_type["procedures"]["definitely_private"]["attributes"])
                

#TODO
# Module-Level Access Control
# - Default module access (no statements)

# Everything should be PUBLIC by default
# - Module with initial private statement

# All subsequent declarations should be PRIVATE
# - Test variables, types, interfaces, procedures
# - Module with initial public statement

# Redundant but valid - everything remains PUBLIC
# - Module with alternating access statements

# private → declare items → public → declare items → private → etc.
# - Each item should have the access from the most recent statement
# - Explicit access overrides

# Items with explicit public/private in a different default section
# - E.g., integer, public :: x when default is PRIVATE
# - Access statements with specific names

# private :: specific_var, specific_type
# - These named items get that access, others keep current default
# - Mixed named and default access statements

# private (changes default) followed by public :: x, y (only affects x, y)
# - Type-Level Access Control
# - Type with no access statements

# Components inherit module"s current default access
# - Procedures default to PUBLIC
# - Type with private components statement

# All components become PRIVATE (unless explicitly marked)
# - Procedures still default to PUBLIC
# - Type with explicit component access

# integer, public :: x when type has private statement
# real, private :: y when no private statement
# Procedure pointer components

# With explicit access specifiers
# Without access (inherit from component default)
# Type-bound procedures access

# Default (PUBLIC)
# Explicit procedure, private :: method
# Explicit procedure, public :: method
# Generic interfaces in types

# generic, public :: write(formatted) => ...
# generic, private :: operator(+) => ...
# Without explicit access
# Final procedures

# Should not have explicit access (always private-ish)
# Edge Cases
# Module contains section

# Procedures after contains can"t have bare access statements
# Must use private function f() form
# They inherit last module-level default
# Nested implications

# Private type with public components
# Public type in private module section
# Private generic with public specific procedures
# Access and USE association

# private module entities and what"s visible outside
# Types that are private but have public components
# Invalid Fortran that parser might accept

# Multiple private statements in type (only one allowed)
# public statement in type components (not allowed)
# Bare access statements after type contains
# Comments and spacing

# Access statements with comments
# Blank lines between access changes and declarations
# Old-style vs modern Fortran

# Access control with common blocks
# Access control with equivalence
# Module procedures vs contains procedures
if __name__ == "__main__":
    unittest.main()

# module factory_pattern
#    private
#    type :: implementation
#       integer :: secret_data
#    end type
   
#    type, public :: handle
#       type(implementation), pointer, private :: impl => null()
#    end type
   
#    public :: create_handle, use_handle
   
# contains
#    function create_handle() result(h)
#       type(handle) :: h
#       allocate(h%impl)
#       h%impl%secret_data = 42
#    end function
   
#    subroutine use_handle(h)
#       type(handle), intent(in) :: h
#       ! Can access h%impl here, but users can't
#    end subroutine
# end module