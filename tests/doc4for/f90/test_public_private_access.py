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

    def test_private_all_1(self):
        self.fs.create_file(
            '/fake/path/types.f90',
            contents='''\
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
''',
        )
        result = extract_module_data([Path('/fake/path/types.f90')])
        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(module['module_name'], 'shapes')
        types = module['types']
        self.assertEqual(len(types), 2)
        shape = types['shape']
        self.assertEqual(shape['type_name'], 'shape')
        self.assertEqual(shape['attributes'], ['public'])
        self.assertEqual(len(shape['data_components']), 1)
        shape_area = shape['data_components']['area']
        self.assertEqual(shape_area['attributes'], [])
        self.assertEqual(shape_area['description'], '')
        self.assertIsNone(shape_area['dimension'])
        self.assertEqual(shape['description'], '')
        self.assertEqual(shape['generic_interfaces'], {})
        self.assertEqual(shape['procedures'], {})
        self.assertIsNone(shape['extends'])
        rectangle = types['rectangle']
        self.assertEqual(rectangle['type_name'], 'rectangle')
        self.assertEqual(rectangle['attributes'], ['public'])
        self.assertEqual(len(rectangle['data_components']), 2)
        rectangle_width = rectangle['data_components']['width']
        self.assertEqual(rectangle_width['attributes'], [])
        self.assertEqual(rectangle_width['description'], '')
        self.assertIsNone(rectangle_width['dimension'])
        rectangle_length = rectangle['data_components']['length']
        self.assertEqual(rectangle_length['attributes'], [])
        self.assertEqual(rectangle_length['description'], '')
        self.assertIsNone(rectangle_length['dimension'])
        self.assertEqual(rectangle['description'], '')
        self.assertEqual(rectangle['generic_interfaces'], {})
        self.assertEqual(rectangle['procedures'], {})
        self.assertEqual(rectangle['extends'], 'shape')

#     def test_private_all_2(self):
#         self.fs.create_file(
#             '/fake/path/types.f90',
#             contents='''\
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
# ''',
#         )
#         result = extract_module_data([Path('/fake/path/types.f90')])
#         self.assertEqual(len(result), 1)
#         module = result[0]
#         self.assertEqual(module['module_name'], 'shapes')
#         types = module['types']
#         self.assertEqual(len(types), 2)
#         shape = types['shape']
#         self.assertEqual(shape['type_name'], 'shape')
#         self.assertEqual(shape['attributes'], ['public'])
#         self.assertEqual(len(shape['data_components']), 1)
#         shape_area = shape['data_components']['area']
#         self.assertEqual(shape_area['attributes'], [])
#         self.assertEqual(shape_area['description'], '')
#         self.assertIsNone(shape_area['dimension'])
#         self.assertEqual(shape['description'], '')
#         self.assertEqual(shape['generic_interfaces'], {})
#         self.assertEqual(shape['procedures'], {})
#         self.assertIsNone(shape['extends'])
#         rectangle = types['rectangle']
#         self.assertEqual(rectangle['type_name'], 'rectangle')
#         self.assertEqual(rectangle['attributes'], ['public'])
#         self.assertEqual(len(rectangle['data_components']), 2)
#         rectangle_width = rectangle['data_components']['width']
#         self.assertEqual(rectangle_width['attributes'], [])
#         self.assertEqual(rectangle_width['description'], '')
#         self.assertIsNone(rectangle_width['dimension'])
#         rectangle_length = rectangle['data_components']['length']
#         self.assertEqual(rectangle_length['attributes'], [])
#         self.assertEqual(rectangle_length['description'], '')
#         self.assertIsNone(rectangle_length['dimension'])
#         self.assertEqual(rectangle['description'], '')
#         self.assertEqual(rectangle['generic_interfaces'], {})
#         self.assertEqual(rectangle['procedures'], {})
#         self.assertEqual(rectangle['extends'], 'shape')

#     def test_public_all_1(self):
#         self.fs.create_file(
#             '/fake/path/types.f90',
#             contents='''\
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
# ''',
#         )
#         result = extract_module_data([Path('/fake/path/types.f90')])
#         self.assertEqual(len(result), 1)
#         module = result[0]
#         self.assertEqual(module['module_name'], 'shapes')
#         types = module['types']
#         self.assertEqual(len(types), 2)
#         shape = types['shape']
#         self.assertEqual(shape['type_name'], 'shape')
#         self.assertEqual(shape['attributes'], ['public'])
#         self.assertEqual(len(shape['data_components']), 1)
#         shape_area = shape['data_components']['area']
#         self.assertEqual(shape_area['attributes'], [])
#         self.assertEqual(shape_area['description'], '')
#         self.assertIsNone(shape_area['dimension'])
#         self.assertEqual(shape['description'], '')
#         self.assertEqual(shape['generic_interfaces'], {})
#         self.assertEqual(shape['procedures'], {})
#         self.assertIsNone(shape['extends'])
#         rectangle = types['rectangle']
#         self.assertEqual(rectangle['type_name'], 'rectangle')
#         self.assertEqual(rectangle['attributes'], ['public'])
#         self.assertEqual(len(rectangle['data_components']), 2)
#         rectangle_width = rectangle['data_components']['width']
#         self.assertEqual(rectangle_width['attributes'], [])
#         self.assertEqual(rectangle_width['description'], '')
#         self.assertIsNone(rectangle_width['dimension'])
#         rectangle_length = rectangle['data_components']['length']
#         self.assertEqual(rectangle_length['attributes'], [])
#         self.assertEqual(rectangle_length['description'], '')
#         self.assertIsNone(rectangle_length['dimension'])
#         self.assertEqual(rectangle['description'], '')
#         self.assertEqual(rectangle['generic_interfaces'], {})
#         self.assertEqual(rectangle['procedures'], {})
#         self.assertEqual(rectangle['extends'], 'shape')

#     def test_public_all_2(self):
#         self.fs.create_file(
#             '/fake/path/types.f90',
#             contents='''\
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
# ''',
#         )
#         result = extract_module_data([Path('/fake/path/types.f90')])
#         self.assertEqual(len(result), 1)
#         module = result[0]
#         self.assertEqual(module['module_name'], 'shapes')
#         types = module['types']
#         self.assertEqual(len(types), 2)
#         shape = types['shape']
#         self.assertEqual(shape['type_name'], 'shape')
#         self.assertEqual(shape['attributes'], ['public'])
#         self.assertEqual(len(shape['data_components']), 1)
#         shape_area = shape['data_components']['area']
#         self.assertEqual(shape_area['attributes'], [])
#         self.assertEqual(shape_area['description'], '')
#         self.assertIsNone(shape_area['dimension'])
#         self.assertEqual(shape['description'], '')
#         self.assertEqual(shape['generic_interfaces'], {})
#         self.assertEqual(shape['procedures'], {})
#         self.assertIsNone(shape['extends'])
#         rectangle = types['rectangle']
#         self.assertEqual(rectangle['type_name'], 'rectangle')
#         self.assertEqual(rectangle['attributes'], ['public'])
#         self.assertEqual(len(rectangle['data_components']), 2)
#         rectangle_width = rectangle['data_components']['width']
#         self.assertEqual(rectangle_width['attributes'], [])
#         self.assertEqual(rectangle_width['description'], '')
#         self.assertIsNone(rectangle_width['dimension'])
#         rectangle_length = rectangle['data_components']['length']
#         self.assertEqual(rectangle_length['attributes'], [])
#         self.assertEqual(rectangle_length['description'], '')
#         self.assertIsNone(rectangle_length['dimension'])
#         self.assertEqual(rectangle['description'], '')
#         self.assertEqual(rectangle['generic_interfaces'], {})
#         self.assertEqual(rectangle['procedures'], {})
#         self.assertEqual(rectangle['extends'], 'shape')

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
#     private  ! Default: everything's private

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
# • Call get_hidden()   allowed
# • Print the result    not allowed 
# • Access any methods  not allowed 
# 1) Report derived%b%x as private (100% safe, but loses information)
# 2) Report derived%b%x as public  (100% correct, but requires module-level analysis)
# 3) Report derived%b%x as "conditionally public" (most correct, most complex) 


if __name__ == '__main__':
    unittest.main()

