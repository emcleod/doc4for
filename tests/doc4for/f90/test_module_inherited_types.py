import unittest
from pathlib import Path
from pyfakefs.fake_filesystem_unittest import TestCase
from doc4for.f90.generate_module_tree import extract_module_data

class TestTypes(TestCase):
    def setUp(self):
        self.setUpPyfakefs()

    def test_single_derived_type_no_comments(self):
        self.fs.create_file(
            '/fake/path/types.f90',
            contents='''\
module shapes
    implicit none
    private
    public :: shape, rectangle

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

    def test_single_derived_type_with_comments(self):
        self.fs.create_file(
            '/fake/path/types.f90',
            contents='''\
module shapes
    implicit none
    private
    public :: shape, rectangle

    !!* Base type for shapes *!
    type :: shape
        !!* The area of a shape *!
        real :: area
    end type shape

    !!*
    ! A rectangle 
    !*!
    type, extends(shape) :: rectangle
        !!* The length of the rectangle *!
        real :: length
        !!* The width of the rectangle *!
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
        self.assertEqual(shape_area['description'], 'The area of a shape\n')
        self.assertIsNone(shape_area['dimension'])
        self.assertEqual(shape['description'], 'Base type for shapes\n')
        self.assertEqual(shape['generic_interfaces'], {})
        self.assertEqual(shape['procedures'], {})
        self.assertIsNone(shape['extends'])
        rectangle = types['rectangle']
        self.assertEqual(rectangle['type_name'], 'rectangle')
        self.assertEqual(rectangle['attributes'], ['public'])
        self.assertEqual(len(rectangle['data_components']), 2)
        rectangle_width = rectangle['data_components']['width']
        self.assertEqual(rectangle_width['attributes'], [])
        self.assertEqual(rectangle_width['description'], 'The width of the rectangle\n')
        self.assertIsNone(rectangle_width['dimension'])
        rectangle_length = rectangle['data_components']['length']
        self.assertEqual(rectangle_length['attributes'], [])
        self.assertEqual(rectangle_length['description'], 'The length of the rectangle\n')
        self.assertIsNone(rectangle_length['dimension'])
        self.assertEqual(rectangle['description'], '\nA rectangle\n\n')
        self.assertEqual(rectangle['generic_interfaces'], {})
        self.assertEqual(rectangle['procedures'], {})
        self.assertEqual(rectangle['extends'], 'shape')

    def test_multiple_derived_type_no_comments(self):
        self.fs.create_file(
            '/fake/path/types.f90',
            contents='''\
 module shapes
     implicit none
     private
     public :: shape, rectangle

     type, abstract :: shape
         real :: area
     contains
         procedure(calculate_area), deferred :: calc_area
     end type shape

     interface
         function calculate_area(this) result(area)
             import :: shape
             class(shape), intent(in) :: this
             real :: area
         end function calculate_area
     end interface

     type, extends(shape) :: rectangle
         real :: length
         real :: width
     contains
         procedure :: calc_area => calc_rectangle_area
     end type rectangle

 contains

     function calc_rectangle_area(this) result(area)
         class(rectangle), intent(in) :: this
         real :: area
         area = this%length * this%width
     end function calc_rectangle_area

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
        self.assertCountEqual(shape['attributes'], ['public', 'abstract'])
        self.assertEqual(len(shape['data_components']), 1)
        shape_area = shape['data_components']['area']
        self.assertEqual(shape_area['attributes'], [])
        self.assertEqual(shape_area['description'], '')
        self.assertIsNone(shape_area['dimension'])
        self.assertEqual(shape['description'], '')
        self.assertEqual(shape['generic_interfaces'], {})
        shape_procedures = shape['procedures']
        self.assertEqual((len(shape_procedures)), 1)
        self.assertEqual(shape_procedures['calc_area']['name'], 'calc_area')
        self.assertEqual(shape_procedures['calc_area']['description'], '')
        self.assertFalse(shape_procedures['calc_area']['is_final'])
        self.assertEqual(shape_procedures['calc_area']['attributes'], ['deferred'])
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
        rectangle_procedures = rectangle['procedures']
        self.assertEqual((len(shape_procedures)), 1)
        self.assertEqual(rectangle_procedures['calc_area']['name'], 'calc_area')
        self.assertEqual(rectangle_procedures['calc_area']['description'], '')
        self.assertFalse(rectangle_procedures['calc_area']['is_final'])
        self.assertEqual(rectangle_procedures['calc_area']['attributes'], [])
        self.assertEqual(rectangle['extends'], 'shape')

    def test_multiple_derived_type_with_comments(self):
        self.fs.create_file(
            '/fake/path/types.f90',
            contents='''\
 module shapes
     implicit none
     private
     public :: shape, rectangle

     !!* Defines a shape *!
     type, abstract :: shape
         !!* The area of the shape *!
         real :: area
     contains
         !!* Calculates the area of a shape *!
         procedure(calculate_area), deferred :: calc_area
     end type shape

     !!* Calculates the area of a shape *!
     interface
         !!* Calculate the area *!
         function calculate_area(this) result(area)
             import :: shape
             class(shape), intent(in) :: this
             real :: area
         end function calculate_area
     end interface

     !!* Defines a rectangle *!
     type, extends(shape) :: rectangle
         !!* The length of the rectangle *!
         real :: length
         !!* The width of the rectangle *!
         real :: width
     contains
         procedure :: calc_area => calc_rectangle_area
     end type rectangle

 contains

     !!* Area of a rectangle = length * width *!
     function calc_rectangle_area(this) result(area)
         class(rectangle), intent(in) :: this
         real :: area
         area = this%length * this%width
     end function calc_rectangle_area

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
        self.assertCountEqual(shape['attributes'], ['public', 'abstract'])
        self.assertEqual(len(shape['data_components']), 1)
        shape_area = shape['data_components']['area']
        self.assertEqual(shape_area['attributes'], [])
        self.assertEqual(shape_area['description'], 'The area of the shape\n')
        self.assertIsNone(shape_area['dimension'])
        self.assertEqual(shape['description'], 'Defines a shape\n')
        self.assertEqual(shape['generic_interfaces'], {})
        shape_procedures = shape['procedures']
        self.assertEqual((len(shape_procedures)), 1)
        self.assertEqual(shape_procedures['calc_area']['name'], 'calc_area')
        self.assertEqual(shape_procedures['calc_area']['description'], 'Calculates the area of a shape\n')
        self.assertFalse(shape_procedures['calc_area']['is_final'])
        self.assertEqual(shape_procedures['calc_area']['attributes'], ['deferred'])
        self.assertIsNone(shape['extends'])
        rectangle = types['rectangle']
        self.assertEqual(rectangle['type_name'], 'rectangle')
        self.assertEqual(rectangle['attributes'], ['public'])
        self.assertEqual(len(rectangle['data_components']), 2)
        rectangle_width = rectangle['data_components']['width']
        self.assertEqual(rectangle_width['attributes'], [])
        self.assertEqual(rectangle_width['description'], 'The width of the rectangle\n')
        self.assertIsNone(rectangle_width['dimension'])
        rectangle_length = rectangle['data_components']['length']
        self.assertEqual(rectangle_length['attributes'], [])
        self.assertEqual(rectangle_length['description'], 'The length of the rectangle\n')
        self.assertIsNone(rectangle_length['dimension'])
        self.assertEqual(rectangle['description'], 'Defines a rectangle\n')
        self.assertEqual(rectangle['generic_interfaces'], {})
        rectangle_procedures = rectangle['procedures']
        self.assertEqual((len(shape_procedures)), 1)
        self.assertEqual(rectangle_procedures['calc_area']['name'], 'calc_area')
        self.assertEqual(rectangle_procedures['calc_area']['description'], '')
        self.assertFalse(rectangle_procedures['calc_area']['is_final'])
        self.assertEqual(rectangle_procedures['calc_area']['attributes'], [])
        self.assertEqual(rectangle['extends'], 'shape')

    def test_inheritance_tree_no_comments(self):
        self.fs.create_file(
            '/fake/path/types.f90',
            contents='''\
module shapes
     implicit none
     private
     public :: shape, rectangle, square

     type :: shape
         real :: area
     contains
         procedure :: calculate_area => shape_area
         procedure :: describe
     end type shape

     type, extends(shape) :: rectangle
         real :: length
         real :: width
     contains
         procedure :: calculate_area => rectangle_area
         procedure :: init => init_rectangle
     end type rectangle

     type, extends(rectangle) :: square
         ! No additional components
     contains
         procedure :: calculate_area => square_area
         procedure :: init => init_square
     end type square

 contains
     function shape_area(this) result(area)
         class(shape), intent(in) :: this
         real :: area
         area = this%area
     end function shape_area

     subroutine describe(this)
         class(shape), intent(in) :: this
         print *, "This is a shape with area:", this%area
     end subroutine describe

     function rectangle_area(this) result(area)
         class(rectangle), intent(in) :: this
         real :: area
         area = this%length * this%width
     end function rectangle_area

     subroutine init_rectangle(this, length, width)
         class(rectangle), intent(inout) :: this
         real, intent(in) :: length, width
         this%length = length
         this%width = width
         this%area = this%calculate_area()
     end subroutine init_rectangle

     function square_area(this) result(area)
         class(square), intent(in) :: this
         real :: area
         area = this%length ** 2
     end function square_area

     subroutine init_square(this, side)
         class(square), intent(inout) :: this
         real, intent(in) :: side
         this%length = side
         this%width = side
         this%area = this%calculate_area()
     end subroutine init_square

 end module shapes
''',
        )
        result = extract_module_data([Path('/fake/path/types.f90')])
        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(module['module_name'], 'shapes')
        types = module['types']
        self.assertEqual(len(types), 3)
        shape = types['shape']
        self.assertEqual(shape['type_name'], 'shape')
        self.assertCountEqual(shape['attributes'], ['public'])
        self.assertEqual(len(shape['data_components']), 1)
        shape_area = shape['data_components']['area']
        self.assertEqual(shape_area['attributes'], [])
        self.assertEqual(shape_area['description'], '')
        self.assertIsNone(shape_area['dimension'])
        self.assertEqual(shape['description'], '')
        self.assertEqual(shape['generic_interfaces'], {})
        shape_procedures = shape['procedures']
        self.assertEqual((len(shape_procedures)), 2)
        self.assertEqual(shape_procedures['calculate_area']['name'], 'calculate_area')
        self.assertEqual(shape_procedures['calculate_area']['description'], '')
        self.assertFalse(shape_procedures['calculate_area']['is_final'])
        self.assertEqual(shape_procedures['calculate_area']['attributes'], [])
        self.assertEqual(shape_procedures['describe']['name'], 'describe')
        self.assertEqual(shape_procedures['describe']['description'], '')
        self.assertFalse(shape_procedures['describe']['is_final'])
        self.assertEqual(shape_procedures['describe']['attributes'], [])
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
        rectangle_procedures = rectangle['procedures']
        self.assertEqual((len(rectangle_procedures)), 2)
        self.assertEqual(rectangle_procedures['calculate_area']['name'], 'calculate_area')
        self.assertEqual(rectangle_procedures['calculate_area']['description'], '')
        self.assertFalse(rectangle_procedures['calculate_area']['is_final'])
        self.assertEqual(rectangle_procedures['calculate_area']['attributes'], [])
        # self.assertEqual(rectangle_procedures['describe']['name'], 'describe')
        # self.assertEqual(rectangle_procedures['describe']['description'], '')
        # self.assertFalse(rectangle_procedures['describe']['is_final'])
        # self.assertEqual(rectangle_procedures['describe']['attributes'], [])
        self.assertEqual(rectangle_procedures['init']['name'], 'init')
        self.assertEqual(rectangle_procedures['init']['description'], '')
        self.assertFalse(rectangle_procedures['init']['is_final'])
        self.assertEqual(rectangle_procedures['init']['attributes'], [])
        self.assertEqual(rectangle['extends'], 'shape')
        square = types['square']
        self.assertEqual(square['type_name'], 'square')
        self.assertEqual(square['attributes'], ['public'])
        self.assertEqual(len(square['data_components']), 0)
        #TODO
        # square_width = square['data_components']['width']
        # self.assertEqual(square_width['attributes'], [])
        # self.assertEqual(square_width['description'], '')
        # self.assertIsNone(square_width['dimension'])
        # square_length = square['data_components']['length']
        # self.assertEqual(square_length['attributes'], [])
        # self.assertEqual(square_length['description'], '')
        # self.assertIsNone(square_length['dimension'])
        self.assertEqual(square['description'], '')
        self.assertEqual(square['generic_interfaces'], {})
        square_procedures = square['procedures']
        self.assertEqual((len(square_procedures)), 2)
        self.assertEqual(square_procedures['calculate_area']['name'], 'calculate_area')
        self.assertEqual(square_procedures['calculate_area']['description'], '')
        self.assertFalse(square_procedures['calculate_area']['is_final'])
        self.assertEqual(square_procedures['calculate_area']['attributes'], [])
        self.assertEqual(square_procedures['init']['name'], 'init')
        self.assertEqual(square_procedures['init']['description'], '')
        self.assertFalse(square_procedures['init']['is_final'])
        self.assertEqual(square_procedures['init']['attributes'], [])
        # TODO
        # self.assertEqual(square_procedures['describe']['name'], 'describe')
        # self.assertEqual(square_procedures['describe']['description'], '')
        # self.assertFalse(square_procedures['describe']['is_final'])
        # self.assertEqual(square_procedures['describe']['attributes'], [])
        self.assertEqual(square['extends'], 'rectangle')


    def test_inheritance_specific_binding_statement(self):
        self.fs.create_file(
            '/fake/path/types.f90',
            contents='''\
module test_module

  type, extends(function_type) :: sine_type
    contains
      procedure :: f => sine_f
      procedure :: taylor => sine_taylor
  end type sine_type

end module test_module
''',
        )
        result = extract_module_data([Path('/fake/path/types.f90')])
        self.assertEqual(len(result), 1)
        module = result[0]

    def test_inheritance_multiple_binding_statements(self):
        self.fs.create_file(
            '/fake/path/types.f90',
            contents='''\
module test_module

  type :: vector_type
    real :: x, y, z
  contains
    procedure :: add_vector
    procedure :: add_scalar
    generic :: add => add_vector, add_scalar
  end type vector_type

end module test_module
''',
        )
        result = extract_module_data([Path('/fake/path/types.f90')])
        self.assertEqual(len(result), 1)
        module = result[0]

if __name__ == '__main__':
    unittest.main()

