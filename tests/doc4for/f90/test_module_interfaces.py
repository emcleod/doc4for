import unittest
from pathlib import Path
from pyfakefs.fake_filesystem_unittest import TestCase
from doc4for.f90.generate_module_tree import extract_module_data

class TestInterfaces(TestCase):
    maxDiff=None

    def setUp(self):
        self.setUpPyfakefs()

    def test_abstract_interface(self):
        self.fs.create_file(
            '/fake/path/abstract_interface.f90',
            contents='''\
module abstract_interface_mod
    implicit none

    !!*
    ! An abstract interface containing a function <code>func</code>
    !*!
    abstract interface
        !!* Transforms x into y somehow !*!
        function func(x) result(y)
            real, intent(in) :: x
            real :: y
        end function func
    end interface

end module abstract_interface_mod
'''
        )
        result = extract_module_data([Path('/fake/path/abstract_interface.f90')])
        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(len(module['interfaces']), 1)
        interface = module['interfaces'][0]        
        self.assertEqual(interface['description'], "\nAn abstract interface containing a function &lt;code&gt;func&lt;/code&gt;\n\n")
        self.assertEqual(interface['attributes'], ['abstract'])
        self.assertNotIn('operator_symbol', interface)
        self.assertEqual(len(interface['procedures']), 1)
        function = interface['procedures']['func']
        self.assertEqual(function['arguments'], ['x'])
        self.assertEqual(function['attributes'], [])
        self.assertNotIn('binding_type', function)
        # self.assertEqual(function['description'], "Transforms x into y somehow")
        self.assertEqual(function['in'], {'x': {'type': 'real', 'description': '', 'dimension': ''}})
        self.assertEqual(function['out'], {})
        self.assertEqual(function['return'], {'y': {'type': 'real', 'description': '', 'dimension': ''}})
        self.assertNotIn('interface', function)

#     def test_abstract_interface_with_nested_interface(self):
#         self.fs.create_file(
#             "/fake/path/interfaces.f90",
#             contents="""\
# module test_mod
#     implicit none

#     !!* Interface for numerical integration functions
#     ! @param f Function to integrate
#     ! @param a Lower bound
#     ! @param b Upper bound
#     ! @return Integral value
#     !*!
#     abstract interface
#         function integrand(f, a, b) result(integral)
#             implicit none
#             interface
#                 function f(x)
#                     real, intent(in) :: x
#                     real :: f
#                 end function f
#             end interface
#             real, intent(in) :: a, b
#             real :: integral
#         end function integrand
#     end interface

# end module test_mod
# """
#         )
#         result = extract_module_data([Path("/fake/path/interfaces.f90")])
#         self.assertEqual(len(result), 1)
#         module = result[0]
#         self.assertEqual(len(module['interfaces']), 1)
#         interface = module['interfaces']['integrand']
#         self.assertEqual(interface['name'], 'integrand')
#         self.assertEqual(interface['description'], 'Interface for numerical integration functions')
#         self.assertIn('abstract', interface['attributes'])
#         self.assertIsNone(interface['operator_symbol'])
#         self.assertEqual(len(interface['procedures']), 1)
#         self.assertEqual(len(interface['module_procedure_names']), 0)

    def test_named_interface_explicit_procedure(self):
        self.fs.create_file(
            "/fake/path/interfaces.f90",
            contents="""\
module test_mod
    implicit none

    !!*
    ! Interface for vector operations
    ! Contains functions for various vector manipulations
    !*!
    interface vector_ops
        !!*
        ! Normalizes a vector
        ! @param v Input vector
        ! @return Normalized vector
        !*!
        function normalize(v) result(normalized)
            real, dimension(:), intent(in) :: v
            real, dimension(size(v)) :: normalized
        end function normalize

        !!*
        ! Calculates the magnitude of a vector
        ! @param v Input vector
        ! @return Scalar magnitude
        !*!
        function magnitude(v) result(mag)
            real, dimension(:), intent(in) :: v
            real :: mag
        end function magnitude

        !!*
        ! Scales a vector by a factor
        !*!
        subroutine scale(v, factor)
            real, dimension(:), intent(inout) :: v
            real, intent(in) :: factor
        end subroutine scale
    end interface vector_ops

contains
    function normalize(v) result(normalized)
        real, dimension(:), intent(in) :: v
        real, dimension(size(v)) :: normalized
        real :: mag
        mag = sqrt(sum(v**2))
        normalized = v / mag
    end function normalize

    function magnitude(v) result(mag)
        real, dimension(:), intent(in) :: v
        real :: mag
        mag = sqrt(sum(v**2))
    end function magnitude

    subroutine scale(v, factor)
        real, dimension(:), intent(inout) :: v
        real, intent(in) :: factor
        v = v * factor
    end subroutine scale
end module test_mod
"""
        )
        result = extract_module_data([Path("/fake/path/interfaces.f90")])
        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(len(module['interfaces']), 1)
        interface = module['interfaces'][0]
        self.assertEqual(interface['name'], 'vector_ops')
        self.assertEqual(interface['description'], 'Interface for vector addition\n\n')
        self.assertEqual(interface['operator_symbol'], '+')
        self.assertEqual(len(interface['module_procedure_names']), 2)
        self.assertIn('add_vectors', interface['module_procedure_names'])
        self.assertIn('add_vector_scalar', interface['module_procedure_names'])


#     def test_named_interface_module_procedure(self):
#         self.fs.create_file(
#             "/fake/path/interfaces.f90",
#             contents="""\

# """
#         )
#         result = extract_module_data([Path("/fake/path/interfaces.f90")])
#         self.assertEqual(len(result), 1)
#         module = result[0]
#         self.assertEqual(len(module['interfaces']), 1)
#         interface = module['interfaces'][0]
#         self.assertEqual(interface['name'], 'operator(+)')
#         self.assertEqual(interface['description'], 'Interface for vector addition\n\n')
#         self.assertEqual(interface['operator_symbol'], '+')
#         self.assertEqual(len(interface['module_procedure_names']), 2)
#         self.assertIn('add_vectors', interface['module_procedure_names'])
#         self.assertIn('add_vector_scalar', interface['module_procedure_names'])

#     def test_operator_interface_explicit_procedure(self):
#         self.fs.create_file(
#             "/fake/path/interfaces.f90",
#             contents="""\
# module test_mod
#     implicit none

#     !!* Interface for vector addition
#     !*!
#     interface operator(+)
#         !!*
#         ! Adds vectors 
#         !*!
#         function add_vectors(a, b)
#           real, dimension(:), intent(in) :: a, b
#           real, dimension(size(a)) :: add_vectors
#         end function add_vectors
#         function add_vector_scalar(a, b)
#           real, dimension(:), intent(in) :: a
#           real, intent(in) :: b
#           real, dimension(size(a)):: add_vector_scalar
#         end function
#     end interface

# contains
#     function add_vectors(a, b) result(c)
#         real, intent(in) :: a(:), b(:)
#         real, allocatable :: c(:)
#         c = a + b
#     end function add_vectors

#     function add_vector_scalar(a, b) result(c)
#         real, intent(in) :: a(:), b
#         real, allocatable :: c(:)
#         c = a + b
#     end function add_vector_scalar
# end module test_mod
# """
#         )
#         result = extract_module_data([Path("/fake/path/interfaces.f90")])
#         self.assertEqual(len(result), 1)
#         module = result[0]
#         self.assertEqual(len(module['interfaces']), 1)
#         interface = module['interfaces'][0]
#         self.assertEqual(interface['name'], 'operator(+)')
#         self.assertEqual(interface['description'], 'Interface for vector addition\n\n')
#         self.assertEqual(interface['operator_symbol'], '+')
#         self.assertEqual(len(interface['module_procedure_names']), 2)
#         self.assertIn('add_vectors', interface['module_procedure_names'])
#         self.assertIn('add_vector_scalar', interface['module_procedure_names'])
#         interface = module['interfaces'][0]        
#         self.assertEqual(interface['description'], "\nAn abstract interface containing a function &lt;code&gt;func&lt;/code&gt;\n\n")
#         self.assertEqual(interface['attributes'], ['abstract'])
#         self.assertNotIn('operator_symbol', interface)
#         self.assertEqual(len(interface['procedures']), 1)
#         function = interface['procedures']['func']
#         self.assertEqual(function['arguments'], ['x'])
#         self.assertEqual(function['attributes'], [])
#         self.assertNotIn('binding_type', function)
#         # self.assertEqual(function['description'], "Transforms x into y somehow")
#         self.assertEqual(function['in'], {'x': {'type': 'real', 'description': '', 'dimension': ''}})
#         self.assertEqual(function['out'], {})
#         self.assertEqual(function['return'], {'y': {'type': 'real', 'description': '', 'dimension': ''}})
#         self.assertNotIn('interface', function)

#     def test_operator_interface_module_procedure(self):
#         self.fs.create_file(
#             "/fake/path/interfaces.f90",
#             contents="""\
# module test_mod
#     implicit none

#     !!* Interface for vector addition
#     !*!
#     interface operator(+)
#         !!*
#         ! Adds vectors 
#         !*!
#         module procedure add_vectors
#         module procedure add_vector_scalar
#     end interface

# contains
#     function add_vectors(a, b) result(c)
#         real, intent(in) :: a(:), b(:)
#         real, allocatable :: c(:)
#         c = a + b
#     end function add_vectors

#     function add_vector_scalar(a, b) result(c)
#         real, intent(in) :: a(:), b
#         real, allocatable :: c(:)
#         c = a + b
#     end function add_vector_scalar
# end module test_mod
# """
#         )
#         result = extract_module_data([Path("/fake/path/interfaces.f90")])
#         self.assertEqual(len(result), 1)
#         module = result[0]
#         self.assertEqual(len(module['interfaces']), 1)
#         interface = module['interfaces']['operator(+)']
#         self.assertEqual(interface['name'], 'operator(+)')
#         self.assertEqual(interface['description'], 'Interface for vector addition')
#         self.assertEqual(interface['operator_symbol'], '+')
#         self.assertEqual(len(interface['module_procedure_names']), 2)
#         self.assertIn('add_vectors', interface['module_procedure_names'])
#         self.assertIn('add_vector_scalar', interface['module_procedure_names'])

#     def test_assignment_interface(self):
#         self.fs.create_file(
#             "/fake/path/interfaces.f90",
#             contents="""\
# module test_mod
#     implicit none

#     !!* Custom assignment for vector type
#     !*!
#     interface assignment(=)
#         module procedure assign_vector
#     end interface

# contains
#     subroutine assign_vector(a, b)
#         real, allocatable, intent(out) :: a(:)
#         real, intent(in) :: b(:)
#         a = b
#     end subroutine assign_vector
# end module test_mod
# """
#         )
#         result = extract_module_data([Path("/fake/path/interfaces.f90")])
#         self.assertEqual(len(result), 1)
#         module = result[0]
#         self.assertEqual(len(module['interfaces']), 1)
#         interface = module['interfaces']['assignment(=)']
#         self.assertEqual(interface['name'], 'assignment(=)')
#         self.assertEqual(interface['description'], 'Custom assignment for vector type')
#         self.assertEqual(interface['operator_symbol'], '=')
#         self.assertEqual(len(interface['module_procedure_names']), 1)
#         self.assertIn('assign_vector', interface['module_procedure_names'])

#     def test_comprehensive_interfaces(self):
#         self.fs.create_file(
#             "/fake/path/interfaces.f90",
#             contents="""\
# module math_operations
#     implicit none

#     !!* Interface for function integration
#     !*!
#     abstract interface
#         function func_interface(x)
#             real, intent(in) :: x
#             real :: func_interface
#         end function func_interface
#     end interface

#     !!* Vector operations interface
#     !*!
#     interface operator(.dot.)
#         module procedure dot_product_r
#         module procedure dot_product_d
#     end interface

#     !!* Matrix assignment interface
#     !*!
#     interface assignment(=)
#         module procedure assign_matrix_real
#         module procedure assign_matrix_double
#     end interface

#     !!* Generic norm interface
#     !*!
#     interface norm
#         module procedure vector_norm
#         module procedure matrix_norm
#     end interface

# contains
#     real function dot_product_r(a, b)
#         real, intent(in) :: a(:), b(:)
#         dot_product_r = sum(a * b)
#     end function dot_product_r

#     double precision function dot_product_d(a, b)
#         double precision, intent(in) :: a(:), b(:)
#         dot_product_d = sum(a * b)
#     end function dot_product_d

#     subroutine assign_matrix_real(a, b)
#         real, allocatable, intent(out) :: a(:,:)
#         real, intent(in) :: b(:,:)
#         a = b
#     end subroutine assign_matrix_real

#     subroutine assign_matrix_double(a, b)
#         double precision, allocatable, intent(out) :: a(:,:)
#         double precision, intent(in) :: b(:,:)
#         a = b
#     end subroutine assign_matrix_double

#     function vector_norm(v) result(n)
#         real, intent(in) :: v(:)
#         real :: n
#         n = sqrt(sum(v**2))
#     end function vector_norm

#     function matrix_norm(m) result(n)
#         real, intent(in) :: m(:,:)
#         real :: n
#         n = sqrt(sum(m**2))
#     end function matrix_norm

# end module math_operations
# """
#         )
#         result = extract_module_data([Path("/fake/path/interfaces.f90")])
#         self.assertEqual(len(result), 1)
#         module = result[0]
        
#         # Check all interfaces exist
#         self.assertEqual(len(module['interfaces']), 2)  # abstract and operator interfaces
#         self.assertEqual(len(module['generic_interfaces']), 1)  # norm interface
        
#         # Check abstract interface
#         func_interface = module['interfaces']['func_interface']
#         self.assertEqual(func_interface['name'], 'func_interface')
#         self.assertIn('abstract', func_interface['attributes'])
        
#         # Check operator interface
#         dot_interface = module['interfaces']['operator(.dot.)']
#         self.assertEqual(dot_interface['operator_symbol'], '.dot.')
#         self.assertEqual(len(dot_interface['module_procedure_names']), 2)
#         self.assertIn('dot_product_r', dot_interface['module_procedure_names'])
#         self.assertIn('dot_product_d', dot_interface['module_procedure_names'])
        
#         # Check assignment interface
#         assign_interface = module['interfaces']['assignment(=)']
#         self.assertEqual(assign_interface['operator_symbol'], '=')
#         self.assertEqual(len(assign_interface['module_procedure_names']), 2)
#         self.assertIn('assign_matrix_real', assign_interface['module_procedure_names'])
#         self.assertIn('assign_matrix_double', assign_interface['module_procedure_names'])
        
#         # Check generic interface
#         norm_interface = module['generic_interfaces']['norm']
#         self.assertEqual(len(norm_interface['specific_procedures']), 2)
#         self.assertIn('vector_norm', norm_interface['specific_procedures'])
#         self.assertIn('matrix_norm', norm_interface['specific_procedures'])

if __name__ == '__main__':
    unittest.main()


# module complex_interfaces
#     implicit none
#     private

#     public :: general_math, vector_ops

#     interface general_math
#         module procedure int_func, real_func
#         procedure double_func
#     end interface

#     interface vector_ops
#         pure function dot_product(a, b) result(c)
#             real, dimension(:), intent(in) :: a, b
#             real :: c
#         end function dot_product

#         elemental subroutine scale(v, factor)
#             real, intent(inout) :: v
#             real, intent(in) :: factor
#         end subroutine scale

#         function cross_product(a, b, c) result(d)
#             real, dimension(3), intent(in) :: a, b
#             real, dimension(3), intent(out), optional :: c
#             real, dimension(3) :: d
#         end function cross_product
#     end interface

#     abstract interface
#         function func(x, callback) result(y)
#             import
#             real, intent(in) :: x
#             procedure(real_func) :: callback
#             real :: y
#         end function func
#     end interface

#     interface
#         function external_func(x) bind(C, name="c_func")
#             use, intrinsic :: iso_c_binding
#             real(c_float), value :: x
#             real(c_float) :: external_func
#         end function external_func
#     end interface

# contains
#     function int_func(x)
#         integer, intent(in) :: x
#         integer :: int_func
#         int_func = x * 2
#     end function int_func

#     function real_func(x)
#         real, intent(in) :: x
#         real :: real_func
#         real_func = x * 2.0
#     end function real_func
# end module complex_interfaces    


# module complex_interfaces
#     implicit none
#     private
#     public :: vector_ops, operator(+)

#     !!* Private interface for internal use *!
#     private interface internal_ops
#         module procedure internal_func
#     end interface

#     !!* Public interface with various procedure types *!
#     public interface vector_ops
#         pure function pure_op(x)
#             real, intent(in) :: x
#             real :: pure_op
#         end function

#         elemental function elem_op(x)
#             real, intent(in) :: x
#             real :: elem_op
#         end function

#         recursive function rec_op(x)
#             real, intent(in) :: x
#             real :: rec_op
#         end function

#         function array_op(x)
#             real, dimension(:), intent(in) :: x
#             real, dimension(size(x)) :: array_op
#         end function

#         function procedure_arg(func, x)
#             interface
#                 function func(x)
#                     real, intent(in) :: x
#                     real :: func
#                 end function
#             end interface
#             real, intent(in) :: x
#             real :: procedure_arg
#         end function
#     end interface

#     !!* Interface for derived type IO *!
#     interface write(formatted)
#         module procedure write_mytype
#     end interface

# contains
#     ! Implementation procedures...
# end module complex_interfaces