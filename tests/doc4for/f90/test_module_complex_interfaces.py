import unittest
from pathlib import Path
from pyfakefs.fake_filesystem_unittest import TestCase
from doc4for.f90.generate_module_tree import extract_module_data
from doc4for.models.variable_models import PolymorphismType

class TestInterfaces(TestCase):
    maxDiff=None

    def setUp(self):
        self.setUpPyfakefs()

    def test_generic_interface_mixed(self):
        """Test a generic interface with both explicit procedures and module procedures."""
        self.fs.create_file(
            "/fake/path/mixed_interface.f90",
            contents="""\
    module mixed_interface_mod
        implicit none

        !!*
        ! Generic interface for merging data
        ! Supports different merge algorithms
        !*!
        interface merge_data
            module procedure merge_sorted
            module procedure merge_unsorted

            !!*
            ! Merges two arrays with custom comparison function
            ! @in arr1 First array
            ! @in arr2 Second array
            ! @in comparator Comparison function
            ! @return Merged array
            !*!
            function merge_with_comparator(arr1, arr2, comparator) result(merged)
                implicit none
                real, dimension(:), intent(in) :: arr1, arr2
                !!* Comparison function interface *!
                interface
                    !!*
                    ! Compares two real values
                    ! @in x First value
                    ! @in y Second value
                    ! @return True if x should come before y
                    !*!
                    function comparator(x, y)
                        real, intent(in) :: x, y
                        logical :: comparator
                    end function comparator
                end interface
                real, dimension(size(arr1) + size(arr2)) :: merged
            end function merge_with_comparator
        end interface merge_data

    contains
        function merge_sorted(arr1, arr2) result(merged)
            real, dimension(:), intent(in) :: arr1, arr2
            real, dimension(size(arr1) + size(arr2)) :: merged
            ! Implementation
        end function merge_sorted

        subroutine merge_unsorted(arr1, arr2, out)
            real, dimension(:), intent(in) :: arr1, arr2
            real, dimension(:), intent(out) :: out
            ! Implementation
        end subroutine merge_unsorted
    end module mixed_interface_mod
    """
        )
        
        result = extract_module_data([Path("/fake/path/mixed_interface.f90")])
        module = result[0]
        interface = module["interfaces"][0]

        # Check interface type
        self.assertEqual(interface["name"], "merge_data")
        self.assertEqual(interface["description"], "\nGeneric interface for merging data\nSupports different merge algorithms\n\n")
        
        # Check module procedures
        self.assertEqual(len(interface["module_procedures"]), 2)
        self.assertIn("merge_sorted", interface["module_procedures"])
        self.assertIn("merge_unsorted", interface["module_procedures"])
        
        # Check explicit procedures
        self.assertEqual(len(interface["procedures"]), 1)
        merge_comp = interface["procedures"]["merge_with_comparator"]
        self.assertEqual(len(merge_comp["argument_interfaces"]), 1)
        
        # Check nested interface
        comp_interface = merge_comp["argument_interfaces"]["comparator"]
        self.assertEqual(comp_interface["description"], "Comparison function interface\n")
        comp_func = comp_interface["procedures"]["comparator"]
        self.assertEqual(comp_func["return"]["type"], "LOGICAL")

    def test_interface_multiple_operators(self):
        """Test module with multiple operator interfaces."""
        self.fs.create_file(
            "/fake/path/operators.f90",
            contents="""\
    module matrix_operators
        implicit none

        !!* Matrix addition interface *!
        interface operator(+)
            module procedure add_matrices
        end interface

        !!* Matrix multiplication interface *!
        interface operator(*)
            module procedure multiply_matrices
        end interface

        !!* Matrix equality test interface *!
        interface operator(==)
            function compare_matrices(a, b) result(equal)
                real, dimension(:,:), intent(in) :: a, b
                logical :: equal
            end function compare_matrices
        end interface

    contains
        function add_matrices(a, b) result(sum)
            real, dimension(:,:), intent(in) :: a, b
            real, dimension(size(a,1), size(a,2)) :: sum
            sum = a + b
        end function add_matrices

        function multiply_matrices(a, b) result(product)
            real, dimension(:,:), intent(in) :: a, b
            real, dimension(size(a,1), size(b,2)) :: product
            product = matmul(a, b)
        end function multiply_matrices
    end module matrix_operators
    """
        )
        result = extract_module_data([Path("/fake/path/operators.f90")])
        module = result[0]
        
        self.assertEqual(len(module["interfaces"]), 3)
        
        # Check operator symbols
        operators = ["+", "*", "=="]
        for i, op in enumerate(operators):
            self.assertEqual(module["interfaces"][i]["operator_symbol"], op)
        
        # Check types of procedures
        self.assertEqual(len(module["interfaces"][0]["module_procedures"]), 1)  # +
        self.assertEqual(len(module["interfaces"][1]["module_procedures"]), 1)  # *
        self.assertEqual(len(module["interfaces"][2]["procedures"]), 1)  # ==
        self.assertEqual(len(module["interfaces"][2]["module_procedures"]), 0)  # ==

    def test_interface_with_use_statements(self):
        """Test interfaces that use types from other modules."""
        self.fs.create_file(
            "/fake/path/interface_with_type.f90",
            contents="""\
    module custom_types
        implicit none
        type :: point
            real :: x, y
        end type point
    end module custom_types

    module operations
        use custom_types
        implicit none

        !!*
        ! Interface for operations on custom types
        ! Provides distance calculation for points
        !*!
        interface distance
            !!*
            ! Calculates the distance between two points
            ! @in p1 First point
            ! @in p2 Second point
            ! @return Distance between points
            !*!
            function distance_points(p1, p2) result(dist)
                import :: point
                type(point), intent(in) :: p1, p2
                real :: dist
            end function distance_points
        end interface

    contains
        function distance_points(p1, p2) result(dist)
            type(point), intent(in) :: p1, p2
            real :: dist
            dist = sqrt((p1%x - p2%x)**2 + (p1%y - p2%y)**2)
        end function distance_points
    end module operations
    """
        )
        # This test simulates handling of modules that import types from other modules
        result = extract_module_data([Path("/fake/path/interface_with_type.f90")])
        operations_mod = result[1]  # Second module: operations
        
        interface = operations_mod["interfaces"][0]
        proc = interface["procedures"]["distance_points"]
        
        # Check if the type information is correctly parsed
        self.assertEqual(proc["in"]["p1"]["type"], "point")
        self.assertEqual(proc["in"]["p2"]["type"], "point")
        self.assertEqual(proc["return"]["type"], "REAL")

    def test_interface_with_deferred_binding(self):
        """Test interface with type-bound procedures."""
        self.fs.create_file(
            "/fake/path/deferred_binding.f90",
            contents="""\
    module abstract_type
        implicit none

        type, abstract :: shape
        contains
            !!* Abstract interface for area calculation *!
            procedure(area_interface), deferred :: area
            !!* Abstract interface for perimeter calculation *!
            procedure(perimeter_interface), deferred :: perimeter
        end type shape

        !!* Interface for area calculation *!
        abstract interface
            !!*
            ! Calculates the area of a shape
            ! @in this Shape instance
            ! @return Area of the shape
            !*!
            function area_interface(this) result(area)
                import :: shape
                class(shape), intent(in) :: this
                real :: area
            end function area_interface

            !!*
            ! Calculates the perimeter of a shape
            ! @in this Shape instance
            ! @return Perimeter of the shape
            !*!
            function perimeter_interface(this) result(perimeter)
                import :: shape
                class(shape), intent(in) :: this
                real :: perimeter
            end function perimeter_interface
        end interface

    end module abstract_type
    """
        )
        result = extract_module_data([Path("/fake/path/deferred_binding.f90")])
        module = result[0]
        
        interfaces = module["interfaces"]
        self.assertEqual(len(interfaces), 1)
        
        for interface in module["interfaces"]:
            self.assertEqual(interface["attributes"], ["ABSTRACT"])
        
        # Check the import statement was handled
        area_interface = module["interfaces"][0]
        area_proc = area_interface["procedures"]["area_interface"]
        self.assertEqual(area_proc["in"]["this"]["type"], "shape")
        self.assertEqual(area_proc["in"]["this"]["polymorphism_type"], PolymorphismType.LIMITED)

    def test_interface_with_elemental_pure(self):
        """Test interface with elemental and pure procedures."""
        self.fs.create_file(
            "/fake/path/elemental_pure.f90",
            contents="""\
    module math_functions
        implicit none

        !!* 
        ! Interface for mathematical functions
        ! Provides elemental and pure operations
        !*!
        interface sigmoid
            !!*
            ! Computes the sigmoid function element-wise
            ! @in x Input value or array
            ! @return Sigmoid(x)
            !*!
            elemental pure function sigmoid_scalar(x) result(y)
                real, intent(in) :: x
                real :: y
            end function sigmoid_scalar
        end interface

        !!* Interface for safe division *!
        interface safe_divide
            module procedure safe_divide_real
            module procedure safe_divide_int
        end interface

    contains
        elemental pure function sigmoid_scalar(x) result(y)
            real, intent(in) :: x
            real :: y
            y = 1.0 / (1.0 + exp(-x))
        end function sigmoid_scalar

        elemental pure function safe_divide_real(a, b, default) result(c)
            real, intent(in) :: a, b
            real, intent(in), optional :: default
            real :: c
            if (abs(b) < epsilon(b)) then
                if (present(default)) then
                    c = default
                else
                    c = 0.0
                end if
            else
                c = a / b
            end if
        end function safe_divide_real

        pure function safe_divide_int(a, b, default) result(c)
            integer, intent(in) :: a, b
            integer, intent(in), optional :: default
            real :: c
            if (b == 0) then
                if (present(default)) then
                    c = real(default)
                else
                    c = 0.0
                end if
            else
                c = real(a) / real(b)
            end if
        end function safe_divide_int
    end module math_functions
    """
        )
        result = extract_module_data([Path("/fake/path/elemental_pure.f90")])
        module = result[0]
        
        # Check sigmoid interface
        sigmoid_interface = module["interfaces"][0]
        sigmoid_proc = sigmoid_interface["procedures"]["sigmoid_scalar"]
        self.assertEqual(sigmoid_proc["attributes"], ["ELEMENTAL", "PURE"])
        
        # Check safe_divide interface
        safe_divide_interface = module["interfaces"][1]
        self.assertEqual(len(safe_divide_interface["module_procedures"]), 2)

if __name__ == "__main__":
    unittest.main()

#TODO
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