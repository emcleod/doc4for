import unittest
from pathlib import Path
from pyfakefs.fake_filesystem_unittest import TestCase
from doc4for.f90.generate_module_tree import extract_module_data
from doc4for.models.common import BindingTypeEnum
from doc4for.models.procedure_models import PassType

class TestTypeBindingProcedures(TestCase):

    def setUp(self):
        self.setUpPyfakefs()

    def test_type_bound_procedure_binding(self):
        self.fs.create_file(
            "/fake/path/type_bound_proc_binding.f90",
            contents="""\
    module type_bound_proc_binding_mod
        use iso_c_binding
        implicit none

        !!* Basic shape type with C-bound method *!
        type :: shape
            real(c_double) :: area
        contains
            !!* Calculate area using C binding *!
            procedure, nopass :: calculate => calculate_shape_area
            
            !!* Regular method *!
            procedure :: regular_method => regular_impl
        end type shape
        
        !!* Complex shape *!
        type :: complex_shape
        contains
            !!* Area calculation with custom name binding *!
            procedure :: area => complex_area
        end type complex_shape
        
        !!* Type *!
        type :: weird_shape
        contains
            procedure :: weird_method => weird_impl
        end type weird_shape
        
        !!* Type with binding syntax variations *!
        type :: methods
        contains
            procedure, pass(self) :: method1 => impl1
            
            procedure, public :: method2 => impl2
            
            procedure :: method3 => impl3
        end type methods

    contains
        
        !!* Implementation with its own binding *!
        function calculate_shape_area(s) bind(c, name="c_shape_area") result(a)
            type(shape), intent(in) :: s
            real(c_double) :: a
            a = s%area
        end function
        
        !!* Regular implementation without binding *!
        function regular_impl(this) result(res)
            class(shape), intent(in) :: this
            real :: res
            res = this%area
        end function
        
        !!* Implementation with binding matching its type-bound declaration *!
        function complex_area(this) bind(c) result(a)
            class(complex_shape), intent(in) :: this
            real(c_double) :: a
            a = 0.0  ! Placeholder
        end function
        
        !!* Implementation with unusual binding syntax *!
        subroutine weird_impl(this) bind  (  C  )
            class(weird_shape), intent(inout) :: this
            ! Empty implementation
        end subroutine
        
        !!* Mixed case binding implementation *!
        function impl1(self) Bind(C) result(res)
            class(methods), intent(in) :: self
            integer(c_int) :: res
            res = 1
        end function
        
        !!* Implementation with other attributes *!
        function impl2(this) bind(c) result(res)
            class(methods), intent(in) :: this
            integer(c_int) :: res
            res = 2
        end function
        
        !!* Implementation with quoted name *!
        function impl3(this) bind(c, name="different_quoted_name") result(res)
            class(methods), intent(in) :: this
            integer(c_int) :: res
            res = 3
        end function

    end module type_bound_proc_binding_mod
    """
        )
        result = extract_module_data([Path("/fake/path/type_bound_proc_binding.f90")])
        module = result[0]
        
        # Check type-bound procedures 
        shape_type = module["types"]["shape"]
        
        # Check procedure attributes
        calculate_proc = shape_type["procedures"]["calculate"]
        self.assertEqual(calculate_proc["pass_type"], PassType.NONE)
        
        # Check regular procedure without binding
        regular_proc = shape_type["procedures"]["regular_method"]
        self.assertEqual(regular_proc["pass_type"], PassType.DEFAULT)
        
        # Check complex type procedures
        complex_type = module["types"]["complex_shape"]
        area_proc = complex_type["procedures"]["area"]
        self.assertEqual(area_proc["pass_type"], PassType.DEFAULT)
        
        # Check weird type procedures
        weird_type = module["types"]["weird_shape"]
        weird_proc = weird_type["procedures"]["weird_method"]
        self.assertEqual(weird_proc["pass_type"], PassType.DEFAULT)
        
        # Check methods type procedures
        var_type = module["types"]["methods"]
        
        # Method with pass(self)
        method1 = var_type["procedures"]["method1"]
        self.assertEqual(method1["pass_type"], PassType.NAMED)
        self.assertEqual(method1["pass_name"], "self")

        # Method with public attribute
        method2 = var_type["procedures"]["method2"]
        self.assertIn("PUBLIC", method2["attributes"])
        
        # Regular method
        method3 = var_type["procedures"]["method3"]
        self.assertEqual(method3["pass_type"], PassType.DEFAULT)
                
        # Implementation with binding and custom name
        calculate_impl = module["functions"]["calculate_shape_area"]
        self.assertIn("binding_type", calculate_impl)
        self.assertEqual(calculate_impl["binding_type"]["type"], BindingTypeEnum.BIND_C)
        self.assertEqual(calculate_impl["binding_type"]["name"], "c_shape_area")
        
        # Regular implementation without binding
        regular_impl = module["functions"]["regular_impl"]
        self.assertIn("binding_type", regular_impl)
        self.assertIsNone(regular_impl["binding_type"])
        
        # Complex implementation with default binding
        complex_impl = module["functions"]["complex_area"]
        self.assertIn("binding_type", complex_impl)
        self.assertEqual(complex_impl["binding_type"]["type"], BindingTypeEnum.BIND_C)
        self.assertIsNone(complex_impl["binding_type"]["name"])  # No explicit name
        
        # Weird implementation with unusual spacing
        weird_impl = module["subroutines"]["weird_impl"]
        self.assertIn("binding_type", weird_impl)
        self.assertEqual(weird_impl["binding_type"]["type"], BindingTypeEnum.BIND_C)
        self.assertIsNone(weird_impl["binding_type"]["name"])  # No explicit name
        
        # Mixed case binding implementation
        impl1 = module["functions"]["impl1"]
        self.assertIn("binding_type", impl1)
        self.assertEqual(impl1["binding_type"]["type"], BindingTypeEnum.BIND_C)
        self.assertIsNone(impl1["binding_type"]["name"])  # No explicit name
        
        # Implementation with binding
        impl2 = module["functions"]["impl2"]
        self.assertIn("binding_type", impl2)
        self.assertEqual(impl2["binding_type"]["type"], BindingTypeEnum.BIND_C)
        self.assertIsNone(impl2["binding_type"]["name"])  # No explicit name
        
        # Implementation with quoted name binding
        impl3 = module["functions"]["impl3"]
        self.assertIn("binding_type", impl3)
        self.assertEqual(impl3["binding_type"]["type"], BindingTypeEnum.BIND_C)
        self.assertEqual(impl3["binding_type"]["name"], "different_quoted_name")

    def test_generic_binding_with_bind_c(self):
        self.fs.create_file(
            "/fake/path/generic_binding.f90",
            contents="""\
    module generic_binding_mod
        use iso_c_binding
        implicit none
        
        type :: numeric_ops
        contains
            !!* Generic add operation *!
            generic :: add => add_int, add_real
            
            !!* Specific implementations for different types *!
            procedure :: add_int
            procedure :: add_real
            
            !!* Generic without binding *!
            generic :: subtract => subtract_int, subtract_real
            procedure :: subtract_int
            procedure :: subtract_real
        end type numeric_ops
        
        contains
        
        !!* Add integers with C binding *!
        function add_int(this, a, b) bind(c, name="add_int_c") result(res)
            class(numeric_ops), intent(in) :: this
            integer(c_int), value :: a, b
            integer(c_int) :: res
            res = a + b
        end function
        
        !!* Add reals with C binding *!
        function add_real(this, a, b) bind(c, name="add_real_c") result(res)
            class(numeric_ops), intent(in) :: this
            real(c_double), value :: a, b
            real(c_double) :: res
            res = a + b
        end function
        
        !!* Subtract integers (no C binding) *!
        function subtract_int(this, a, b) result(res)
            class(numeric_ops), intent(in) :: this
            integer, intent(in) :: a, b
            integer :: res
            res = a - b
        end function
        
        !!* Subtract reals (no C binding) *!
        function subtract_real(this, a, b) result(res)
            class(numeric_ops), intent(in) :: this
            real, intent(in) :: a, b
            real :: res
            res = a - b
        end function
        
    end module generic_binding_mod
    """
        )
        result = extract_module_data([Path("/fake/path/generic_binding.f90")])
        module = result[0]
        
        # Check type with generic bindings
        num_ops = module["types"]["numeric_ops"]
        
        # Check generic interface for add (no binding on generic itself)
        add_generic = num_ops["generic_interfaces"]["add"]
        self.assertEqual(add_generic["specific_procedures"], ["add_int", "add_real"])
        
        # Check generic interface for subtract (no binding on generic itself)
        subtract_generic = num_ops["generic_interfaces"]["subtract"]
        self.assertEqual(subtract_generic["specific_procedures"], ["subtract_int", "subtract_real"])
        
        # Check specific procedure implementations have correct binding
        add_int = module["functions"]["add_int"]
        self.assertIn("binding_type", add_int)
        self.assertEqual(add_int["binding_type"]["type"], BindingTypeEnum.BIND_C)
        self.assertEqual(add_int["binding_type"]["name"], "add_int_c")
        
        add_real = module["functions"]["add_real"]
        self.assertIn("binding_type", add_real)
        self.assertEqual(add_real["binding_type"]["type"], BindingTypeEnum.BIND_C)
        self.assertEqual(add_real["binding_type"]["name"], "add_real_c")
        
        subtract_int = module["functions"]["subtract_int"]
        self.assertIn("binding_type", subtract_int)
        self.assertIsNone(subtract_int["binding_type"])  # No binding
        
        subtract_real = module["functions"]["subtract_real"]
        self.assertIn("binding_type", subtract_real)
        self.assertIsNone(subtract_real["binding_type"])  # No binding

    def test_final_binding_with_bind_c(self):
        self.fs.create_file(
            "/fake/path/final_binding.f90",
            contents="""\
    module final_binding_mod
        use iso_c_binding
        implicit none
        
        type :: resource_holder
            type(c_ptr) :: handle = c_null_ptr
        contains
            !!* Finalizer with C binding *!
            final :: cleanup
        end type resource_holder
        
        type :: simple_type
            integer :: data
        contains
            !!* Regular finalizer without binding *!
            final :: simple_cleanup
        end type simple_type
        
        contains
        
        !!* Resource cleanup with C binding to allow external resource cleanup *!
        subroutine cleanup(this) bind(c, name="c_resource_cleanup")
            type(resource_holder), intent(inout) :: this
            ! Implementation to free C resources
        end subroutine
        
        !!* Simple cleanup without C binding *!
        subroutine simple_cleanup(this)
            type(simple_type), intent(inout) :: this
            ! Implementation
        end subroutine
        
    end module final_binding_mod
    """
        )
        result = extract_module_data([Path("/fake/path/final_binding.f90")])
        module = result[0]
        
        # Check type with final binding
        resource_type = module["types"]["resource_holder"]
        
        # Check final binding - note that final bindings themselves don"t have binding_type,
        # but the procedures they reference can have binding_type
        has_final = any(proc["is_final"] for proc in resource_type["procedures"].values())
        self.assertTrue(has_final)
        
        # Check that the final subroutine has C binding
        cleanup = module["subroutines"]["cleanup"]
        self.assertIn("binding_type", cleanup)
        self.assertEqual(cleanup["binding_type"]["type"], BindingTypeEnum.BIND_C)
        self.assertEqual(cleanup["binding_type"]["name"], "c_resource_cleanup")
        
        # Check the regular finalizer without binding
        simple_cleanup = module["subroutines"]["simple_cleanup"]
        self.assertIn("binding_type", simple_cleanup)
        self.assertIsNone(simple_cleanup["binding_type"])


    def test_generic_interface_binding(self):
        self.fs.create_file(
            "/fake/path/generic_interface_binding.f90",
            contents="""\
    module generic_interface_binding_mod
        use iso_c_binding
        implicit none

        !!* Generic interface with C binding *!
        interface compute
            !!* C-bound implementation for integers *!
            function compute_int(x) bind(c, name="compute_int_c") result(y)
                use iso_c_binding
                integer(c_int), value :: x
                integer(c_int) :: y
            end function
            
            !!* C-bound implementation for reals *!
            function compute_real(x) bind(c, name="compute_real_c") result(y)
                use iso_c_binding
                real(c_double), value :: x
                real(c_double) :: y
            end function
        end interface
        
        !!* Generic interface with mixed binding *!
        interface mixed
            !!* C-bound implementation *!
            function mixed_c(x) bind(c, name="mixed_c_impl") result(y)
                use iso_c_binding
                real(c_double), value :: x
                real(c_double) :: y
            end function
            
            !!* Regular implementation without binding *!
            function mixed_f(x) result(y)
                use iso_c_binding
                real(c_double), intent(in) :: x
                real(c_double) :: y
            end function
        end interface
        
        !!* Regular generic interface without binding *!
        interface regular
            function regular_int(x) result(y)
                integer, intent(in) :: x
                integer :: y
            end function
            
            function regular_real(x) result(y)
                real, intent(in) :: x
                real :: y
            end function
        end interface

    contains
        
        !!* Implementation of C-bound compute for integers *!
        function compute_int(x) bind(c, name="compute_int_c") result(y)
            use iso_c_binding
            integer(c_int), value :: x
            integer(c_int) :: y
            y = x * x
        end function
        
        !!* Implementation of C-bound compute for reals *!
        function compute_real(x) bind(c, name="compute_real_c") result(y)
            use iso_c_binding
            real(c_double), value :: x
            real(c_double) :: y
            y = x * x
        end function
        
        !!* Implementation of C-bound mixed function *!
        function mixed_c(x) bind(c, name="mixed_c_impl") result(y)
            use iso_c_binding
            real(c_double), value :: x
            real(c_double) :: y
            y = x * x
        end function
        
        !!* Implementation of regular mixed function *!
        function mixed_f(x) result(y)
            use iso_c_binding
            real(c_double), intent(in) :: x
            real(c_double) :: y
            y = x * x
        end function
        
        !!* Implementation of regular functions *!
        function regular_int(x) result(y)
            integer, intent(in) :: x
            integer :: y
            y = x * x
        end function
        
        function regular_real(x) result(y)
            real, intent(in) :: x
            real :: y
            y = x * x
        end function

    end module generic_interface_binding_mod
    """
        )
        result = extract_module_data([Path("/fake/path/generic_interface_binding.f90")])
        module = result[0]
        
        # Check generic interfaces
        self.assertEqual(len(module["interfaces"]), 3)
        
        # Find each interface by name
        compute_interface = next(i for i in module["interfaces"] if i["name"] == "compute")
        mixed_interface = next(i for i in module["interfaces"] if i["name"] == "mixed")
        regular_interface = next(i for i in module["interfaces"] if i["name"] == "regular")
        
        # Check there is no binding type in the interface itself 
        self.assertNotIn("binding_type", compute_interface)        
        self.assertNotIn("binding_type", mixed_interface) 
        self.assertNotIn("binding_type", regular_interface)
        
        # Check individual procedures in the compute interface
        compute_int_proc = compute_interface["procedures"]["compute_int"]
        self.assertEqual(compute_int_proc["binding_type"]["type"], BindingTypeEnum.BIND_C)
        self.assertEqual(compute_int_proc["binding_type"]["name"], "compute_int_c")
        
        compute_real_proc = compute_interface["procedures"]["compute_real"]
        self.assertEqual(compute_real_proc["binding_type"]["type"], BindingTypeEnum.BIND_C)
        self.assertEqual(compute_real_proc["binding_type"]["name"], "compute_real_c")
        
        # Check mixed interface with both bound and unbound procedures
        mixed_c_proc = mixed_interface["procedures"]["mixed_c"]
        self.assertEqual(mixed_c_proc["binding_type"]["type"], BindingTypeEnum.BIND_C)
        self.assertEqual(mixed_c_proc["binding_type"]["name"], "mixed_c_impl")
        
        mixed_f_proc = mixed_interface["procedures"]["mixed_f"]
        self.assertIsNone(mixed_f_proc["binding_type"])
        
        # Check regular interface procedures have no binding
        regular_int_proc = regular_interface["procedures"]["regular_int"]
        self.assertIsNone(regular_int_proc["binding_type"])
        
        regular_real_proc = regular_interface["procedures"]["regular_real"]
        self.assertIsNone(regular_real_proc["binding_type"])
        
        # Check the module procedure implementations match their interface declarations
        compute_int_impl = module["functions"]["compute_int"]
        self.assertEqual(compute_int_impl["binding_type"]["type"], BindingTypeEnum.BIND_C)
        self.assertEqual(compute_int_impl["binding_type"]["name"], "compute_int_c")
        
        mixed_c_impl = module["functions"]["mixed_c"]
        self.assertEqual(mixed_c_impl["binding_type"]["type"], BindingTypeEnum.BIND_C)
        self.assertEqual(mixed_c_impl["binding_type"]["name"], "mixed_c_impl")
        
        mixed_f_impl = module["functions"]["mixed_f"]
        self.assertIsNone(mixed_f_impl["binding_type"])


if __name__ == "__main__":
    unittest.main()
