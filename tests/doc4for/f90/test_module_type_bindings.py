import unittest
from pathlib import Path
from pyfakefs.fake_filesystem_unittest import TestCase
from doc4for.f90.generate_module_tree import extract_module_data
from doc4for.models.common import BindingTypeEnum, BindingType

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
        procedure, bind(c), nopass :: calculate => calculate_shape_area
        
        !!* Regular method without binding *!
        procedure :: regular_method => regular_impl
    end type shape
    
    !!* Complex shape with named binding *!
    type :: complex_shape
    contains
        !!* Area calculation with custom name binding *!
        procedure, bind(c, name="complex_area_calc") :: area => complex_area
    end type complex_shape
    
    !!* Type with unusual binding syntax *!
    type :: weird_shape
    contains
        !!* Method with unusual binding syntax *!
        procedure, BIND  (  C  ) :: weird_method => weird_impl
    end type weird_shape
    
    !!* Type with binding syntax variations *!
    type :: binding_variations
    contains
        !!* Mixed case binding *!
        procedure, Bind(C), pass(self) :: method1 => impl1
        
        !!* Binding with other attributes *!
        procedure, public, bind(c) :: method2 => impl2
        
        !!* Double quotes in name *!
        procedure, bind(c, name="quoted_name") :: method3 => impl3
    end type binding_variations

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
        class(binding_variations), intent(in) :: self
        integer(c_int) :: res
        res = 1
    end function
    
    !!* Implementation with other attributes *!
    function impl2(this) bind(c) result(res)
        class(binding_variations), intent(in) :: this
        integer(c_int) :: res
        res = 2
    end function
    
    !!* Implementation with quoted name *!
    function impl3(this) bind(c, name="different_quoted_name") result(res)
        class(binding_variations), intent(in) :: this
        integer(c_int) :: res
        res = 3
    end function

end module type_bound_proc_binding_mod
"""
        )
        result = extract_module_data([Path("/fake/path/type_bound_proc_binding.f90")])
        module = result[0]
        
        # Check binding in basic type-bound procedure specification
        shape_type = module["types"]["shape"]
        
        # Check C-bound procedure
        calculate_proc = shape_type["procedures"]["calculate"]
        self.assertIn("binding_type", calculate_proc)
        binding_type = calculate_proc["binding_type"]
        self.assertEqual(binding_type["type"], BindingTypeEnum.BIND_C)
        self.assertIsNone(binding_type["name"])
        self.assertIn("nopass", calculate_proc["attributes"])
        
        # Check regular procedure without binding
        regular_proc = shape_type["procedures"]["regular_method"]
        self.assertIn("binding_type", regular_proc)
        binding_type = regular_proc["binding_type"]
        self.assertIsNone(binding_type)
        
        # Check binding with custom name
        complex_type = module["types"]["complex_shape"]
        area_proc = complex_type["procedures"]["area"]
        self.assertIn("binding_type", area_proc)
        binding_type = area_proc["binding_type"]
        self.assertEqual(binding_type["type"], BindingTypeEnum.BIND_C)
        self.assertEqual(binding_type["name"], "complex_area_calc")
        
        # Check unusual binding syntax
        weird_type = module["types"]["weird_shape"]
        weird_proc = weird_type["procedures"]["weird_method"]
        self.assertIn("binding_type", weird_proc)
        binding_type = weird_proc["binding_type"]
        self.assertEqual(binding_type["type"], BindingTypeEnum.BIND_C)
        self.assertIsNone(binding_type["name"])
        
        # Check binding variations
        var_type = module["types"]["binding_variations"]
        
        # Mixed case
        method1 = var_type["procedures"]["method1"]
        self.assertIn("binding_type", method1)
        binding_type = method1["binding_type"]
        self.assertEqual(binding_type["type"], BindingTypeEnum.BIND_C)
        self.assertIsNone(binding_type["name"])
        self.assertIn("pass(self)", method1["attributes"])
        
        # With other attributes
        method2 = var_type["procedures"]["method2"]
        self.assertIn("binding_type", method2)
        binding_type = method2["binding_type"]
        self.assertEqual(binding_type["type"], BindingTypeEnum.BIND_C)
        self.assertIsNone(binding_type["name"])
        self.assertIn("public", method2["attributes"])
        
        # Double quotes
        method3 = var_type["procedures"]["method3"]
        self.assertIn("binding_type", method3)
        binding_type = method3["binding_type"]
        self.assertEqual(binding_type["type"], BindingTypeEnum.BIND_C)
        self.assertEqual(binding_type["name"], "quoted_name")
        
        # Check function implementations
        
        # Implementation with binding
        calculate_impl = module["functions"]["calculate_shape_area"]
        self.assertIn("binding_type", calculate_impl)
        self.assertEqual(calculate_impl["binding_type"]["type"], BindingTypeEnum.BIND_C)
        self.assertEqual(calculate_impl["binding_type"]["name"], "c_shape_area")
        
        # Regular implementation
        regular_impl = module["functions"]["regular_impl"]
        self.assertIn("binding_type", regular_impl)
        self.assertIsNone(regular_impl["binding_type"])
        
        # Complex implementation with binding
        complex_impl = module["functions"]["complex_area"]
        self.assertIn("binding_type", complex_impl)
        self.assertEqual(complex_impl["binding_type"]["type"], BindingTypeEnum.BIND_C)
        self.assertIsNone(complex_impl["binding_type"]["name"])
        
        # Weird implementation
        weird_impl = module["subroutines"]["weird_impl"]
        self.assertIn("binding_type", weird_impl)
        self.assertEqual(weird_impl["binding_type"]["type"], BindingTypeEnum.BIND_C)
        self.assertIsNone(weird_impl["binding_type"]["name"])
        
        # Mixed case implementation
        impl1 = module["functions"]["impl1"]
        self.assertIn("binding_type", impl1)
        self.assertEqual(impl1["binding_type"]["type"], BindingTypeEnum.BIND_C)
        self.assertIsNone(impl1["binding_type"]["name"])
        
        # Implementation with other attributes
        impl2 = module["functions"]["impl2"]
        self.assertIn("binding_type", impl2)
        self.assertEqual(impl2["binding_type"]["type"], BindingTypeEnum.BIND_C)
        self.assertIsNone(impl2["binding_type"]["name"])
        
        # Implementation with quoted name
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
            !!* Generic add operation with C binding *!
            generic, bind(c, name="c_add") :: add => add_int, add_real
            
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
        function add_int(this, a, b) bind(c) result(res)
            class(numeric_ops), intent(in) :: this
            integer(c_int), value :: a, b
            integer(c_int) :: res
            res = a + b
        end function
        
        !!* Add reals with C binding *!
        function add_real(this, a, b) bind(c) result(res)
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
        
        # Check generic binding with C interoperability
        add_generic = num_ops["generic_interfaces"]["add"]
        self.assertIn("binding_type", add_generic)
        self.assertIsNone(add_generic["binding_type"]) # because fparser doesn"t handle it
        # self.assertEqual(add_generic["binding_type"]["type"], BindingTypeEnum.BIND_C)
        # self.assertEqual(add_generic["binding_type"]["name"], "c_add")
        self.assertEqual(add_generic["specific_procedures"], ["add_int", "add_real"])
        
        # Check generic binding without C interoperability
        subtract_generic = num_ops["generic_interfaces"]["subtract"]
        self.assertIn("binding_type", subtract_generic)
        self.assertIsNone(subtract_generic["binding_type"]) # see above
        # self.assertEqual(subtract_generic["binding_type"]["type"], BindingTypeEnum.DEFAULT)
        # self.assertIsNone(subtract_generic["binding_type"]["name"])
        self.assertEqual(subtract_generic["specific_procedures"], ["subtract_int", "subtract_real"])
        
        # Check specific procedures have correct binding
        add_int = module["functions"]["add_int"]
        self.assertEqual(add_int["binding_type"]["type"], BindingTypeEnum.BIND_C)
        
        subtract_int = module["functions"]["subtract_int"]
        self.assertEqual(subtract_int["binding_type"]["type"], BindingTypeEnum.DEFAULT)


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
