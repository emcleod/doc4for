import unittest
from pathlib import Path
from pyfakefs.fake_filesystem_unittest import TestCase
from doc4for.f90.generate_module_tree import extract_module_data
from doc4for.models.common import BindingTypeEnum
from doc4for.models.procedure_models import PassType

class TestDerivedTypeBindings(TestCase):

    def setUp(self):
        self.setUpPyfakefs()

    def test_procedures_with_binding(self):
        self.fs.create_file(
            "/fake/path/procedures_binding.f90",
            contents="""\
module procedures_binding_mod
    use iso_c_binding
    implicit none

    !!* C-compatible type (no bound procedures with binding) *!
    type, bind(c) :: c_type1
        real(c_double) :: value
    end type c_type1
    
    !!* Regular type with bound procedures *!
    type :: c_calculator
        real(c_double) :: last_result
    contains
        procedure :: add => calculator_add
    end type c_calculator
    
    !!* Another type to avoid binding conflicts *!
    type :: regular_calculator
        real :: last_result
    contains
        procedure :: multiply => regular_multiply
    end type regular_calculator

contains
    
    function calculator_add(this, a, b) bind(c) result(res)
        class(c_calculator), intent(inout) :: this
        real(c_double), value :: a, b
        real(c_double) :: res
        res = a + b
        this%last_result = res
    end function
    
    function regular_multiply(this, a, b) result(res)
        class(regular_calculator), intent(inout) :: this
        real, intent(in) :: a, b
        real :: res
        res = a * b
        this%last_result = res
    end function

end module procedures_binding_mod
"""
        )
        result = extract_module_data([Path("/fake/path/procedures_binding.f90")])
        module = result[0]
        
        # Check C-compatible type
        c_type = module["types"]["c_type1"]
        assert c_type["binding_type"] is not None
        self.assertEqual(c_type["binding_type"]["type"], BindingTypeEnum.BIND_C)
        self.assertIsNone(c_type["binding_type"]["name"])
        
        # Check type with bound procedure (type itself has no binding)
        calculator = module["types"]["c_calculator"]
        self.assertIsNone(calculator["binding_type"])
        
        # Check type-bound procedure (no binding on the procedure declaration)
        add_proc = calculator["procedures"]["add"]
        self.assertEqual(add_proc["name"], "add")
        self.assertEqual(add_proc["implementation"], "calculator_add")
        self.assertIsNone(add_proc.get("binding_type"))  # Type-bound procedures don't have binding_type
        
        # Check regular type has no binding
        regular_calc = module["types"]["regular_calculator"]
        self.assertIsNone(regular_calc["binding_type"])
        
        multiply_proc = regular_calc["procedures"]["multiply"]
        self.assertEqual(multiply_proc["implementation"], "regular_multiply")
        
        # Check implementation function has C binding
        calc_add = module["functions"]["calculator_add"] 
        assert calc_add["binding_type"] is not None
        self.assertEqual(calc_add["binding_type"]["type"], BindingTypeEnum.BIND_C)
        self.assertIsNone(calc_add["binding_type"]["name"])
        self.assertEqual(calc_add["attributes"], ["PUBLIC"])
        # Check function arguments
        self.assertEqual(calc_add["arguments"], ["this", "a", "b"])
        self.assertIn("this", calc_add["in"])
        self.assertIn("a", calc_add["in"])
        self.assertIn("b", calc_add["in"])
        self.assertEqual(calc_add["return"]["type"], "REAL")
        self.assertEqual(calc_add["return"]["kind"], "c_double")
        
        # Check regular function has no binding
        regular_mult = module["functions"]["regular_multiply"]
        self.assertIsNone(regular_mult["binding_type"])
        self.assertNotIn("bind", " ".join(regular_mult["attributes"]))
        # Check function arguments
        self.assertEqual(regular_mult["arguments"], ["this", "a", "b"])
        self.assertEqual(regular_mult["return"]["type"], "REAL")
        self.assertIsNone(regular_mult["return"]["kind"])  # No explicit kind

    def test_derived_type_binding_with_procedures(self):
        self.fs.create_file(
            "/fake/path/derived_type_with_procedures.f90",
            contents="""\
module derived_type_with_procedures_mod
    use iso_c_binding
    implicit none

    !!* C-compatible type with bound procedures *!
    type, bind(c) :: calculator_t
        real(c_double) :: last_result
    contains
        procedure :: add => calculator_add
        procedure :: multiply => calculator_multiply
    end type calculator_t
    
    !!* Regular type with bound procedures *!
    type :: regular_calculator
        real :: last_result
    contains
        procedure :: add => regular_add
        procedure :: multiply => regular_multiply
    end type regular_calculator

contains
    
    function calculator_add(this, a, b) bind(c) result(res)
        class(calculator_t), intent(inout) :: this
        real(c_double), value :: a, b
        real(c_double) :: res
        res = a + b
        this%last_result = res
    end function
    
    function calculator_multiply(this, a, b) result(res)
        class(calculator_t), intent(inout) :: this
        real(c_double), intent(in) :: a, b
        real(c_double) :: res
        res = a * b
        this%last_result = res
    end function
    
    function regular_add(this, a, b) result(res)
        class(regular_calculator), intent(inout) :: this
        real, intent(in) :: a, b
        real :: res
        res = a + b
        this%last_result = res
    end function
    
    function regular_multiply(this, a, b) result(res)
        class(regular_calculator), intent(inout) :: this
        real, intent(in) :: a, b
        real :: res
        res = a * b
        this%last_result = res
    end function

end module derived_type_with_procedures_mod
"""
        )
        result = extract_module_data([Path("/fake/path/derived_type_with_procedures.f90")])
        module = result[0]
        
        # Check C-compatible calculator type
        calculator = module["types"]["calculator_t"]
        assert calculator["binding_type"] is not None
        self.assertEqual(calculator["binding_type"]["type"], BindingTypeEnum.BIND_C)
        self.assertIsNone(calculator["binding_type"]["name"])  # No name support yet
        
        # Check type-bound procedures (no binding_type on procedure declarations)
        add_proc = calculator["procedures"]["add"]
        self.assertEqual(add_proc["name"], "add")
        self.assertEqual(add_proc["implementation"], "calculator_add")
        self.assertFalse(add_proc["is_final"])
        self.assertIsNone(add_proc["bound_to"])
        
        multiply_proc = calculator["procedures"]["multiply"]
        self.assertEqual(multiply_proc["name"], "multiply") 
        self.assertEqual(multiply_proc["implementation"], "calculator_multiply")
        
        # Check regular calculator type
        regular = module["types"]["regular_calculator"]
        self.assertIsNone(regular["binding_type"])
        
        # Check procedure implementations - calculator_add with C binding
        calculator_add = module["functions"]["calculator_add"]
        assert calculator_add["binding_type"] is not None
        self.assertEqual(calculator_add["binding_type"]["type"], BindingTypeEnum.BIND_C)
        self.assertIsNone(calculator_add["binding_type"]["name"])
        self.assertEqual(calculator_add["attributes"], ["PUBLIC"])
        # Check arguments with C types
        self.assertEqual(calculator_add["arguments"], ["this", "a", "b"])
        self.assertEqual(calculator_add["in"]["a"]["type"], "REAL")
        self.assertEqual(calculator_add["in"]["a"]["kind"], "c_double")
        self.assertIn("VALUE", calculator_add["in"]["a"]["attributes"])
        self.assertEqual(calculator_add["in"]["b"]["type"], "REAL")
        self.assertEqual(calculator_add["in"]["b"]["kind"], "c_double")
        self.assertIn("VALUE", calculator_add["in"]["b"]["attributes"])
        
        # Check calculator_multiply - no C binding
        calculator_multiply = module["functions"]["calculator_multiply"]
        self.assertIsNone(calculator_multiply["binding_type"])
        self.assertNotIn("bind", " ".join(calculator_multiply["attributes"]))
        # Still uses C types because of the parent type
        self.assertEqual(calculator_multiply["in"]["a"]["type"], "REAL")
        self.assertEqual(calculator_multiply["in"]["a"]["kind"], "c_double")
        
        # Check regular_add - no C binding
        regular_add = module["functions"]["regular_add"]
        self.assertIsNone(regular_add["binding_type"])
        self.assertEqual(regular_add["arguments"], ["this", "a", "b"])
        self.assertEqual(regular_add["in"]["a"]["type"], "REAL")
        self.assertIsNone(regular_add["in"]["a"]["kind"])  # No explicit kind
        
        # Check regular_multiply - no C binding
        regular_multiply = module["functions"]["regular_multiply"]
        self.assertIsNone(regular_multiply["binding_type"])
        self.assertEqual(regular_multiply["return"]["type"], "REAL")
    
    def test_derived_type_binding(self):
        self.fs.create_file(
            "/fake/path/derived_type_binding.f90",
            contents="""\
module derived_type_binding_mod
    use iso_c_binding
    implicit none

    !!* Basic type with C binding and default name *!
    type, bind(c) :: c_point_t
        real(c_double) :: x, y
    end type c_point_t
    
    !!* Type with C binding and explicit name *!
    type, bind(c) :: c_vector_t
        real(c_double) :: x, y, z
        integer(c_int) :: id
    end type c_vector_t
    
    !!* No binding type *!
    type :: regular_point
        real :: x, y
    end type regular_point
    
    !!* Type with mixed attributes *!
    type, public, bind(c) :: public_c_type
        integer(c_int) :: data
    end type public_c_type
    
    !!* Type with unusual spacing in binding *!
    type, bind  (  c  ) :: weird_type
        real(c_double) :: val
    end type weird_type

end module derived_type_binding_mod
"""
        )
        result = extract_module_data([Path("/fake/path/derived_type_binding.f90")])
        module = result[0]
        
        # Check basic type with C binding
        point = module["types"]["c_point_t"]
        assert point["binding_type"] is not None
        self.assertEqual(point["binding_type"]["type"], BindingTypeEnum.BIND_C)
        self.assertIsNone(point["binding_type"]["name"])
        
        # Check type with C binding (would have explicit name but fparser doesn't support it)
        vector = module["types"]["c_vector_t"]
        assert vector["binding_type"] is not None
        self.assertEqual(vector["binding_type"]["type"], BindingTypeEnum.BIND_C)
        self.assertIsNone(vector["binding_type"]["name"])  # Would be "c_vector_s" if supported
        
        # Check type without binding
        regular = module["types"]["regular_point"]
        self.assertIsNone(regular["binding_type"])
        
        # Check type with multiple attributes including bind(c)
        public_type = module["types"]["public_c_type"]
        assert public_type["binding_type"] is not None
        self.assertEqual(public_type["binding_type"]["type"], BindingTypeEnum.BIND_C)
        self.assertIsNone(public_type["binding_type"]["name"])
        self.assertIn("PUBLIC", public_type["attributes"])
        
        # Check type with unusual spacing (would have name if supported)
        weird = module["types"]["weird_type"]
        assert weird["binding_type"] is not None
        self.assertEqual(weird["binding_type"]["type"], BindingTypeEnum.BIND_C)
        self.assertIsNone(weird["binding_type"]["name"])  # Would be "weird_c_type" if supported

    def test_derived_type_binding_with_components(self):
        self.fs.create_file(
            "/fake/path/derived_type_components.f90",
            contents="""\
module derived_type_components_mod
    use iso_c_binding
    implicit none

    !!* C-compatible struct with mixed component types *!
    type, bind(c) :: complex_struct
        integer(c_int) :: id
        real(c_double) :: values(3)
        type(c_ptr) :: handle
    end type complex_struct
    
    !!* Nested C-compatible struct *!
    type, bind(c) :: parent_struct
        type(complex_struct) :: child
        integer(c_int) :: count
    end type parent_struct
    
    !!* Type with C binding containing arrays *!
    type, bind(c) :: array_container
        integer(c_int) :: sizes(10)
        real(c_double) :: matrix(3,3)
    end type array_container

contains

    !!* Function returning C-compatible type *!
    function create_complex_struct(id) bind(c) result(s)
        integer(c_int), value :: id
        type(complex_struct) :: s
        s%id = id
        s%values = 0.0_c_double
        s%handle = c_null_ptr
    end function create_complex_struct
    
    !!* Subroutine modifying C-compatible type *!
    subroutine modify_parent_struct(p) bind(c)
        type(parent_struct), intent(inout) :: p
        p%count = p%count + 1
        p%child%id = p%count
    end subroutine modify_parent_struct
    
    !!* Function without C binding working with C types *!
    function sum_array_container(ac) result(total)
        type(array_container), intent(in) :: ac
        real(c_double) :: total
        total = sum(ac%matrix) + sum(real(ac%sizes, c_double))
    end function sum_array_container

end module derived_type_components_mod
"""
        )
        result = extract_module_data([Path("/fake/path/derived_type_components.f90")])
        module = result[0]
        
        # Check complex struct with mixed components
        complex_struct = module["types"]["complex_struct"]
        assert complex_struct["binding_type"] is not None
        self.assertEqual(complex_struct["binding_type"]["type"], BindingTypeEnum.BIND_C)
        self.assertIsNone(complex_struct["binding_type"]["name"])
        self.assertEqual(len(complex_struct["data_components"]), 3)
        
        # Check nested struct
        parent = module["types"]["parent_struct"]
        self.assertIsNotNone(parent["binding_type"])
        assert parent["binding_type"] is not None
        self.assertEqual(parent["binding_type"]["type"], BindingTypeEnum.BIND_C)
        self.assertIsNone(parent["binding_type"]["name"])
        self.assertEqual(parent["data_components"]["child"]["type"], "complex_struct")
        
        # Check array container (would have name if supported)
        array_container = module["types"]["array_container"]
        assert array_container["binding_type"] is not None
        self.assertEqual(array_container["binding_type"]["type"], BindingTypeEnum.BIND_C)
        self.assertIsNone(array_container["binding_type"]["name"])  # Would be "c_array_container" if supported
        self.assertIn("dimension", array_container["data_components"]["sizes"])
        
        # Check function returning C-compatible type with C binding
        create_func = module["functions"]["create_complex_struct"]
        assert create_func["binding_type"] is not None
        self.assertEqual(create_func["binding_type"]["type"], BindingTypeEnum.BIND_C)
        self.assertIsNone(create_func["binding_type"]["name"])
        self.assertEqual(create_func["return"]["type"], "complex_struct")
        self.assertEqual(create_func["in"]["id"]["type"], "INTEGER")
        self.assertEqual(create_func["in"]["id"]["kind"], "c_int")
        self.assertIn("VALUE", create_func["in"]["id"]["attributes"])
        
        # Check subroutine with C binding
        modify_sub = module["subroutines"]["modify_parent_struct"]
        assert modify_sub["binding_type"] is not None
        self.assertEqual(modify_sub["binding_type"]["type"], BindingTypeEnum.BIND_C)
        self.assertIsNone(modify_sub["binding_type"]["name"])
        self.assertIn("p", modify_sub["in"])
        self.assertIn("p", modify_sub["out"])  # intent(inout)
        self.assertEqual(modify_sub["in"]["p"]["type"], "parent_struct")
        
        # Check function without C binding but using C types
        sum_func = module["functions"]["sum_array_container"]
        self.assertIsNone(sum_func["binding_type"])
        self.assertEqual(sum_func["return"]["type"], "REAL")
        self.assertEqual(sum_func["return"]["kind"], "c_double")
        self.assertEqual(sum_func["in"]["ac"]["type"], "array_container")

    def test_type_bound_procedure_access_specifiers(self):
        self.fs.create_file(
            "/fake/path/access_specifiers.f90",
            contents="""\
    module access_specifiers_mod
        implicit none

        !!* Type with various access specifiers on procedures *!
        type :: access_test_type
            private
            real :: internal_data
            integer, public :: public_data
        contains
            !!* Public method with default pass *!
            procedure, public :: public_method
            
            !!* Private method with implementation *!
            procedure, private :: private_method => private_impl
            
            !!* Public method with explicit pass *!
            procedure, public, pass :: public_with_pass
            
            !!* Private method with nopass *!
            procedure, private, nopass :: private_nopass => static_private_impl
            
            !!* Public method with pass to specific argument *!
            procedure, public, pass(this) :: public_pass_explicit
            
            !!* Method with multiple attributes *!
            procedure, public, nopass :: public_static => public_static_impl
            
            !!* Final procedure (always private) *!
            final :: cleanup
            
            !!* Generic interface with access specifier *!
            generic, public :: write(formatted) => write_formatted
            generic, public :: operator(+) => add_types
            generic, private :: process => process_real, process_integer
            
            !!* Private specific procedures for generics *!
            procedure, private :: write_formatted
            procedure, private :: add_types
            procedure, private :: process_real
            procedure, private :: process_integer
        end type access_test_type
        
        !!* Type with default private and selective public *!
        type :: selective_access_type
            real :: x, y
        contains
            private  ! Default access for all following
            
            !!* These are private by default *!
            procedure :: internal_method1
            procedure :: internal_method2
            
            !!* Explicitly public methods *!
            procedure, public :: get_x
            procedure, public :: set_x
            procedure, public :: compute => compute_impl
            
            !!* Generic with mixed access *!
            generic, public :: assignment(=) => assign_from_real, assign_from_type
            procedure :: assign_from_real  ! Private (default)
            procedure :: assign_from_type   ! Private (default)
        end type selective_access_type
        
        !!* Type with procedure pointer components *!
        type :: proc_pointer_type
            procedure(interface_func), pointer, public :: public_ptr => null()
            procedure(interface_func), pointer, private :: private_ptr => null()
            procedure(interface_sub), pointer, public, nopass :: static_ptr => null()
        contains
            procedure, public :: set_private_ptr
            procedure, public :: call_private_ptr
        end type proc_pointer_type

        !!* Abstract interfaces *!
        abstract interface
            function interface_func(x) result(y)
                real, intent(in) :: x
                real :: y
            end function interface_func
            
            subroutine interface_sub(data)
                real, intent(inout) :: data(:)
            end subroutine interface_sub
        end interface

    contains

        !!* Implementations *!
        
        subroutine public_method(this)
            class(access_test_type), intent(inout) :: this
            ! Implementation
        end subroutine public_method
        
        subroutine private_impl(this)
            class(access_test_type), intent(inout) :: this
            ! Implementation
        end subroutine private_impl
        
        subroutine public_with_pass(this)
            class(access_test_type), intent(inout) :: this
            ! Implementation
        end subroutine public_with_pass
        
        subroutine static_private_impl()
            ! No 'this' argument
        end subroutine static_private_impl
        
        subroutine public_pass_explicit(this)
            class(access_test_type), intent(inout) :: this
            ! Implementation
        end subroutine public_pass_explicit
        
        function public_static_impl() result(res)
            logical :: res
            res = .true.
        end function public_static_impl
        
        subroutine cleanup(this)
            type(access_test_type), intent(inout) :: this
            ! Finalization
        end subroutine cleanup
        
        subroutine write_formatted(dtv, unit, iotype, v_list, iostat, iomsg)
            class(access_test_type), intent(in) :: dtv
            integer, intent(in) :: unit
            character(*), intent(in) :: iotype
            integer, intent(in) :: v_list(:)
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
            ! Implementation
        end subroutine write_formatted
        
        function add_types(this, other) result(res)
            class(access_test_type), intent(in) :: this, other
            type(access_test_type) :: res
            ! Implementation
        end function add_types
        
        subroutine process_real(this, x)
            class(access_test_type), intent(inout) :: this
            real, intent(in) :: x
            ! Implementation
        end subroutine process_real
        
        subroutine process_integer(this, i)
            class(access_test_type), intent(inout) :: this
            integer, intent(in) :: i
            ! Implementation
        end subroutine process_integer
        
        ! Selective access type implementations
        
        subroutine internal_method1(this)
            class(selective_access_type), intent(inout) :: this
            ! Private implementation
        end subroutine internal_method1
        
        subroutine internal_method2(this)
            class(selective_access_type), intent(inout) :: this
            ! Private implementation
        end subroutine internal_method2
        
        function get_x(this) result(x)
            class(selective_access_type), intent(in) :: this
            real :: x
            x = this%x
        end function get_x
        
        subroutine set_x(this, x)
            class(selective_access_type), intent(inout) :: this
            real, intent(in) :: x
            this%x = x
        end subroutine set_x
        
        subroutine compute_impl(this)
            class(selective_access_type), intent(inout) :: this
            ! Implementation
        end subroutine compute_impl
        
        subroutine assign_from_real(this, r)
            class(selective_access_type), intent(inout) :: this
            real, intent(in) :: r
            this%x = r
            this%y = r
        end subroutine assign_from_real
        
        subroutine assign_from_type(this, other)
            class(selective_access_type), intent(inout) :: this
            type(selective_access_type), intent(in) :: other
            this%x = other%x
            this%y = other%y
        end subroutine assign_from_type
        
        ! Procedure pointer type implementations
        
        subroutine set_private_ptr(this, func)
            class(proc_pointer_type), intent(inout) :: this
            procedure(interface_func) :: func
            this%private_ptr => func
        end subroutine set_private_ptr
        
        function call_private_ptr(this, x) result(y)
            class(proc_pointer_type), intent(in) :: this
            real, intent(in) :: x
            real :: y
            if (associated(this%private_ptr)) then
                y = this%private_ptr(x)
            else
                y = 0.0
            end if
        end function call_private_ptr

    end module access_specifiers_mod
    """
        )
        
        result = extract_module_data([Path("/fake/path/access_specifiers.f90")], False)
        module = result[0]
        
        # Test access_test_type
        access_type = module["types"]["access_test_type"]
        
        # Check type", acce has private components by default
        self.assertEqual(access_type["attributes"], ["PUBLIC"])
        
        # Check data components
        self.assertEqual(access_type["data_components"]["internal_data"]["attributes"], ["PRIVATE"])
        self.assertEqual(access_type["data_components"]["public_data"]["attributes"], ["PUBLIC"])
        
        # Check procedure access specifiers
        public_method = access_type["procedures"]["public_method"]
        self.assertIn("PUBLIC", public_method["attributes"])
        self.assertEqual(public_method["pass_type"], PassType.DEFAULT)
        self.assertIsNone(public_method["implementation"])
        
        private_method = access_type["procedures"]["private_method"]
        self.assertIn("PRIVATE", private_method["attributes"])
        self.assertEqual(private_method["implementation"], "private_impl")
        
        public_with_pass = access_type["procedures"]["public_with_pass"]
        self.assertIn("PUBLIC", public_with_pass["attributes"])
        self.assertEqual(public_with_pass["pass_type"], PassType.DEFAULT)
        
        private_nopass = access_type["procedures"]["private_nopass"]
        self.assertIn("PRIVATE", private_nopass["attributes"])
        self.assertEqual(private_nopass["pass_type"], PassType.NONE)
        self.assertEqual(private_nopass["implementation"], "static_private_impl")
        
        public_pass_explicit = access_type["procedures"]["public_pass_explicit"]
        self.assertIn("PUBLIC", public_pass_explicit["attributes"])
        self.assertEqual(public_pass_explicit["pass_type"], PassType.NAMED)
        self.assertEqual(public_pass_explicit["pass_name"], "this")
        
        public_static = access_type["procedures"]["public_static"]
        self.assertIn("PUBLIC", public_static["attributes"])
        self.assertEqual(public_static["pass_type"], PassType.NONE)
        self.assertEqual(public_static["implementation"], "public_static_impl")
        
        # Check final procedure
        cleanup = access_type["procedures"]["cleanup"]
        self.assertTrue(cleanup["is_final"])
        # Final procedures don't have explicit access specifiers
        
        # Check generic interfaces
        write_generic = access_type["generic_interfaces"]["write(formatted)"]
        self.assertIn("PUBLIC", write_generic["attributes"])
        self.assertIn("write_formatted", write_generic["specific_procedures"])
        
        plus_generic = access_type["generic_interfaces"]["operator(+)"]
        self.assertIn("PUBLIC", plus_generic["attributes"])
        self.assertIn("add_types", plus_generic["specific_procedures"])
        
        process_generic = access_type["generic_interfaces"]["process"]
        self.assertIn("PRIVATE", process_generic["attributes"])
        self.assertIn("process_real", process_generic["specific_procedures"])
        self.assertIn("process_integer", process_generic["specific_procedures"])
        
        # Check specific procedures for generics
        write_formatted = access_type["procedures"]["write_formatted"]
        self.assertIn("PRIVATE", write_formatted["attributes"])
        
        # Test selective_access_type
        selective_type = module["types"]["selective_access_type"]
        
        # Check default private procedures
        internal1 = selective_type["procedures"]["internal_method1"]
        # But they should not have 'public'
        self.assertNotIn("PUBLIC", internal1["attributes"])
        
        internal2 = selective_type["procedures"]["internal_method2"]
        self.assertNotIn("PUBLIC", internal2["attributes"])
        
        # Check explicitly public methods
        get_x = selective_type["procedures"]["get_x"]
        self.assertIn("PUBLIC", get_x["attributes"])
        
        set_x = selective_type["procedures"]["set_x"]
        self.assertIn("PUBLIC", set_x["attributes"])
        
        compute = selective_type["procedures"]["compute"]
        self.assertIn("PUBLIC", compute["attributes"])
        self.assertEqual(compute["implementation"], "compute_impl")
        
        # Check generic assignment
        assignment = selective_type["generic_interfaces"]["assignment(=)"]
        self.assertIn("PUBLIC", assignment["attributes"])
        self.assertIn("assign_from_real", assignment["specific_procedures"])
        self.assertIn("assign_from_type", assignment["specific_procedures"])
        
        # Test proc_pointer_type
        proc_type = module["types"]["proc_pointer_type"]
        
        # Check procedure pointer components
        public_ptr = proc_type["procedures"]["public_ptr"]
        self.assertIn("PUBLIC", public_ptr["attributes"])
        self.assertIn("POINTER", public_ptr["attributes"])
        self.assertEqual(public_ptr["bound_to"], "interface_func")
        
        private_ptr = proc_type["procedures"]["private_ptr"]
        self.assertIn("PRIVATE", private_ptr["attributes"])
        self.assertIn("POINTER", private_ptr["attributes"])
        self.assertEqual(private_ptr["bound_to"], "interface_func")
        
        static_ptr = proc_type["procedures"]["static_ptr"]
        self.assertIn("PUBLIC", static_ptr["attributes"])
        self.assertIn("POINTER", static_ptr["attributes"])
        self.assertEqual(static_ptr["pass_type"], PassType.NONE)
        self.assertEqual(static_ptr["bound_to"], "interface_sub")
        
        # Check methods
        set_private = proc_type["procedures"]["set_private_ptr"]
        self.assertIn("PUBLIC", set_private["attributes"])
        
        call_private = proc_type["procedures"]["call_private_ptr"]
        self.assertIn("PUBLIC", call_private["attributes"])

    def test_type_bound_procedure_access_switching(self):
        self.fs.create_file(
            "/fake/path/access_switching.f90",
            contents="""\
    module access_switching_mod
        implicit none
        
        type :: access_switch_type
            integer :: data
        contains
            ! Start with default (PUBLIC)
            procedure :: default_public_1
            procedure :: default_public_2
            generic :: operator(+) => default_public_1  ! Should be public
            
            private  ! Switch to private - all following are private by default
            
            procedure :: now_private_1
            procedure :: now_private_2
            procedure, public :: explicit_public_override  ! Override with explicit public
            generic :: assignment(=) => now_private_1  ! Private generic
            generic, public :: write(formatted) => write_impl  ! Explicit public generic
            procedure :: write_impl  ! Should be private (current default)
            procedure, public :: another_public_override
            procedure :: still_private
            final :: cleanup  ! Final procedures don't have explicit access
        end type access_switch_type
        
        ! Type with private components but changing procedure access
        type :: mixed_access_type
            private  ! Components are private
            real :: hidden_x, hidden_y
            
        contains
            ! No initial statement, so procedures default to PUBLIC
            procedure :: starts_public_1
            procedure :: starts_public_2
            
            private  ! Now switch to private
            
            procedure :: becomes_private
            generic :: assignment(=) => becomes_private  ! Private generic
            procedure, public :: public_in_private_section
            procedure :: stays_private
            procedure, public :: another_public_override
            procedure :: ends_private
        end type mixed_access_type
        
        ! Type that starts with private procedures
        type :: private_first_type
        contains
            private  ! Start with private
            
            procedure :: private_from_start
            procedure, public :: public_override_1
            procedure :: still_private
            procedure, public :: public_override_2
            generic, public :: operator(*) => public_override_1
            generic :: operator(-) => still_private  ! Private generic
        end type private_first_type

    contains
        
        ! Implementation stubs
        subroutine default_public_1(this)
            class(access_switch_type) :: this
        end subroutine
        
        subroutine default_public_2(this)
            class(access_switch_type) :: this
        end subroutine
        
        subroutine now_private_1(this, other)
            class(access_switch_type), intent(out) :: this
            class(access_switch_type), intent(in) :: other
        end subroutine
        
        subroutine now_private_2(this)
            class(access_switch_type) :: this
        end subroutine
        
        subroutine explicit_public_override(this)
            class(access_switch_type) :: this
        end subroutine
        
        subroutine write_impl(dtv, unit, iotype, v_list, iostat, iomsg)
            class(access_switch_type), intent(in) :: dtv
            integer, intent(in) :: unit
            character(*), intent(in) :: iotype
            integer, intent(in) :: v_list(:)
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
        
        subroutine another_public_override(this)
            class(access_switch_type) :: this
        end subroutine
        
        subroutine still_private(this)
            class(access_switch_type) :: this
        end subroutine
        
        subroutine cleanup(this)
            type(access_switch_type) :: this
        end subroutine
        
        ! Mixed access type implementations
        subroutine starts_public_1(this)
            class(mixed_access_type) :: this
        end subroutine
        
        subroutine starts_public_2(this)
            class(mixed_access_type) :: this
        end subroutine
        
        subroutine becomes_private(this, other)
            class(mixed_access_type), intent(out) :: this
            class(mixed_access_type), intent(in) :: other
        end subroutine
        
        subroutine public_in_private_section(this)
            class(mixed_access_type) :: this
        end subroutine
        
        subroutine stays_private(this)
            class(mixed_access_type) :: this
        end subroutine
        
        subroutine another_public_override(this)
            class(mixed_access_type) :: this
        end subroutine
        
        subroutine ends_private(this)
            class(mixed_access_type) :: this
        end subroutine
        
        ! Private first type implementations
        subroutine private_from_start(this)
            class(private_first_type) :: this
        end subroutine
        
        function public_override_1(this, other) result(res)
            class(private_first_type), intent(in) :: this
            class(private_first_type), intent(in) :: other
            type(private_first_type) :: res
        end function
        
        subroutine still_private(this, other)
            class(private_first_type), intent(out) :: this
            class(private_first_type), intent(in) :: other
        end subroutine
        
        subroutine public_override_2(this)
            class(private_first_type) :: this
        end subroutine

    end module access_switching_mod
    """
        )
        
        result = extract_module_data([Path("/fake/path/access_switching.f90")], False)
        module = result[0]
        
        # Test access_switch_type
        switch_type = module["types"]["access_switch_type"]
        
        # Check procedures with default PUBLIC access
        self.assertIn("PUBLIC", switch_type["procedures"]["default_public_1"]["attributes"])
        self.assertIn("PUBLIC", switch_type["procedures"]["default_public_2"]["attributes"])
        self.assertIn("PUBLIC", switch_type["generic_interfaces"]["operator(+)"]["attributes"])
        
        # After 'private' statement
        self.assertIn("PRIVATE", switch_type["procedures"]["now_private_1"]["attributes"])
        self.assertIn("PRIVATE", switch_type["procedures"]["now_private_2"]["attributes"])
        self.assertIn("PUBLIC", switch_type["procedures"]["explicit_public_override"]["attributes"])
        self.assertIn("PRIVATE", switch_type["generic_interfaces"]["assignment(=)"]["attributes"])
        self.assertIn("PUBLIC", switch_type["generic_interfaces"]["write(formatted)"]["attributes"])
        self.assertIn("PRIVATE", switch_type["procedures"]["write_impl"]["attributes"])
        self.assertIn("PUBLIC", switch_type["procedures"]["another_public_override"]["attributes"])
        self.assertIn("PRIVATE", switch_type["procedures"]["still_private"]["attributes"])
        
        # Final procedure (no explicit access)
        self.assertTrue(switch_type["procedures"]["cleanup"]["is_final"])
        
        # Test mixed_access_type
        mixed_type = module["types"]["mixed_access_type"]
        
        # Components should be private
        self.assertIn("PRIVATE", mixed_type["data_components"]["hidden_x"]["attributes"])
        self.assertIn("PRIVATE", mixed_type["data_components"]["hidden_y"]["attributes"])
        
        # Procedures start with PUBLIC default
        self.assertIn("PUBLIC", mixed_type["procedures"]["starts_public_1"]["attributes"])
        self.assertIn("PUBLIC", mixed_type["procedures"]["starts_public_2"]["attributes"])
        
        # After 'private' statement
        self.assertIn("PRIVATE", mixed_type["procedures"]["becomes_private"]["attributes"])
        self.assertIn("PRIVATE", mixed_type["generic_interfaces"]["assignment(=)"]["attributes"])
        self.assertIn("PUBLIC", mixed_type["procedures"]["public_in_private_section"]["attributes"])
        self.assertIn("PRIVATE", mixed_type["procedures"]["stays_private"]["attributes"])
        self.assertIn("PUBLIC", mixed_type["procedures"]["another_public_override"]["attributes"])
        self.assertIn("PRIVATE", mixed_type["procedures"]["ends_private"]["attributes"])
        
        # Test private_first_type
        private_type = module["types"]["private_first_type"]
        
        # All start private, some explicitly overridden to public
        self.assertIn("PRIVATE", private_type["procedures"]["private_from_start"]["attributes"])
        self.assertIn("PUBLIC", private_type["procedures"]["public_override_1"]["attributes"])
        self.assertIn("PRIVATE", private_type["procedures"]["still_private"]["attributes"])
        self.assertIn("PUBLIC", private_type["procedures"]["public_override_2"]["attributes"])
        self.assertIn("PUBLIC", private_type["generic_interfaces"]["operator(*)"]["attributes"])
        self.assertIn("PRIVATE", private_type["generic_interfaces"]["operator(-)"]["attributes"])
            
    def test_procedure_pointer_component_access(self):
        self.fs.create_file(
            "/fake/path/proc_pointer_access.f90",
            contents="""\
    module proc_pointer_access_mod
        implicit none
        
        ! Define some interfaces
        abstract interface
            function func_interface(x) result(y)
                real, intent(in) :: x
                real :: y
            end function func_interface
            
            subroutine sub_interface(a, b)
                integer, intent(in) :: a
                integer, intent(out) :: b
            end subroutine sub_interface
        end interface
        
        ! Type with private components including procedure pointers
        type :: private_component_type
            private
            real :: data
            procedure(func_interface), pointer :: func_ptr  ! Should be PRIVATE
            procedure(sub_interface), pointer :: sub_ptr    ! Should be PRIVATE
            procedure(func_interface), pointer, public :: public_func_ptr  ! Explicitly PUBLIC
        contains
            ! No private statement here, so procedures default to PUBLIC
            procedure :: set_func_ptr
            procedure :: call_func_ptr
            procedure, private :: internal_method  ! Explicitly PRIVATE
        end type private_component_type
        
        ! Type with public components but private procedures
        type :: mixed_access_type
            ! No private statement, so components are PUBLIC by default
            real :: data
            procedure(func_interface), pointer :: func_ptr  ! Should be PUBLIC
            procedure(sub_interface), pointer, private :: private_sub_ptr  ! Explicitly PRIVATE
        contains
            private  ! Makes following procedures PRIVATE by default
            procedure :: method1
            procedure :: method2
            procedure, public :: public_method  ! Explicitly PUBLIC
        end type mixed_access_type
        
        ! Type with all private
        type :: all_private_type
            private
            procedure(func_interface), pointer :: comp_ptr  ! PRIVATE from component section
            procedure(func_interface), pointer, nopass :: static_ptr  ! PRIVATE, with NOPASS
        contains
            private
            procedure :: proc1  ! PRIVATE from procedure section
            procedure, nopass :: static_proc  ! PRIVATE, with NOPASS
        end type all_private_type

    contains
        
        ! Implementation for private_component_type
        subroutine set_func_ptr(this, fptr)
            class(private_component_type), intent(inout) :: this
            procedure(func_interface) :: fptr
            this%func_ptr => fptr
        end subroutine set_func_ptr
        
        function call_func_ptr(this, x) result(y)
            class(private_component_type), intent(in) :: this
            real, intent(in) :: x
            real :: y
            if (associated(this%func_ptr)) then
                y = this%func_ptr(x)
            else
                y = 0.0
            end if
        end function call_func_ptr
        
        subroutine internal_method(this)
            class(private_component_type), intent(inout) :: this
            ! Internal processing
        end subroutine internal_method
        
        ! Implementation for mixed_access_type
        subroutine method1(this)
            class(mixed_access_type), intent(inout) :: this
            ! Implementation
        end subroutine method1
        
        subroutine method2(this)
            class(mixed_access_type), intent(inout) :: this
            ! Implementation
        end subroutine method2
        
        subroutine public_method(this)
            class(mixed_access_type), intent(inout) :: this
            ! Implementation
        end subroutine public_method
        
        ! Implementation for all_private_type
        subroutine proc1(this)
            class(all_private_type), intent(inout) :: this
            ! Implementation
        end subroutine proc1
        
        subroutine static_proc()
            ! No this argument
        end subroutine static_proc

    end module proc_pointer_access_mod
    """
        )
        
        result = extract_module_data([Path("/fake/path/proc_pointer_access.f90")], False)
        module = result[0]
        
        # Test private_component_type
        private_comp_type = module["types"]["private_component_type"]
        
        # Check procedure pointer components - should follow component access (PRIVATE)
        self.assertIn("func_ptr", private_comp_type["procedures"])
        func_ptr = private_comp_type["procedures"]["func_ptr"]
        self.assertEqual(func_ptr["attributes"], ["POINTER", "PRIVATE"])
        self.assertEqual(func_ptr["bound_to"], "func_interface")
        self.assertEqual(func_ptr["pass_type"], PassType.DEFAULT)
        
        self.assertIn("sub_ptr", private_comp_type["procedures"])
        sub_ptr = private_comp_type["procedures"]["sub_ptr"]
        self.assertEqual(sub_ptr["attributes"], ["POINTER", "PRIVATE"])
        self.assertEqual(sub_ptr["bound_to"], "sub_interface")
        
        self.assertIn("public_func_ptr", private_comp_type["procedures"])
        public_func_ptr = private_comp_type["procedures"]["public_func_ptr"]
        self.assertIn("PUBLIC", public_func_ptr["attributes"])
        self.assertIn("POINTER", public_func_ptr["attributes"])
        
        # Check type-bound procedures - should be PUBLIC (no private in contains)
        self.assertIn("set_func_ptr", private_comp_type["procedures"])
        self.assertEqual(private_comp_type["procedures"]["set_func_ptr"]["attributes"], ["PUBLIC"])
        
        self.assertIn("call_func_ptr", private_comp_type["procedures"])
        self.assertEqual(private_comp_type["procedures"]["call_func_ptr"]["attributes"], ["PUBLIC"])
        
        self.assertIn("internal_method", private_comp_type["procedures"])
        self.assertEqual(private_comp_type["procedures"]["internal_method"]["attributes"], ["PRIVATE"])
        
        # Test mixed_access_type
        mixed_type = module["types"]["mixed_access_type"]
        
        # Check procedure pointer components - should follow component access (PUBLIC by default)
        self.assertIn("func_ptr", mixed_type["procedures"])
        self.assertEqual(mixed_type["procedures"]["func_ptr"]["attributes"], ["POINTER", "PUBLIC"])
        
        self.assertIn("private_sub_ptr", mixed_type["procedures"])
        private_sub = mixed_type["procedures"]["private_sub_ptr"]
        self.assertIn("PRIVATE", private_sub["attributes"])
        self.assertIn("POINTER", private_sub["attributes"])
        
        # Check type-bound procedures - should be PRIVATE (private in contains)
        self.assertEqual(mixed_type["procedures"]["method1"]["attributes"], ["PRIVATE"])
        self.assertEqual(mixed_type["procedures"]["method2"]["attributes"], ["PRIVATE"])
        self.assertEqual(mixed_type["procedures"]["public_method"]["attributes"], ["PUBLIC"])
        
        # Test all_private_type
        all_private_type = module["types"]["all_private_type"]
        
        # Procedure pointer components with NOPASS
        comp_ptr = all_private_type["procedures"]["comp_ptr"]
        self.assertEqual(comp_ptr["attributes"], ["POINTER", "PRIVATE"])
        self.assertEqual(comp_ptr["pass_type"], PassType.DEFAULT)
        
        static_ptr = all_private_type["procedures"]["static_ptr"]
        self.assertEqual(static_ptr["attributes"], ["POINTER", "PRIVATE"])
        self.assertEqual(static_ptr["pass_type"], PassType.NONE)  # NOPASS
        
        # Type-bound procedures
        self.assertEqual(all_private_type["procedures"]["proc1"]["attributes"], ["PRIVATE"])
        self.assertEqual(all_private_type["procedures"]["static_proc"]["attributes"], ["PRIVATE"])
        self.assertEqual(all_private_type["procedures"]["static_proc"]["pass_type"], PassType.NONE)
            
    def test_procedure_pointer_edge_cases(self):
        self.fs.create_file(
            "/fake/path/proc_pointer_edge_cases.f90",
            contents="""\
    module proc_pointer_edge_cases_mod
        implicit none
        
        abstract interface
            function scalar_func(x) result(y)
                real, intent(in) :: x
                real :: y
            end function scalar_func
            
            subroutine array_sub(arr)
                real, dimension(:), intent(inout) :: arr
            end subroutine array_sub
            
            function no_arg_func() result(val)
                integer :: val
            end function no_arg_func
        end interface
        
        type :: edge_case_type
            private
            
            ! Multiple procedure pointers in one declaration
            procedure(scalar_func), pointer :: func1, func2, func3
            
            ! Procedure pointer with null initialization
            procedure(scalar_func), pointer :: null_func => null()
            
            ! Multiple with different attributes
            procedure(array_sub), pointer, public :: pub_sub1, pub_sub2
            
            ! Procedure pointer with nopass
            procedure(no_arg_func), pointer, nopass :: static_func => null()
            
            ! Mix of public and private in separate declarations
            procedure(scalar_func), pointer, public :: public_ptr
            procedure(scalar_func), pointer :: private_ptr  ! Should be PRIVATE (default)
            
        contains
            procedure :: init_pointers
            procedure :: test_pointers
        end type edge_case_type
        
        ! Type with procedure pointers but no private statement
        type :: default_access_type
            ! No private statement, so components are PUBLIC by default
            procedure(scalar_func), pointer :: default_ptr1, default_ptr2
            procedure(array_sub), pointer, private :: explicit_private_ptr
            procedure(no_arg_func), pointer, nopass :: default_static => null()
        contains
            private  ! Only affects procedures, not components
            procedure :: method1
            procedure, public :: public_method
        end type default_access_type
        
        ! Type testing initialization edge cases
        type :: init_edge_cases
            procedure(scalar_func), pointer :: uninitialized_ptr
            procedure(scalar_func), pointer :: null_init_ptr => null()
            procedure(scalar_func), pointer, private :: private_null_ptr => null()
            ! Multiple with mixed initialization
            procedure(no_arg_func), pointer :: ptr_a => null(), ptr_b, ptr_c => null()
        end type init_edge_cases

    contains
        
        subroutine init_pointers(this)
            class(edge_case_type), intent(inout) :: this
            ! Initialize pointers
        end subroutine init_pointers
        
        subroutine test_pointers(this)
            class(edge_case_type), intent(in) :: this
            ! Test pointer usage
        end subroutine test_pointers
        
        subroutine method1(this)
            class(default_access_type), intent(inout) :: this
            ! Implementation
        end subroutine method1
        
        subroutine public_method(this)
            class(default_access_type), intent(inout) :: this
            ! Implementation
        end subroutine public_method

    end module proc_pointer_edge_cases_mod
    """
        )
        
        result = extract_module_data([Path("/fake/path/proc_pointer_edge_cases.f90")], False)
        module = result[0]
        
        # Test edge_case_type
        edge_type = module["types"]["edge_case_type"]
        
        # Test multiple procedure pointers in one declaration
        self.assertIn("func1", edge_type["procedures"])
        self.assertIn("func2", edge_type["procedures"])
        self.assertIn("func3", edge_type["procedures"])
        
        # All should have same attributes and interface
        for func_name in ["func1", "func2", "func3"]:
            func = edge_type["procedures"][func_name]
            self.assertEqual(func["attributes"], ["POINTER", "PRIVATE"])
            self.assertEqual(func["bound_to"], "scalar_func")
            self.assertEqual(func["pass_type"], PassType.DEFAULT)
        
        # Test null initialization
        self.assertIn("null_func", edge_type["procedures"])
        null_func = edge_type["procedures"]["null_func"]
        self.assertEqual(null_func["attributes"], ["POINTER", "PRIVATE"])
        self.assertEqual(null_func["bound_to"], "scalar_func")
        # Note: We might want to track initialization in the future
        
        # Test multiple public pointers
        self.assertIn("pub_sub1", edge_type["procedures"])
        self.assertIn("pub_sub2", edge_type["procedures"])
        for pub_name in ["pub_sub1", "pub_sub2"]:
            pub_sub = edge_type["procedures"][pub_name]
            self.assertIn("PUBLIC", pub_sub["attributes"])
            self.assertIn("POINTER", pub_sub["attributes"])
            self.assertEqual(pub_sub["bound_to"], "array_sub")
        
        # Test nopass with null init
        self.assertIn("static_func", edge_type["procedures"])
        static_func = edge_type["procedures"]["static_func"]
        self.assertEqual(static_func["pass_type"], PassType.NONE)
        self.assertIn("POINTER", static_func["attributes"])
        self.assertIn("PRIVATE", static_func["attributes"])
        
        # Test mixed access
        self.assertIn("public_ptr", edge_type["procedures"])
        self.assertIn("PUBLIC", edge_type["procedures"]["public_ptr"]["attributes"])
        
        self.assertIn("private_ptr", edge_type["procedures"])
        self.assertIn("PRIVATE", edge_type["procedures"]["private_ptr"]["attributes"])
        
        # Test type-bound procedures
        self.assertEqual(edge_type["procedures"]["init_pointers"]["attributes"], ["PUBLIC"])
        self.assertEqual(edge_type["procedures"]["test_pointers"]["attributes"], ["PUBLIC"])
        
        # Test default_access_type
        default_type = module["types"]["default_access_type"]
        
        # Components should be PUBLIC by default
        self.assertIn("default_ptr1", default_type["procedures"])
        self.assertIn("default_ptr2", default_type["procedures"])
        self.assertIn("PUBLIC", default_type["procedures"]["default_ptr1"]["attributes"])
        self.assertIn("PUBLIC", default_type["procedures"]["default_ptr2"]["attributes"])
        
        # Explicit private
        self.assertIn("explicit_private_ptr", default_type["procedures"])
        self.assertIn("PRIVATE", default_type["procedures"]["explicit_private_ptr"]["attributes"])
        
        # Default with nopass
        self.assertIn("default_static", default_type["procedures"])
        self.assertEqual(default_type["procedures"]["default_static"]["pass_type"], PassType.NONE)
        self.assertIn("PUBLIC", default_type["procedures"]["default_static"]["attributes"])
        
        # Type-bound procedures affected by private statement
        self.assertEqual(default_type["procedures"]["method1"]["attributes"], ["PRIVATE"])
        self.assertEqual(default_type["procedures"]["public_method"]["attributes"], ["PUBLIC"])
        
        # Test init_edge_cases
        init_type = module["types"]["init_edge_cases"]
        
        # All procedure pointers should be captured regardless of initialization
        self.assertIn("uninitialized_ptr", init_type["procedures"])
        self.assertIn("null_init_ptr", init_type["procedures"])
        self.assertIn("private_null_ptr", init_type["procedures"])
        
        # Multiple with mixed initialization
        self.assertIn("ptr_a", init_type["procedures"])
        self.assertIn("ptr_b", init_type["procedures"])
        self.assertIn("ptr_c", init_type["procedures"])
        
        # Check private attribute on private_null_ptr
        self.assertIn("PRIVATE", init_type["procedures"]["private_null_ptr"]["attributes"])
        
        # All others should be PUBLIC (no private statement in type)
        for ptr_name in ["uninitialized_ptr", "null_init_ptr", "ptr_a", "ptr_b", "ptr_c"]:
            self.assertIn("PUBLIC", init_type["procedures"][ptr_name]["attributes"])
        
if __name__ == "__main__":
    unittest.main()
