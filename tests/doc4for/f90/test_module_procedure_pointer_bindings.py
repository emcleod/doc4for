import unittest
from pathlib import Path
from typing import cast
from pyfakefs.fake_filesystem_unittest import TestCase
from doc4for.f90.generate_module_tree import extract_module_data
from doc4for.models.common import BindingTypeEnum, BindingType

class TestTypeBindingProcedures(TestCase):

    def setUp(self):
        self.setUpPyfakefs()


    def test_procedure_pointer_binding(self):
        self.fs.create_file(
            "/fake/path/proc_pointer_binding.f90",
            contents="""\
    module proc_pointer_binding_mod
        use iso_c_binding
        implicit none
        
        !!* Abstract interface for C-interoperable function pointer *!
        abstract interface
            function callback_func(x) bind(c) result(y)
                use iso_c_binding
                real(c_double), value :: x
                real(c_double) :: y
            end function
        end interface
        
        !!* Procedure pointer with C binding *!
        procedure(callback_func), pointer, bind(c) :: c_callback => null()
        
        !!* Procedure pointer using abstract interface with binding *!
        procedure(callback_func), pointer :: f_callback => null()
        
        !!* Regular procedure pointer without binding *!
        procedure(real), pointer :: simple_func => null()

    contains

        !!* Set the C callback pointer *!
        subroutine set_callback(new_callback) bind(c, name='set_c_callback')
            type(c_funptr), value :: new_callback
            ! Implementation would convert c_funptr to Fortran procedure pointer
        end subroutine
        
        !!* Function that can be assigned to procedure pointer *!
        function square_it(x) bind(c, name='c_square') result(y)
            use iso_c_binding
            real(c_double), value :: x
            real(c_double) :: y
            y = x * x
        end function

    end module proc_pointer_binding_mod
    """
        )
        result = extract_module_data([Path('/fake/path/proc_pointer_binding.f90')])
        module = result[0]
        
        # Check the abstract interface
        callback_interface = module['interfaces'][0]  # First interface
        self.assertIn('abstract', callback_interface['attributes'])
        
        callback_func = callback_interface['procedures']['callback_func']
        self.assertIn('binding_type', callback_func)
        self.assertEqual(callback_func['binding_type']['type'], BindingTypeEnum.BIND_C)
        
        # Check the module procedures
        set_callback = module['subroutines']['set_callback']
        self.assertIn('binding_type', set_callback)
        self.assertEqual(set_callback['binding_type']['type'], BindingTypeEnum.BIND_C)
        self.assertEqual(set_callback['binding_type']['name'], 'set_c_callback')
        
        square_it = module['functions']['square_it']
        self.assertIn('binding_type', square_it)
        self.assertEqual(square_it['binding_type']['type'], BindingTypeEnum.BIND_C)
        self.assertEqual(square_it['binding_type']['name'], 'c_square')
                
        if 'c_callback' in module.get('variables', {}):
            c_callback = module['variables']['c_callback']
            self.assertIn('binding_type', c_callback)
            self.assertEqual(c_callback['binding_type']['type'], BindingTypeEnum.BIND_C)
            self.assertIn('pointer', c_callback['attributes'])
        
        if 'f_callback' in module.get('variables', {}):
            f_callback = module['variables']['f_callback']
            self.assertIn('binding_type', f_callback)
            self.assertIn('pointer', f_callback['attributes'])


    def test_mixed_procedure_pointers(self):
        self.fs.create_file(
            "/fake/path/mixed_proc_pointers.f90",
            contents="""\
    module mixed_proc_pointers_mod
        use iso_c_binding
        implicit none
        
        !!* Type containing procedure pointers with mixed binding *!
        type :: callback_container
            !!* C-bound procedure pointer *!
            procedure(c_func_interface), pointer, nopass, bind(c) :: c_proc => null()
            
            !!* Regular procedure pointer *!
            procedure(regular_func_interface), pointer, nopass :: f_proc => null()
        end type
        
        !!* Abstract interface for C-compatible function *!
        abstract interface
            function c_func_interface(x) bind(c) result(y)
                use iso_c_binding
                real(c_double), value :: x
                real(c_double) :: y
            end function
        end interface
        
        !!* Abstract interface for regular Fortran function *!
        abstract interface
            function regular_func_interface(x) result(y)
                real, intent(in) :: x
                real :: y
            end function
        end interface
        
        !!* Array of procedure pointers with binding *!
        procedure(c_func_interface), pointer, bind(c) :: c_funcs(5) => null()
        
    contains

        !!* Register a C function pointer *!
        subroutine register_c_proc(container, proc_ptr) bind(c)
            type(callback_container), intent(inout) :: container
            type(c_funptr), value :: proc_ptr
            ! Implementation would convert c_funptr to procedure pointer
        end subroutine
        
        !!* Register a Fortran function pointer *!
        subroutine register_f_proc(container, proc_ptr)
            type(callback_container), intent(inout) :: container
            procedure(regular_func_interface), pointer, intent(in) :: proc_ptr
            container%f_proc => proc_ptr
        end subroutine
        
        !!* Example implementation matching C interface *!
        function example_c_func(x) bind(c) result(y)
            use iso_c_binding
            real(c_double), value :: x
            real(c_double) :: y
            y = x * 2.0
        end function
        
        !!* Example implementation matching regular interface *!
        function example_f_func(x) result(y)
            real, intent(in) :: x
            real :: y
            y = x * 3.0
        end function

    end module mixed_proc_pointers_mod
    """
        )
        result = extract_module_data([Path('/fake/path/mixed_proc_pointers.f90')])
        module = result[0]
            
        result = extract_module_data([Path('/fake/path/mixed_proc_pointers.f90')])
        module = result[0]
        
        # Check the type with procedure pointers
        callback_type = module['types']['callback_container']
        
        # Check c_proc - now in the procedures section
        c_proc = callback_type['procedures']['c_proc']
        self.assertIn('binding_type', c_proc)
        binding_type = cast(BindingType, c_proc['binding_type'])
        self.assertEqual(binding_type['type'], BindingTypeEnum.BIND_C)
        self.assertIn('nopass', c_proc['attributes'])
        
        # Check f_proc - now in the procedures section
        f_proc = callback_type['procedures']['f_proc']
        self.assertIn('binding_type', f_proc)
        binding_type = cast(BindingType, f_proc['binding_type'])
        self.assertEqual(binding_type['type'], BindingTypeEnum.DEFAULT)
        self.assertIn('nopass', f_proc['attributes'])
        
        # Check the abstract interfaces
        c_interface = module['interfaces'][0]  # First interface
        c_func = c_interface['procedures']['c_func_interface']
        self.assertEqual(c_func['binding_type']['type'], BindingTypeEnum.BIND_C)
        
        f_interface = module['interfaces'][1]  # Second interface
        f_func = f_interface['procedures']['regular_func_interface']
        self.assertEqual(f_func['binding_type']['type'], BindingTypeEnum.DEFAULT)
        
        # Check subroutines
        register_c = module['subroutines']['register_c_proc']
        self.assertEqual(register_c['binding_type']['type'], BindingTypeEnum.BIND_C)
        
        register_f = module['subroutines']['register_f_proc']
        self.assertEqual(register_f['binding_type']['type'], BindingTypeEnum.DEFAULT)
        
        # Check function implementations
        c_example = module['functions']['example_c_func']
        self.assertEqual(c_example['binding_type']['type'], BindingTypeEnum.BIND_C)
        
        f_example = module['functions']['example_f_func']
        self.assertEqual(f_example['binding_type']['type'], BindingTypeEnum.DEFAULT)


if __name__ == "__main__":
    unittest.main()
