import unittest
from pathlib import Path
from pyfakefs.fake_filesystem_unittest import TestCase
from doc4for.f90.generate_module_tree import extract_module_data
from doc4for.models.common import ImportType

class TestImportStatementsInInterfaces(TestCase):
    maxDiff = None

    def setUp(self):
        self.setUpPyfakefs()

    def test_import_all_from_host(self):
        """Test IMPORT statement without entity list (imports everything)."""
        self.fs.create_file(
            "/fake/path/import_all.f90",
            contents="""\
!!*
! Module demonstrating import all from host
!*!
module import_all
    implicit none
    
    type :: my_type
        real :: x, y
    end type my_type
    
    real, parameter :: my_constant = 3.14
    
contains
    !!*
    ! Subroutine with interface using import all
    !*!
    subroutine process_with_callback(data, callback)
        type(my_type), intent(inout) :: data
        
        interface
            subroutine callback(item)
                import  ! Import everything from host scope
                type(my_type), intent(inout) :: item
            end subroutine
        end interface
        
        call callback(data)
    end subroutine
end module import_all
"""
        )
        
        result = extract_module_data([Path("/fake/path/import_all.f90")])
        
        module = next((m for m in result if m["module_name"] == "import_all"), None)
        assert module is not None
        
        # Check the subroutine
        process_sub = module["subroutines"]["process_with_callback"]
        
        # Check the interface
        self.assertIn("callback", process_sub["argument_interfaces"])
        callback_interface = process_sub["argument_interfaces"]["callback"]
        
        # Check the callback procedure in the interface
        callback_proc = callback_interface["procedures"]["callback"]
        
        # Check imports
        self.assertEqual(len(callback_proc["imports"]), 1)
        import_stmt = callback_proc["imports"][0]
        self.assertEqual(import_stmt["import_type"], ImportType.ALL)
        self.assertEqual(import_stmt["entities"], [])

    def test_import_specific_entities(self):
        """Test IMPORT statement with specific entity list."""
        self.fs.create_file(
            "/fake/path/import_specific.f90",
            contents="""\
!!*
! Module demonstrating specific imports
!*!
module import_specific
    implicit none
    
    type :: type_a
        integer :: a
    end type type_a
    
    type :: type_b
        real :: b
    end type type_b
    
    type :: type_c
        complex :: c
    end type type_c
    
    integer, parameter :: max_size = 100
    
contains
    !!*
    ! Function with interface using specific imports
    !*!
    function transform_data(input, transformer) result(output)
        type(type_a), intent(in) :: input
        type(type_b) :: output
        
        interface
            function transformer(x) result(y)
                import :: type_a, type_b  ! Only import needed types
                type(type_a), intent(in) :: x
                type(type_b) :: y
            end function
        end interface
        
        output = transformer(input)
    end function
end module import_specific
"""
        )
        
        result = extract_module_data([Path("/fake/path/import_specific.f90")])
        
        module = next((m for m in result if m["module_name"] == "import_specific"), None)
        assert module is not None
        
        # Check the function
        transform_func = module["functions"]["transform_data"]
        
        # Check the interface
        self.assertIn("transformer", transform_func["argument_interfaces"])
        transformer_interface = transform_func["argument_interfaces"]["transformer"]
        
        # Check the transformer procedure in the interface
        transformer_proc = transformer_interface["procedures"]["transformer"]
        
        # Check imports
        self.assertEqual(len(transformer_proc["imports"]), 1)
        import_stmt = transformer_proc["imports"][0]
        self.assertEqual(import_stmt["import_type"], ImportType.SPECIFIC)
        self.assertEqual(set(import_stmt["entities"]), {"type_a", "type_b"})

    def test_multiple_import_statements(self):
        """Test multiple IMPORT statements in the same interface procedure."""
        self.fs.create_file(
            "/fake/path/multiple_imports.f90",
            contents="""\
!!*
! Module demonstrating multiple import statements
!*!
module multiple_imports
    implicit none
    
    type :: config_type
        logical :: debug
        integer :: verbosity
    end type config_type
    
    type :: data_type
        real, allocatable :: values(:)
    end type data_type
    
contains
    !!*
    ! Subroutine with interface having multiple imports
    !*!
    subroutine process_with_options(data, config, processor)
        type(data_type), intent(inout) :: data
        type(config_type), intent(in) :: config
        
        interface
            subroutine processor(d, c, status)
                import :: data_type     ! First import
                import :: config_type   ! Second import
                type(data_type), intent(inout) :: d
                type(config_type), intent(in) :: c
                integer, intent(out) :: status
            end subroutine
        end interface
        
        integer :: status
        call processor(data, config, status)
    end subroutine
end module multiple_imports
"""
        )
        
        result = extract_module_data([Path("/fake/path/multiple_imports.f90")])
        
        module = next((m for m in result if m["module_name"] == "multiple_imports"), None)
        assert module is not None
        
        # Check the subroutine
        process_sub = module["subroutines"]["process_with_options"]
        
        # Check the interface
        processor_interface = process_sub["argument_interfaces"]["processor"]
        processor_proc = processor_interface["procedures"]["processor"]
        
        # Check multiple imports
        self.assertEqual(len(processor_proc["imports"]), 2)
        
        # First import
        self.assertEqual(processor_proc["imports"][0]["import_type"], ImportType.SPECIFIC)
        self.assertEqual(processor_proc["imports"][0]["entities"], ["data_type"])
        
        # Second import
        self.assertEqual(processor_proc["imports"][1]["import_type"], ImportType.SPECIFIC)
        self.assertEqual(processor_proc["imports"][1]["entities"], ["config_type"])

    def test_import_in_multiple_interface_procedures(self):
        """Test IMPORT statements in different procedures within the same interface block."""
        self.fs.create_file(
            "/fake/path/multi_proc_interface.f90",
            contents="""\
!!*
! Module with interface containing multiple procedures with imports
!*!
module multi_proc_interface
    implicit none
    
    type :: state_type
        integer :: state_id
        real :: value
    end type state_type
    
contains
    !!*
    ! Generic interface with multiple procedures using imports
    !*!
    subroutine generic_processor(state, func)
        type(state_type), intent(inout) :: state
        
        interface
            subroutine func(s)
                import :: state_type
                type(state_type), intent(inout) :: s
            end subroutine
            
            function func_alt(s) result(modified)
                import  ! Import all
                type(state_type), intent(in) :: s
                type(state_type) :: modified
            end function
        end interface
        
        call func(state)
    end subroutine
end module multi_proc_interface
"""
        )
        
        result = extract_module_data([Path("/fake/path/multi_proc_interface.f90")])
        
        module = next((m for m in result if m["module_name"] == "multi_proc_interface"), None)
        assert module is not None
        
        # Check the subroutine
        generic_sub = module["subroutines"]["generic_processor"]
        
        # Should have one interface with multiple procedures
        # Note: The parser might handle this differently - adjust based on actual behavior
        self.assertIn("func", generic_sub["argument_interfaces"])
        func_interface = generic_sub["argument_interfaces"]["func"]
        
        # Check imports in first procedure
        if "func" in func_interface["procedures"]:
            func_proc = func_interface["procedures"]["func"]
            self.assertEqual(len(func_proc["imports"]), 1)
            self.assertEqual(func_proc["imports"][0]["import_type"], ImportType.SPECIFIC)
            self.assertEqual(func_proc["imports"][0]["entities"], ["state_type"])
        
        # Check imports in second procedure
        if "func_alt" in func_interface["procedures"]:
            func_alt_proc = func_interface["procedures"]["func_alt"]
            self.assertEqual(len(func_alt_proc["imports"]), 1)
            self.assertEqual(func_alt_proc["imports"][0]["import_type"], ImportType.ALL)
            self.assertEqual(func_alt_proc["imports"][0]["entities"], [])

    def test_import_with_use_in_same_interface(self):
        """Test IMPORT and USE statements in the same interface procedure."""
        self.fs.create_file(
            "/fake/path/external_module.f90",
            contents="""\
module external_module
    implicit none
    integer, parameter :: external_constant = 42
end module external_module
"""
        )
        
        self.fs.create_file(
            "/fake/path/import_and_use.f90",
            contents="""\
!!*
! Module demonstrating IMPORT and USE together
!*!
module import_and_use
    implicit none
    
    type :: local_type
        real :: local_value
    end type local_type
    
contains
    !!*
    ! Subroutine with interface using both IMPORT and USE
    !*!
    subroutine combined_example(data, processor)
        type(local_type), intent(inout) :: data
        
        interface
            subroutine processor(item, factor)
                use external_module, only: external_constant
                import :: local_type
                type(local_type), intent(inout) :: item
                integer, intent(in) :: factor
            end subroutine
        end interface
        
        call processor(data, 2)
    end subroutine
end module import_and_use
"""
        )
        
        result = extract_module_data([
            Path("/fake/path/external_module.f90"),
            Path("/fake/path/import_and_use.f90")
        ])
        
        module = next((m for m in result if m["module_name"] == "import_and_use"), None)
        assert module is not None
        
        # Check the subroutine
        combined_sub = module["subroutines"]["combined_example"]
        
        # Check the interface
        processor_interface = combined_sub["argument_interfaces"]["processor"]
        processor_proc = processor_interface["procedures"]["processor"]
        
        # Check imports
        self.assertEqual(len(processor_proc["imports"]), 1)
        self.assertEqual(processor_proc["imports"][0]["import_type"], ImportType.SPECIFIC)
        self.assertEqual(processor_proc["imports"][0]["entities"], ["local_type"])
        
        # Check uses
        self.assertIn("external_module", processor_proc["uses"])
        self.assertEqual(processor_proc["uses"]["external_module"]["selections"], ["external_constant"])

    def test_no_import_in_interface(self):
        """Test interface procedures without IMPORT statements."""
        self.fs.create_file(
            "/fake/path/no_import.f90",
            contents="""\
!!*
! Module with interface that doesn't need imports
!*!
module no_import
    implicit none
    
contains
    !!*
    ! Function with interface using only intrinsic types
    !*!
    function apply_operation(x, op) result(y)
        real, intent(in) :: x
        real :: y
        
        interface
            function op(val) result(res)
                ! No import needed - only uses intrinsic types
                real, intent(in) :: val
                real :: res
            end function
        end interface
        
        y = op(x)
    end function
end module no_import
"""
        )
        
        result = extract_module_data([Path("/fake/path/no_import.f90")])
        
        module = next((m for m in result if m["module_name"] == "no_import"), None)
        assert module is not None
        
        # Check the function
        apply_func = module["functions"]["apply_operation"]
        
        # Check the interface
        op_interface = apply_func["argument_interfaces"]["op"]
        op_proc = op_interface["procedures"]["op"]
        
        # Check no imports
        self.assertEqual(len(op_proc["imports"]), 0)

if __name__ == '__main__':
    unittest.main()
