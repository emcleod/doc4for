import unittest
from pathlib import Path
from pyfakefs.fake_filesystem_unittest import TestCase
from doc4for.f90.generate_file_tree import extract_file_data
from doc4for.f90.generate_module_tree import extract_module_data
from doc4for.models.common import BindingTypeEnum
from doc4for.models.dimension_models import BoundType

class TestEquivalenceDeclarations(TestCase):

    def setUp(self):
        self.setUpPyfakefs()

    def test_equivalence_in_module(self):
        self.fs.create_file(
            "/fake/path/equivalence_in_module.f90",
            contents="""\
module data_conversion
    implicit none
    
    ! Module variables NOT in any common block
    real :: temperature_celsius(100)
    real :: temperature_fahrenheit(100)
    real :: temperature_kelvin(100)
    
    !!* Buffer for binary I/O *!
    real(kind=8) :: double_buffer(1000)
    !!* Buffer for binary I/O *!
    integer(kind=8) :: int64_buffer(1000)
    !!* Buffer for binary I/O *!
    integer(kind=1) :: byte_buffer(8000)
    
    !!* Make different temperature arrays share memory *!
    equivalence (temperature_celsius, temperature_fahrenheit, temperature_kelvin)
    
    !!* Multiple views of the I/O buffer *!
    equivalence (double_buffer, int64_buffer, byte_buffer)
    
contains
    subroutine read_binary_data(unit)
        integer, intent(in) :: unit
        ! Read as bytes, process as doubles
        read(unit) byte_buffer
    end subroutine
end module
"""
        )
        
        result = extract_module_data([Path('/fake/path/equivalence_in_module.f90')])
        self.assertEqual(len(result), 1)
        module = result[0]
        
        # Check basic module info
        self.assertEqual(module["module_name"], "data_conversion")
        
        # Check that equivalence relationships exist
        self.assertIn("equivalence", module)
        self.assertIsNotNone(module["equivalence"])
        self.assertEqual(len(module["equivalence"]), 2)
        
        # Check temperature equivalence relationship
        temp_equiv = module["equivalence"][0]
        self.assertEqual(temp_equiv["description"], "Make different temperature arrays share memory\n")
        self.assertEqual(len(temp_equiv["variables"]), 3)
        self.assertIn("temperature_celsius", temp_equiv["variables"])
        self.assertIn("temperature_fahrenheit", temp_equiv["variables"])
        self.assertIn("temperature_kelvin", temp_equiv["variables"])
        
        # Check buffer equivalence relationship
        buffer_equiv = module["equivalence"][1]
        self.assertEqual(buffer_equiv["description"], "Multiple views of the I/O buffer\n")
        self.assertEqual(len(buffer_equiv["variables"]), 3)
        self.assertIn("double_buffer", buffer_equiv["variables"])
        self.assertIn("int64_buffer", buffer_equiv["variables"])
        self.assertIn("byte_buffer", buffer_equiv["variables"])
        
        # Verify the variables exist and have correct dimensions
        self.assertIn("variables", module)
        
        # Check temperature arrays
        self.assertIn("temperature_celsius", module["variables"])
        self.assertEqual(module["variables"]["temperature_celsius"]["type"], "REAL")
        self.assertIsNotNone(module["variables"]["temperature_celsius"]["dimension"])
        
        # Check buffer arrays with different kinds
        self.assertIn("double_buffer", module["variables"])
        self.assertEqual(module["variables"]["double_buffer"]["type"], "REAL")
        self.assertEqual(module["variables"]["double_buffer"]["kind"], "8")
        
        self.assertIn("int64_buffer", module["variables"])
        self.assertEqual(module["variables"]["int64_buffer"]["type"], "INTEGER")
        self.assertEqual(module["variables"]["int64_buffer"]["kind"], "8")
        
        self.assertIn("byte_buffer", module["variables"])
        self.assertEqual(module["variables"]["byte_buffer"]["type"], "INTEGER")
        self.assertEqual(module["variables"]["byte_buffer"]["kind"], "1")
        
        # Check that the subroutine exists
        self.assertIn("subroutines", module)
        self.assertIn("read_binary_data", module["subroutines"])


    def test_equivalence_at_file_level(self):
        self.fs.create_file(
            "/fake/path/file_level_equivalence.f90",
            contents="""\
    !!* Main program with equivalenced variables *!
    program file_level_test
        implicit none
        
        !!* Working arrays that are never used simultaneously *!
        real :: matrix_a(100, 100)
        real :: matrix_b(100, 100)
        real :: temp_workspace(10000)
        
        !!* Memory-efficient matrix storage *!
        equivalence (matrix_a, temp_workspace)
        equivalence (matrix_b, temp_workspace)
        
        call process_matrices()
    end program

    !!* Standalone subroutine with local equivalence *!
    subroutine standalone_sub()
        implicit none
        
        !!* Local buffer with multiple interpretations *!
        character(len=80) :: text_buffer
        real :: float_data(20)  
        integer :: int_data(20)
        
        !!* Interpret buffer as different types *!
        equivalence (text_buffer, float_data, int_data)
        
        read(5, '(A)') text_buffer
    end subroutine
    """
        )
        
        result = extract_file_data([Path('/fake/path/file_level_equivalence.f90')])
        self.assertEqual(len(result), 1)
        file_data = result[0]
        
        # Check program-level equivalence
        self.assertIn("programs", file_data)
        program = file_data["programs"]["file_level_test"]
        self.assertIn("equivalence", program)
        self.assertIsNotNone(program["equivalence"])
        self.assertEqual(len(program["equivalence"]), 2)
        
        # First equivalence: matrix_a with temp_workspace
        equiv1 = program["equivalence"][0]
        self.assertEqual(equiv1["description"], "Memory-efficient matrix storage\n")
        self.assertEqual(len(equiv1["variables"]), 2)
        self.assertIn("matrix_a", equiv1["variables"])
        self.assertIn("temp_workspace", equiv1["variables"])
        
        # Second equivalence: matrix_b with temp_workspace  
        equiv2 = program["equivalence"][1]
        self.assertEqual(len(equiv2["variables"]), 2)
        self.assertIn("matrix_b", equiv2["variables"])
        self.assertIn("temp_workspace", equiv2["variables"])
        
        # Check standalone subroutine equivalence
        self.assertIn("subroutines", file_data)
        sub = file_data["subroutines"]["standalone_sub"]
        self.assertIn("equivalence", sub)
        self.assertIsNotNone(sub["equivalence"])
        self.assertEqual(len(sub["equivalence"]), 1)
        
        sub_equiv = sub["equivalence"][0]
        self.assertEqual(sub_equiv["description"], "Interpret buffer as different types\n")
        self.assertEqual(len(sub_equiv["variables"]), 3)
        self.assertIn("text_buffer", sub_equiv["variables"])
        self.assertIn("float_data", sub_equiv["variables"])
        self.assertIn("int_data", sub_equiv["variables"])

    def test_equivalence_with_common_blocks(self):
        self.fs.create_file(
            "/fake/path/common_equivalence.f90",
            contents="""\
    module common_equiv_test
        implicit none
        
        !!* Variables in different contexts *!
        real :: module_var(100)
        real :: shared_data(200)
        real :: local_buffer(300)
        
        !!* Common block storage *!
        common /data_block/ shared_data
        
        !!* Link module variable with common block variable *!
        equivalence (module_var, shared_data(1))
        
        !!* Another equivalence with offset *!
        equivalence (local_buffer, shared_data(101))
        
    contains
        subroutine process_data()
            ! Modifying module_var affects shared_data(1:100)
            module_var = 42.0
        end subroutine
    end module
    """
        )
        
        result = extract_file_data([Path('/fake/path/common_equivalence.f90')])
        file_data = result[0]
        module = file_data["modules"]["common_equiv_test"]
        
        # Check equivalence relationships
        self.assertIn("equivalence", module)
        self.assertEqual(len(module["equivalence"]), 2)
        
        # First equivalence: module_var with shared_data
        equiv1 = module["equivalence"][0]
        self.assertEqual(equiv1["description"], "Link module variable with common block variable\n")
        self.assertEqual(len(equiv1["variables"]), 2)
        self.assertIn("module_var", equiv1["variables"])
        self.assertIn("shared_data", equiv1["variables"])
        
        # Second equivalence: local_buffer with shared_data offset
        equiv2 = module["equivalence"][1]
        self.assertEqual(equiv2["description"], "Another equivalence with offset\n")
        self.assertEqual(len(equiv2["variables"]), 2)
        self.assertIn("local_buffer", equiv2["variables"])
        self.assertIn("shared_data", equiv2["variables"])
        
        # Verify common block exists
        self.assertIn("common_blocks", module)
        self.assertIn("data_block", module["common_blocks"])

    def test_scalar_equivalence_unions(self):
        self.fs.create_file(
            "/fake/path/scalar_unions.f90",
            contents="""\
    module union_types
        implicit none
        
        !!* Union for different numeric representations *!
        real(kind=4) :: float_val
        integer(kind=4) :: int_val
        
        !!* Character overlay for bit examination *!
        character(len=4) :: char_val
        
        !!* Create union of scalar values *!
        equivalence (float_val, int_val, char_val)
        
        !!* Complex number decomposition *!
        complex :: complex_val
        real :: real_part, imag_part
        
        !!* Access complex components separately *!
        equivalence (complex_val, real_part)
        
        !!* Logical and integer equivalence *!
        logical :: flag
        integer :: flag_bits
        
        !!* Examine logical representation *!
        equivalence (flag, flag_bits)
        
    contains
        subroutine examine_representations()
            float_val = 3.14159
            write(*,*) 'Float:', float_val
            write(*,*) 'As integer bits:', int_val
            write(*,*) 'As characters:', char_val
        end subroutine
    end module
    """
        )
        
        result = extract_file_data([Path('/fake/path/scalar_unions.f90')])
        file_data = result[0]
        module = file_data["modules"]["union_types"]
        
        # Check equivalence relationships
        self.assertIn("equivalence", module)
        self.assertEqual(len(module["equivalence"]), 3)
        
        # First equivalence: float/int/char union
        equiv1 = module["equivalence"][0]
        self.assertEqual(equiv1["description"], "Create union of scalar values\n")
        self.assertEqual(len(equiv1["variables"]), 3)
        self.assertIn("float_val", equiv1["variables"])
        self.assertIn("int_val", equiv1["variables"])
        self.assertIn("char_val", equiv1["variables"])
        
        # Second equivalence: complex decomposition
        equiv2 = module["equivalence"][1]
        self.assertEqual(equiv2["description"], "Access complex components separately\n")
        self.assertEqual(len(equiv2["variables"]), 2)
        self.assertIn("complex_val", equiv2["variables"])
        self.assertIn("real_part", equiv2["variables"])
        
        # Third equivalence: logical/integer
        equiv3 = module["equivalence"][2]
        self.assertEqual(equiv3["description"], "Examine logical representation\n")
        self.assertEqual(len(equiv3["variables"]), 2)
        self.assertIn("flag", equiv3["variables"])
        self.assertIn("flag_bits", equiv3["variables"])
        
        # Verify scalar variables exist with correct types
        variables = module["variables"]
        self.assertEqual(variables["float_val"]["type"], "REAL")
        self.assertEqual(variables["int_val"]["type"], "INTEGER")
        self.assertEqual(variables["char_val"]["type"], "CHARACTER")
        self.assertEqual(variables["complex_val"]["type"], "COMPLEX")
        self.assertEqual(variables["logical_val"]["type"], "LOGICAL")

    def test_equivalence_with_array_sections(self):
        self.fs.create_file(
            "/fake/path/array_sections.f90",
            contents="""\
    module array_sections
        implicit none
        
        !!* Master data array *!
        real :: master_array(1000)
        
        !!* Array views of different sections *!
        real :: header_section(10)
        real :: data_section(980)  
        real :: footer_section(10)
        
        !!* Map sections to master array *!
        equivalence (master_array(1), header_section(1))
        equivalence (master_array(11), data_section(1))
        equivalence (master_array(991), footer_section(1))
        
        !!* Overlapping views *!
        real :: first_half(500), second_half(500)
        real :: middle_section(200)
        
        !!* Create overlapping array views *!
        equivalence (master_array(1), first_half(1))
        equivalence (master_array(501), second_half(1))
        equivalence (master_array(401), middle_section(1))
        
    contains
        subroutine process_sections()
            header_section = 1.0
            data_section = 2.0
            footer_section = 3.0
            ! master_array now has structured data
        end subroutine
    end module
    """
        )
        
        result = extract_file_data([Path('/fake/path/array_sections.f90')])
        file_data = result[0]
        module = file_data["modules"]["array_sections"]
        
        # Check equivalence relationships
        self.assertIn("equivalence", module)
        self.assertEqual(len(module["equivalence"]), 6)
        
        # Check section mappings
        equiv1 = module["equivalence"][0]
        self.assertEqual(equiv1["description"], "Map sections to master array\n")
        self.assertIn("master_array", equiv1["variables"])
        self.assertIn("header_section", equiv1["variables"])
        
        # Check overlapping views
        equiv4 = module["equivalence"][3]
        self.assertEqual(equiv4["description"], "Create overlapping array views\n")
        self.assertIn("master_array", equiv4["variables"])
        self.assertIn("first_half", equiv4["variables"])

    def test_multiple_equivalence_statements_same_variables(self):
        """Test multiple equivalence statements involving the same variables."""
        self.fs.create_file(
            "/fake/path/multiple_equiv.f90",
            contents="""\
    module multiple_equiv
        implicit none
        
        !!* Base arrays *!
        real :: array_a(100), array_b(100), array_c(100)
        real :: array_d(100), array_e(100)
        
        !!* First equivalence group *!
        equivalence (array_a, array_b)
        
        !!* Second equivalence group *!
        equivalence (array_c, array_d, array_e)
        
        !!* Additional equivalence extending first group *!
        equivalence (array_a, array_c)
        
    end module
    """
        )
        
        result = extract_file_data([Path('/fake/path/multiple_equiv.f90')])
        file_data = result[0]
        module = file_data["modules"]["multiple_equiv"]
        
        # Should have 3 separate equivalence relationships
        self.assertIn("equivalence", module)
        self.assertEqual(len(module["equivalence"]), 3)
        
        # First group: array_a, array_b
        equiv1 = module["equivalence"][0]
        self.assertEqual(equiv1["description"], "First equivalence group\n")
        self.assertEqual(len(equiv1["variables"]), 2)
        self.assertIn("array_a", equiv1["variables"])
        self.assertIn("array_b", equiv1["variables"])
        
        # Second group: array_c, array_d, array_e  
        equiv2 = module["equivalence"][1]
        self.assertEqual(equiv2["description"], "Second equivalence group\n")
        self.assertEqual(len(equiv2["variables"]), 3)
        self.assertIn("array_c", equiv2["variables"])
        self.assertIn("array_d", equiv2["variables"])
        self.assertIn("array_e", equiv2["variables"])
        
        # Third group: array_a, array_c (links the two groups)
        equiv3 = module["equivalence"][2]
        self.assertEqual(equiv3["description"], "Additional equivalence extending first group\n")
        self.assertEqual(len(equiv3["variables"]), 2)
        self.assertIn("array_a", equiv3["variables"])
        self.assertIn("array_c", equiv3["variables"])

    def test_equivalence_no_comments(self):
        self.fs.create_file(
            "/fake/path/no_comments.f90",
            contents="""\
    module no_comments
        implicit none
        
        real :: var1, var2, var3
        integer :: int1, int2
        
        ! Regular comment - should not be captured
        equivalence (var1, var2, var3)
        
        equivalence (int1, int2)
        
    end module
    """
        )
        
        result = extract_file_data([Path('/fake/path/no_comments.f90')])
        file_data = result[0]
        module = file_data["modules"]["no_comments"]
        
        # Should still capture equivalence relationships
        self.assertIn("equivalence", module)
        self.assertEqual(len(module["equivalence"]), 2)
        
        # First equivalence should have empty description
        equiv1 = module["equivalence"][0]
        self.assertEqual(equiv1["description"], "")
        self.assertEqual(len(equiv1["variables"]), 3)
        
        # Second equivalence should also have empty description
        equiv2 = module["equivalence"][1]
        self.assertEqual(equiv2["description"], "")  
        self.assertEqual(len(equiv2["variables"]), 2)

    def test_equivalence_in_block_data(self):
        self.fs.create_file(
            "/fake/path/block_data_equiv.f90",
            contents="""\
    !!* Initialize shared data with equivalence *!
    block data shared_init
        implicit none
        
        !!* Shared buffer with multiple views *!
        real(kind=8) :: shared_buffer(500)
        integer(kind=8) :: int_view(500)
        character(len=4000) :: string_view
        
        !!* Common storage for shared data *!
        common /shared_data/ shared_buffer
        
        !!* Multiple interpretations of the same data *!
        equivalence (shared_buffer, int_view, string_view)
        
        data shared_buffer /500*0.0d0/
    end block data
    """
        )
        
        result = extract_file_data([Path('/fake/path/block_data_equiv.f90')])
        file_data = result[0]
        
        # Check block data structure
        self.assertIn("block_data", file_data)
        block_data = file_data["block_data"]["shared_init"]
        
        # Check equivalence in block data
        self.assertIn("equivalence", block_data)
        self.assertEqual(len(block_data["equivalence"]), 1)
        
        equiv = block_data["equivalence"][0]
        self.assertEqual(equiv["description"], "Multiple interpretations of the same data\n")
        self.assertEqual(len(equiv["variables"]), 3)
        self.assertIn("shared_buffer", equiv["variables"])
        self.assertIn("int_view", equiv["variables"])
        self.assertIn("string_view", equiv["variables"])
        
        # Verify common block also exists
        self.assertIn("common_blocks", block_data)
        self.assertIn("shared_data", block_data["common_blocks"])
        
if __name__ == "__main__":
    unittest.main()
