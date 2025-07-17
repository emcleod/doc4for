import unittest
from pathlib import Path
from typing import cast
from pyfakefs.fake_filesystem_unittest import TestCase
from doc4for.models.common import Expression, ExpressionType
from doc4for.models.dimension_models import ArrayBound, BoundType, Dimension
from doc4for.f90.generate_module_tree import extract_module_data
from doc4for.f90.generate_file_tree import extract_file_data

# Helper function for creating dimension expressions
def create_dimension_expr(lower, upper):
    return ArrayBound(
        bound_type=BoundType.FIXED,
        lower=Expression(expr_type=ExpressionType.LITERAL, value=str(lower), function_name=None, arguments=None),
        upper=Expression(expr_type=ExpressionType.LITERAL, value=str(upper), function_name=None, arguments=None),
    )


class TestDataStatements(TestCase):
    
    def setUp(self):
        self.setUpPyfakefs()

    def test_module_basic_data_statements(self):
        """Test basic DATA statements in modules."""
        self.fs.create_file(
            "/fake/path/module_data.f90",
            contents="""\
module data_test_mod
    implicit none
    
    !!* Integers *!
    integer :: i, j, k
    !!* Reals *!
    real :: x, y, z
    !!* The name *!
    character(len=10) :: name
    !!* A flag *!
    logical :: flag
    
    ! Arrays
    integer :: arr(5)
    real :: vec(3)
    
    ! Initialize with DATA statements
    data i, j, k /1, 2, 3/
    data x, y, z /1.1, 2.2, 3.3/
    data name /'John Doe'/
    data flag /.true./
    data arr /5, 4, 3, 2, 1/
    data vec /1.0, 2.0, 3.0/
    
end module data_test_mod
"""
        )
        
        result = extract_module_data([Path("/fake/path/module_data.f90")])
        self.assertEqual(len(result), 1)
        module_data = result[0]
                
        # Check variables were initialized
        variables = module_data["variables"]
        self.assertEqual(variables["i"]["initial_value"], "1")
        self.assertEqual(variables["i"]["description"], "Integers\n")
        self.assertEqual(variables["j"]["initial_value"], "2")
        self.assertEqual(variables["j"]["description"], "Integers\n")
        self.assertEqual(variables["k"]["initial_value"], "3")
        self.assertEqual(variables["k"]["description"], "Integers\n")
        self.assertEqual(variables["x"]["initial_value"], "1.1")
        self.assertEqual(variables["x"]["description"], "Reals\n")
        self.assertEqual(variables["y"]["initial_value"], "2.2")
        self.assertEqual(variables["y"]["description"], "Reals\n")
        self.assertEqual(variables["z"]["initial_value"], "3.3")
        self.assertEqual(variables["z"]["description"], "Reals\n")
        self.assertEqual(variables["name"]["initial_value"], "'John Doe'")
        self.assertEqual(variables["name"]["description"], "The name\n")
        self.assertEqual(variables["flag"]["initial_value"], ".TRUE.")
        self.assertEqual(variables["flag"]["description"], "A flag\n")
        self.assertEqual(variables["arr"]["initial_value"], "5, 4, 3, 2, 1")
        self.assertEqual(variables["arr"]["description"], "")
        self.assertEqual(variables["vec"]["initial_value"], "1.0, 2.0, 3.0")
        self.assertEqual(variables["vec"]["description"], "")

    def test_module_array_data_statements(self):
        """Test array DATA statements in modules."""
        self.fs.create_file(
            "/fake/path/module_array_data.f90",
            contents="""\
module array_data_mod
    implicit none
    
    ! Array declarations
    integer :: simple_array(5)
    real :: repeated_values(10)
    integer :: mixed_array(8)
    integer :: matrix(3,3)
    
    ! Simple array with listed values
    data simple_array /1, 2, 3, 4, 5/
    
    ! Array with repeated values
    data repeated_values /10*1.5/
    
    ! Array with mixed regular and repeated values
    data mixed_array /3*0, 1, 2, 3*9/
    
    ! 2D array initialization
    data matrix /1, 2, 3, 4, 5, 6, 7, 8, 9/
    
end module array_data_mod
"""
        )
        
        result = extract_module_data([Path("/fake/path/module_array_data.f90")])
        self.assertEqual(len(result), 1)
        module_data = result[0]
        
        # Check variables were initialized
        variables = module_data["variables"]
        self.assertEqual(variables["simple_array"]["initial_value"], "1, 2, 3, 4, 5")
        self.assertEqual(variables["repeated_values"]["initial_value"], "10*1.5")
        self.assertEqual(variables["mixed_array"]["initial_value"], "3*0, 1, 2, 3*9")
        self.assertEqual(variables["matrix"]["initial_value"], "1, 2, 3, 4, 5, 6, 7, 8, 9")
        
        # Verify dimensions
        dimension = cast(Dimension, variables["simple_array"]["dimension"])
        self.assertEqual(dimension["dimensions"][0], create_dimension_expr(1, 5))
        
        dimension = cast(Dimension, variables["matrix"]["dimension"])
        self.assertEqual(len(dimension["dimensions"]), 2)
        self.assertEqual(dimension["dimensions"][0], create_dimension_expr(1, 3))
        self.assertEqual(dimension["dimensions"][1], create_dimension_expr(1, 3))

    def test_module_character_data_statements(self):
        """Test character DATA statements in modules."""
        self.fs.create_file(
            "/fake/path/module_char_data.f90",
            contents="""\
module char_data_mod
    implicit none
    
    ! Character variables
    character(len=20) :: full_name
    character(len=50) :: message
    character(len=10) :: codes(3)
    character(len=30) :: partial_init
    
    ! Simple character initialization
    data full_name /'John Smith'/
    
    ! Long string with continuation
    data message /'This is a longer message that demonstrates &
                character continuation in Fortran'/
    
    ! Array of strings
    data codes /'ABC123', 'DEF456', 'GHI789'/
    
    ! Substring initialization (not supported by all compilers in modules)
    data partial_init(1:10) /'First ten '/
    data partial_init(11:20) /'characters'/
    
end module char_data_mod
"""
        )
        
        result = extract_module_data([Path("/fake/path/module_char_data.f90")])
        self.assertEqual(len(result), 1)
        module_data = result[0]
        
        # Check variables were initialized
        variables = module_data["variables"]
        self.assertEqual(variables["full_name"]["initial_value"], "'John Smith'")
        
        message_value = variables["message"]["initial_value"]
        self.assertEqual(message_value, "'This is a longer message that demonstrates                 character continuation in Fortran'")
        
        codes_value = variables["codes"]["initial_value"]
        self.assertEqual(codes_value, "'ABC123', 'DEF456', 'GHI789'")
        
        # Test substring initialization (may not work in all compilers)
        partial_value = variables["partial_init"]["initial_value"]
        self.assertEqual(partial_value, "(1:10)='First ten ', (11:20)='characters'")

    def test_module_implied_do_loops(self):
        """Test implied DO loops in DATA statements in modules."""
        self.fs.create_file(
            "/fake/path/module_implied_do.f90",
            contents="""\
module implied_do_mod
    implicit none
    
    ! Arrays to initialize with DO loops
    integer :: sequence(10)
    integer :: matrix(3,3)
    real :: diagonal(5,5)
    
    ! Simple implied DO loop
    data (sequence(i), i=1,10) /10*0/
    
    ! Nested implied DO loop for 2D array
    data ((matrix(i,j), i=1,3), j=1,3) /9*0/
    
    ! Selective initialization (only diagonal elements)
    data (diagonal(i,i), i=1,5) /5*1.0/
    
end module implied_do_mod
"""
        )
        
        result = extract_module_data([Path("/fake/path/module_implied_do.f90")])
        self.assertEqual(len(result), 1)
        module_data = result[0]
        
        # Check variables were initialized
        variables = module_data["variables"]
        self.assertEqual(variables["sequence"]["initial_value"], "(sequence(i), i=1,10) = 10*0")
        self.assertEqual(variables["matrix"]["initial_value"], "((matrix(i,j), i=1,3), j=1,3) = 9*0")
        self.assertEqual(variables["diagonal"]["initial_value"], "(diagonal(i,i), i=1,5) = 5*1.0")
        
    def test_module_with_common_blocks(self):
        """Test DATA statements with common blocks in modules."""
        self.fs.create_file(
            "/fake/path/module_common_data.f90",
            contents="""\
module common_data_mod
    implicit none
    
    ! Variables in common blocks
    integer :: a, b, c
    real :: x, y, z
    
    ! Common block declaration
    common /block1/ a, b, c
    common /block2/ x, y, z
    
    ! Initialize common block variables
    data a, b, c /10, 20, 30/
    data x, y, z /1.1, 2.2, 3.3/
    
end module common_data_mod
"""
        )
        
        result = extract_module_data([Path("/fake/path/module_common_data.f90")])
        self.assertEqual(len(result), 1)
        module_data = result[0]
        
        # Check common block variables were initialized
        common_blocks = module_data["common_blocks"]
        self.assertIn("block1", common_blocks)
        self.assertIn("block2", common_blocks)
        
        block1_vars = common_blocks["block1"]["variables"]
        self.assertEqual(block1_vars["a"]["initial_value"], "10")
        self.assertEqual(block1_vars["b"]["initial_value"], "20")
        self.assertEqual(block1_vars["c"]["initial_value"], "30")
        
        block2_vars = common_blocks["block2"]["variables"]
        self.assertEqual(block2_vars["x"]["initial_value"], "1.1")
        self.assertEqual(block2_vars["y"]["initial_value"], "2.2")
        self.assertEqual(block2_vars["z"]["initial_value"], "3.3")

    def test_block_data_basic(self):
        """Test basic DATA statements in block data."""
        self.fs.create_file(
            "/fake/path/basic_block_data.f90",
            contents="""\
block data basic_data_init
    implicit none
    
    ! Scalar variables
    integer :: i, j, k
    real :: x, y, z
    character(len=10) :: name
    
    common /scalars/ i, j, k, x, y, z, name
    
    ! Initialize with DATA statements
    data i, j, k /1, 2, 3/
    data x, y, z /1.1, 2.2, 3.3/
    data name /'John Doe'/
    
end block data basic_data_init
"""
        )
        
        result = extract_file_data([Path('/fake/path/basic_block_data.f90')])
        file_data = result[0]
        
        # Get block data
        block_data = file_data["block_data"]["basic_data_init"]
                
        # Check variables were initialized
        scalars_block = block_data["common_blocks"]["scalars"]
        variables = scalars_block["variables"]
        
        self.assertEqual(variables["i"]["initial_value"], "1")
        self.assertEqual(variables["j"]["initial_value"], "2")
        self.assertEqual(variables["k"]["initial_value"], "3")
        self.assertEqual(variables["x"]["initial_value"], "1.1")
        self.assertEqual(variables["y"]["initial_value"], "2.2")
        self.assertEqual(variables["z"]["initial_value"], "3.3")
        self.assertEqual(variables["name"]["initial_value"], "'John Doe'")

    def test_block_data_array_initializations(self):
        """Test array DATA statements in block data."""
        self.fs.create_file(
            "/fake/path/array_block_data.f90",
            contents="""\
block data array_initializations
    implicit none
    
    ! Array declarations
    integer :: simple_array(5)
    real :: repeated_values(10)
    integer :: mixed_array(8)
    integer :: matrix(3,3)
    
    common /arrays/ simple_array, repeated_values, mixed_array, matrix
    
    ! Simple array with listed values
    data simple_array /1, 2, 3, 4, 5/
    
    ! Array with repeated values
    data repeated_values /10*1.5/
    
    ! Array with mixed regular and repeated values
    data mixed_array /3*0, 1, 2, 3*9/
    
    ! 2D array initialization
    data matrix /1, 2, 3, 4, 5, 6, 7, 8, 9/
    
end block data array_initializations
"""
        )
        
        result = extract_file_data([Path('/fake/path/array_block_data.f90')])
        file_data = result[0]
        
        # Get block data
        block_data = file_data["block_data"]["array_initializations"]
                
        # Get common block variables
        arrays_block = block_data["common_blocks"]["arrays"]
        variables = arrays_block["variables"]
        
        # Test simple array initialization
        simple_array = variables["simple_array"]
        self.assertEqual(simple_array["initial_value"], "1, 2, 3, 4, 5")
        
        # Test array with repeated values
        repeated_values = variables["repeated_values"]
        self.assertEqual(repeated_values["initial_value"], "10*1.5")
        
        # Test array with mixed values
        mixed_array = variables["mixed_array"]
        self.assertEqual(mixed_array["initial_value"], "3*0, 1, 2, 3*9")
        
        # Test 2D array
        matrix = variables["matrix"]
        self.assertEqual(matrix["initial_value"], "1, 2, 3, 4, 5, 6, 7, 8, 9")
        self.assertIsNotNone(matrix["dimension"])
        self.assertEqual(len(matrix["dimension"]["dimensions"]), 2)

    def test_block_data_multiple_variables_single_data(self):
        """Test multiple variables in a single DATA statement in block data."""
        self.fs.create_file(
            "/fake/path/multi_var_block_data.f90",
            contents="""\
block data multi_variable_initialization
    implicit none
    
    ! Variable declarations
    integer :: a, b, c
    real :: x, y, z
    character(len=10) :: name1, name2
    integer :: scores(3)
    
    common /multi_var/ a, b, c, x, y, z, name1, name2, scores
    
    ! Multiple variables in one DATA statement
    data a, b, c /10, 20, 30/
    
    ! Mixed types in one statement
    data x, y, z /1.1, 2.2, 3.3/
    
    ! Character variables in one statement
    data name1, name2 /'John', 'Jane'/
    
    ! Mix scalar and array in single statement
    data scores /85, 90, 95/
    
end block data multi_variable_initialization
"""
        )
        
        result = extract_file_data([Path('/fake/path/multi_var_block_data.f90')])
        file_data = result[0]
        
        # Get block data
        block_data = file_data["block_data"]["multi_variable_initialization"]
        
        # Get common block variables
        multi_var_block = block_data["common_blocks"]["multi_var"]
        variables = multi_var_block["variables"]
        
        # Test integer initialization
        self.assertEqual(variables["a"]["initial_value"], "10")
        self.assertEqual(variables["b"]["initial_value"], "20")
        self.assertEqual(variables["c"]["initial_value"], "30")
        
        # Test floating point initialization
        self.assertEqual(variables["x"]["initial_value"], "1.1")
        self.assertEqual(variables["y"]["initial_value"], "2.2")
        self.assertEqual(variables["z"]["initial_value"], "3.3")
        
        # Test character initialization
        self.assertEqual(variables["name1"]["initial_value"], "'John'")
        self.assertEqual(variables["name2"]["initial_value"], "'Jane'")
        
        # Test array initialization from single statement
        self.assertEqual(variables["scores"]["initial_value"], "85, 90, 95")

    def test_block_data_implied_do_loops(self):
        """Test implied DO loops in DATA statements in block data."""
        self.fs.create_file(
            "/fake/path/implied_do_block_data.f90",
            contents="""\
block data implied_do_initialization
    implicit none
    
    ! Arrays to initialize with DO loops
    integer :: sequence(10)
    integer :: matrix(3,3)
    real :: diagonal(5,5)
    
    common /loop_data/ sequence, matrix, diagonal
    
    ! Simple implied DO loop
    data (sequence(i), i=1,10) /10*0/
    
    ! Nested implied DO loop for 2D array
    data ((matrix(i,j), i=1,3), j=1,3) /9*0/
    
    ! Selective initialization (only diagonal elements)
    data (diagonal(i,i), i=1,5) /5*1.0/
    
end block data implied_do_initialization
"""
        )
        
        result = extract_file_data([Path('/fake/path/implied_do_block_data.f90')])
        file_data = result[0]
        
        # Get block data
        block_data = file_data["block_data"]["implied_do_initialization"]
        
        # Get common block variables
        loop_block = block_data["common_blocks"]["loop_data"]
        variables = loop_block["variables"]
        
        sequence = variables["sequence"]
        matrix = variables["matrix"]
        diagonal = variables["diagonal"]
        
        self.assertEqual(sequence["initial_value"], "(sequence(i), i=1,10) = 10*0")
        self.assertEqual(matrix["initial_value"], "((matrix(i,j), i=1,3), j=1,3) = 9*0")
        self.assertEqual(diagonal["initial_value"], "(diagonal(i,i), i=1,5) = 5*1.0")

    def test_block_data_character_and_substring(self):
        """Test character DATA statements and substring initialization in block data."""
        self.fs.create_file(
            "/fake/path/char_block_data.f90",
            contents="""\
block data character_initialization
    implicit none
    
    ! Character variables
    character(len=20) :: full_name
    character(len=50) :: message
    character(len=10) :: codes(3)
    character(len=30) :: partial_init
    
    common /char_data/ full_name, message, codes, partial_init
    
    ! Simple character initialization
    data full_name /'John Smith'/
    
    ! Long string with continuation
    data message /'This is a longer message that demonstrates &
                character continuation in Fortran'/
    
    ! Array of strings
    data codes /'ABC123', 'DEF456', 'GHI789'/
    
    ! Substring initialization
    data partial_init(1:10) /'First ten '/
    data partial_init(11:20) /'characters'/
    
end block data character_initialization
"""
        )
        
        result = extract_file_data([Path('/fake/path/char_block_data.f90')])
        file_data = result[0]
        
        # Get block data
        block_data = file_data["block_data"]["character_initialization"]
        
        # Get common block variables
        char_block = block_data["common_blocks"]["char_data"]
        variables = char_block["variables"]
        
        # Test character initializations
        self.assertEqual(variables["full_name"]["initial_value"], "'John Smith'")
        
        # Test continued string 
        message_value = variables["message"]["initial_value"]
        self.assertIn("continuation", message_value)
        
        # Test array of strings
        codes_value = variables["codes"]["initial_value"]
        self.assertIn("ABC123", codes_value)
        self.assertIn("DEF456", codes_value)
        self.assertIn("GHI789", codes_value)
        
        # Test substring initialization 
        partial_value = variables["partial_init"]["initial_value"]
        self.assertIsNotNone(partial_value)
        self.assertTrue(
            any(text in partial_value for text in ["First ten ", "characters"]),
            f"Expected 'First ten ' or 'characters' in {partial_value}"
        )

    def test_advanced_implied_do_loops(self):
        """Test advanced implied DO loops with stride, expressions, and nesting."""
        self.fs.create_file(
            "/fake/path/advanced_do.f90",
            contents="""\
    module advanced_do_mod
        implicit none
        
        integer, parameter :: n = 5
        integer :: arr1(10)
        integer :: arr2(10)
        integer :: matrix(4,4)
        integer :: cube(3,3,3)
        
        ! Implied DO with stride
        data (arr1(i), i=1,10,2) /5*-1/
        
        ! Implied DO with parameter expressions
        data (arr2(i), i=1,n*2) /10*99/
        
        ! Double nested implied DO
        data ((matrix(i,j), i=1,4), j=1,4) /16*0/
        
        ! Triple nested implied DO
        data (((cube(i,j,k), i=1,3), j=1,3), k=1,3) /27*0/
        
    end module advanced_do_mod
    """
        )
        
        result = extract_module_data([Path("/fake/path/advanced_do.f90")])
        self.assertEqual(len(result), 1)
        module_data = result[0]
        
        # Check variables were initialized
        variables = module_data["variables"]
        
        # Check arr1 (stride DO loop)
        self.assertEqual(variables["arr1"]["initial_value"], "(arr1(i), i=1,10) = 5*-1")
        
        # Check arr2 (expression bounds)
        self.assertEqual(variables["arr2"]["initial_value"], "(arr2(i), i=1,n*2) = 10*99")
        
        # Check matrix (double nested)
        self.assertEqual(variables["matrix"]["initial_value"], "((matrix(i,j), i=1,4), j=1,4) = 16*0")
        
        # Check cube (triple nested)
        self.assertEqual(variables["cube"]["initial_value"], "(((cube(i,j,k), i=1,3), j=1,3), k=1,3) = 27*0")

    def test_array_section_initialization(self):
        """Test initialization of specific array sections."""
        self.fs.create_file(
            "/fake/path/array_sections.f90",
            contents="""\
    module array_sections_mod
        implicit none
        
        integer :: arr(10) = 0  ! Default initialization
        real :: matrix(4,4) = 0.0
        integer :: sparse(20) = 0
        
        ! Initialize specific array section
        data arr(2:5) /4*99/
        
        ! Initialize 2D array section
        data matrix(2:3, 2:3) /4*1.0/
        
        ! Non-contiguous sections
        data sparse(1:20:2) /10*1/  ! Every other element
        
    end module array_sections_mod
    """
        )
        
        result = extract_module_data([Path("/fake/path/array_sections.f90")])
        self.assertEqual(len(result), 1)
        module_data = result[0]
        
        # Check variables were initialized
        variables = module_data["variables"]
        
        # For array sections, the variable should retain its default initialization
        # and the data statement should be recorded, but the initial_value may be complex
        # to represent in the variable description
        
        #TODO fill in asserts

    def test_complex_number_initialization(self):
        """Test initialization of complex numbers."""
        self.fs.create_file(
            "/fake/path/complex_init.f90",
            contents="""\
    module complex_init_mod
        implicit none
        
        complex :: z1
        complex :: z_arr(3)
        complex :: z_matrix(2,2)
        
        ! Single complex value
        data z1 /(1.0, 2.0)/
        
        ! Array of complex values
        data z_arr /(1.0, 1.0), (2.0, 2.0), (3.0, 3.0)/
        
        ! 2D array of complex values
        data z_matrix /(1.0, 1.0), (2.0, 2.0), (3.0, 3.0), (4.0, 4.0)/
        
    end module complex_init_mod
    """
        )
        
        result = extract_module_data([Path("/fake/path/complex_init.f90")])
        self.assertEqual(len(result), 1)
        module_data = result[0]
        
        # Check variables were initialized
        variables = module_data["variables"]
        
        # Check single complex value
        self.assertEqual(variables["z1"]["initial_value"], "(1.0, 2.0)")
        
        # Check array of complex values
        self.assertEqual(variables["z_arr"]["initial_value"], "(1.0, 1.0), (2.0, 2.0), (3.0, 3.0)")
        
        # Check 2D array of complex values
        self.assertEqual(variables["z_matrix"]["initial_value"], "(1.0, 1.0), (2.0, 2.0), (3.0, 3.0), (4.0, 4.0)")

    def test_complex_repeat_patterns(self):
        """Test complex repeat patterns in DATA statements."""
        self.fs.create_file(
            "/fake/path/repeat_patterns.f90",
            contents="""\
    module repeat_patterns_mod
        implicit none
        
        real :: simple_repeat(10)
        real :: expanded_repeat(15)
        integer :: mixed_patterns(40)
        
        ! Simple repeat pattern
        data simple_repeat /10*1.5/
        
        ! Expanded repeat pattern
        data expanded_repeat /2*1.0, 3*2.0, 2*1.0, 3*2.0, 2*1.0, 3*2.0/
        
        ! Mixed patterns with valid syntax
        data mixed_patterns /10*0, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 15*9/
        
    end module repeat_patterns_mod
    """
        )
        
        result = extract_module_data([Path("/fake/path/repeat_patterns.f90")])
        self.assertEqual(len(result), 1)
        module_data = result[0]
        
        # Check variables were initialized
        variables = module_data["variables"]
        
        # Check simple repeat pattern
        self.assertEqual(variables["simple_repeat"]["initial_value"], "10*1.5")
        
        # Check expanded repeat pattern
        expanded_value = variables["expanded_repeat"]["initial_value"]
        self.assertEqual(expanded_value, "2*1.0, 3*2.0, 2*1.0, 3*2.0, 2*1.0, 3*2.0")
        
        # Check mixed patterns
        mixed_value = variables["mixed_patterns"]["initial_value"]
        expected_mixed = "10*0, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 15*9"
        self.assertEqual(mixed_value, expected_mixed)
                    
    def test_multiple_data_statements_same_variable(self):
        """Test multiple DATA statements for the same variable."""
        self.fs.create_file(
            "/fake/path/multiple_data.f90",
            contents="""\
    module multiple_data_mod
        implicit none
        
        integer :: arr(10)
        character(len=30) :: message
        
        ! Partial initialization in multiple statements
        data arr(1:5) /5*1/
        data arr(6:10) /5*2/
        
        ! Substring initialization
        data message(1:10) /'First part'/
        data message(11:20) /'Second part'/
        data message(21:30) /'Third part'/
        
    end module multiple_data_mod
    """
        )
        
        result = extract_module_data([Path("/fake/path/multiple_data.f90")])
        self.assertEqual(len(result), 1)
        module_data = result[0]
        
        # Check variables were initialized
        variables = module_data["variables"]
        
        # For multiple DATA statements on the same variable, we expect
        # the parser to combine them with commas
        
        # Check array partial initialization
        arr_value = variables["arr"]["initial_value"]
        self.assertEqual(arr_value, "(1:5)=5*1, (6:10)=5*2")
        
        # Check character substring initialization
        message_value = variables["message"]["initial_value"]
        self.assertEqual(message_value, "(1:10)='First part', (11:20)='Second part', (21:30)='Third part'")
        
        # Verify the variables have correct types and dimensions
        self.assertEqual(variables["arr"]["type"], "INTEGER")
        self.assertIsNotNone(variables["arr"]["dimension"])
        
        self.assertEqual(variables["message"]["type"], "CHARACTER")
        self.assertEqual(variables["message"]["length"], "30")
        
    def test_parameter_values_in_data(self):
        """Test using PARAMETER values in DATA statements."""
        self.fs.create_file(
            "/fake/path/param_data.f90",
            contents="""\
    module param_data_mod
        implicit none
        
        integer, parameter :: SIZE = 10
        integer, parameter :: FILL_VALUE = 42
        real, parameter :: PI = 3.14159
        real, parameter :: PI_OVER_4 = PI/4
        real, parameter :: PI_OVER_2 = PI/2
        
        integer :: array(SIZE)
        real :: angles(4)
        
        ! Use parameter for array size and fill value
        data array /SIZE*FILL_VALUE/
        
        ! Use parameters in values (no expressions allowed in DATA)
        data angles /0.0, PI_OVER_4, PI_OVER_2, PI/
        
    end module param_data_mod
    """
        )
        
        result = extract_module_data([Path("/fake/path/param_data.f90")])
        self.assertEqual(len(result), 1)
        module_data = result[0]
        
        # Check variables were initialized
        variables = module_data["variables"]
        
        # Check array initialization with parameters - should preserve exact format
        array_value = variables["array"]["initial_value"]
        self.assertEqual(array_value, "SIZE*FILL_VALUE")
        
        # Check angles initialization with PI parameter - should preserve parameter names
        angles_value = variables["angles"]["initial_value"]
        self.assertEqual(angles_value, "0.0, PI_OVER_4, PI_OVER_2, PI")
        
        # Verify the parameters were also captured
        parameters = module_data["parameters"]
        self.assertEqual(parameters["SIZE"]["value"], "10")
        self.assertEqual(parameters["FILL_VALUE"]["value"], "42")
        self.assertEqual(parameters["PI"]["value"], "3.14159")
        self.assertEqual(parameters["PI_OVER_4"]["value"], "PI / 4")
        self.assertEqual(parameters["PI_OVER_2"]["value"], "PI / 2")
        
        # Verify array dimension uses parameter
        array_dim = variables["array"]["dimension"]
        self.assertEqual(array_dim["dimensions"],
                         [ArrayBound(BoundType.VARIABLE, 
                                     Expression(ExpressionType.LITERAL, "1"), 
                                     Expression(ExpressionType.VARIABLE, "SIZE"))])

    def test_mixed_common_block_complexity(self):
        """Test complex initialization across multiple common blocks."""
        self.fs.create_file(
            "/fake/path/mixed_common.f90",
            contents="""\
    module mixed_common_mod
        implicit none
        
        integer :: a, b, c
        real :: x, y, z
        integer :: matrix(3,3)
        
        ! Multiple common blocks
        common /block1/ a, b, c
        common /block2/ x, y, z
        common /block3/ matrix
        
        ! Initialize variables from different blocks in one statement
        data a, x, matrix(1,1) /10, 1.5, 99/
        
        ! Initialize remaining variables
        data b, c /20, 30/
        data y, z /2.5, 3.5/
        data matrix(1,2), matrix(1,3) /88, 77/
        data matrix(2:3, 1:3) /18*0/
        
    end module mixed_common_mod
    """
        )
        
        result = extract_module_data([Path("/fake/path/mixed_common.f90")])
        self.assertEqual(len(result), 1)
        module_data = result[0]
        
        # Check common blocks and variables were initialized
        common_blocks = module_data["common_blocks"]
        self.assertIn("block1", common_blocks)
        self.assertIn("block2", common_blocks)
        self.assertIn("block3", common_blocks)
        
        # Check cross-block initialization
        self.assertEqual(common_blocks["block1"]["variables"]["a"]["initial_value"], "10")
        self.assertEqual(common_blocks["block2"]["variables"]["x"]["initial_value"], "1.5")
        
        # Matrix is more complex due to partial initialization
        matrix_var = common_blocks["block3"]["variables"]["matrix"]
        self.assertIsNotNone(matrix_var["initial_value"])

    def test_character_positioning_and_padding(self):
        """Test complex character positioning and padding."""
        self.fs.create_file(
            "/fake/path/char_positioning.f90",
            contents="""\
    module char_positioning_mod
        implicit none
        
        character(len=80) :: line
        character(len=10) :: words(5)
        
        ! Complex substring initialization
        data line(1:10) /'Header    '/
        data line(11:20) /'Data      '/
        data line(21:30) /'End       '/
        
        ! Padding considerations
        data words /'A', 'BB', 'CCC', 'DDDD', 'EEEEE'/
        
    end module char_positioning_mod
    """
        )
        
        result = extract_module_data([Path("/fake/path/char_positioning.f90")])
        self.assertEqual(len(result), 1)
        module_data = result[0]
        
        # Check variables were initialized
        variables = module_data["variables"]
        
        # Check line initialization (partial through substrings)
        line_value = variables["line"]["initial_value"]
        self.assertIsNotNone(line_value)
        
        # Check words array with padding
        words_value = variables["words"]["initial_value"]
        self.assertIn("'A'", words_value)
        self.assertIn("'BB'", words_value)
        self.assertIn("'CCC'", words_value)
        self.assertIn("'DDDD'", words_value)
        self.assertIn("'EEEEE'", words_value)

    def test_kind_specifiers_in_data(self):
        """Test DATA statements with kind specifiers."""
        self.fs.create_file(
            "/fake/path/kind_specifiers.f90",
            contents="""\
    module kind_specifiers_mod
        implicit none
        
        ! Different precision types
        real(kind=8) :: double_arr(5)
        integer(kind=4) :: long_int(3)
        
        ! Initialize with kind specifiers
        data double_arr /5*1.0d0/
        data long_int /1_4, 2_4, 3_4/
        
    end module kind_specifiers_mod
    """
        )
        
        result = extract_module_data([Path("/fake/path/kind_specifiers.f90")])
        self.assertEqual(len(result), 1)
        module_data = result[0]
            
        # Check variables were initialized
        variables = module_data["variables"]
        
        # Check double precision array
        self.assertEqual(variables["double_arr"]["kind"], "8")
        self.assertEqual(variables["double_arr"]["initial_value"], "5*1.0D0")
        
        # Check long integers
        self.assertEqual(variables["long_int"]["kind"], "4")
        self.assertEqual(variables["long_int"]["initial_value"], "1, 2, 3")

if __name__ == "__main__":
    unittest.main()