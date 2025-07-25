import unittest
from pathlib import Path
from pyfakefs.fake_filesystem_unittest import TestCase
from doc4for.f90.generate_file_tree import extract_file_data

class TestComplexDataInitializations(TestCase):
    
    def setUp(self):
        self.setUpPyfakefs()
    
    def test_array_initializations(self):
        self.fs.create_file(
            "/fake/path/array_data.f90",
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
        
        result = extract_file_data([Path('/fake/path/array_data.f90')])
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
        
    def test_multiple_variables_single_data(self):
        self.fs.create_file(
            "/fake/path/multi_var_data.f90",
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
        
        result = extract_file_data([Path('/fake/path/multi_var_data.f90')])
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
            
    def test_implied_do_loops(self):
        self.fs.create_file(
            "/fake/path/implied_do_data.f90",
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
        
        result = extract_file_data([Path('/fake/path/implied_do_data.f90')])
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

    def test_character_and_substring(self):
        self.fs.create_file(
            "/fake/path/character_data.f90",
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
        
        result = extract_file_data([Path('/fake/path/character_data.f90')])
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
        self.assertEqual(codes_value, "'ABC123', 'DEF456', 'GHI789'")
        
        # Test substring initialization 
        partial_value = variables["partial_init"]["initial_value"]
        self.assertIsNotNone(partial_value)
        self.assertTrue(
            any(text in partial_value for text in ["First ten ", "characters"]),
            f"Expected 'First ten ' or 'characters' in {partial_value}"
        )


if __name__ == "__main__":
    unittest.main()