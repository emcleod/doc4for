import unittest
from pathlib import Path
from pyfakefs.fake_filesystem_unittest import TestCase
from doc4for.f90.generate_file_tree import extract_file_data
from doc4for.f90.generate_module_tree import extract_module_data
from doc4for.models.common import BindingTypeEnum
from doc4for.models.dimension_models import BoundType

class TestCommonBlockDeclarations(TestCase):

    def setUp(self):
        self.setUpPyfakefs()

    def test_common_block_basic_in_module(self):
        self.fs.create_file(
            "/fake/path/common_in_module.f90",
            contents="""\
module common_test_mod
    implicit none
    
    !!* Shared physics constants *!
    real :: pi, gravity, light_speed
    
    !!* Basic common block for physical constants *!
    common /physics/ pi, gravity, light_speed
    
    !!* Mesh parameters *!
    integer :: nx, ny, nz
    real :: dx, dy, dz
    
    !!* Grid parameters common block *!
    common /grid_params/ nx, ny, nz, dx, dy, dz
    
contains
    subroutine init_values()
        pi = 3.14159
        gravity = 9.81
        light_speed = 299792458.0
    end subroutine
end module
"""
        )
        
        result = extract_module_data([Path('/fake/path/common_in_module.f90')])
        self.assertEqual(len(result), 1)
        module = result[0]
        
        # Check basic module structure
        self.assertEqual(module["file_name"], "/fake/path/common_in_module.f90")
        
        # Check common blocks
        self.assertIn("common_blocks", module)
        self.assertEqual(len(module["common_blocks"]), 2)
        
        # Physics common block
        physics_block = module["common_blocks"]["physics"]
        self.assertEqual(physics_block["name"], "physics")
        self.assertEqual(physics_block["description"], "Basic common block for physical constants\n")
        self.assertIsNone(physics_block["binding_type"])
        self.assertEqual(len(physics_block["variables"]), 3)
        self.assertIn("pi", physics_block["variables"])
        self.assertIn("gravity", physics_block["variables"])
        self.assertIn("light_speed", physics_block["variables"])
        
        # Grid parameters common block
        grid_block = module["common_blocks"]["grid_params"]
        self.assertEqual(grid_block["name"], "grid_params")
        self.assertEqual(grid_block["description"], "Grid parameters common block\n")
        self.assertIsNone(grid_block["binding_type"])
        self.assertEqual(len(grid_block["variables"]), 6)
        self.assertIn("nx", grid_block["variables"])
        self.assertIn("dy", grid_block["variables"])

    def test_common_block_basic_in_block_data(self):
        self.fs.create_file(
            "/fake/path/common_in_block_data.f90",
            contents="""\
!!* Initialization of global constants *!
block data global_constants
    implicit none
    
    !!* Shared physics constants *!
    real :: pi, gravity, light_speed
    
    !!* 
    ! Basic common block for physical constants 
    !*!
    common /physics/ pi, gravity, light_speed
    
    !!* Mesh parameters *!
    integer :: nx, ny, nz
    real :: dx, dy, dz
    
    !!* Grid parameters common block *!
    common /grid_params/ nx, ny, nz, dx, dy, dz
    
    data pi /3.14159/
    data gravity /9.81/
    data light_speed /299792458.0/
    
    data nx, ny, nz /100, 100, 50/
    data dx, dy, dz /0.1, 0.1, 0.2/
end block data
"""
        )
        
        result = extract_file_data([Path('/fake/path/common_in_block_data.f90')])
        self.assertEqual(len(result), 1)
        file_data = result[0]
        
        # Check basic block data structure
        self.assertEqual(file_data["file_name"], "/fake/path/common_in_block_data.f90")
        self.assertIn("block_data", file_data)
        self.assertEqual(len(file_data["block_data"]), 1)
        block_data = file_data["block_data"]["global_constants"]
        
        # Check common blocks
        self.assertIn("common_blocks", block_data)
        self.assertEqual(len(block_data["common_blocks"]), 2)
        
        # Physics common block
        physics_block = block_data["common_blocks"]["physics"]
        self.assertEqual(physics_block["name"], "physics")
        self.assertEqual(physics_block["description"], "Basic common block for physical constants\n")
        self.assertIsNone(physics_block["binding_type"])
        self.assertEqual(len(physics_block["variables"]), 3)
        self.assertIn("pi", physics_block["variables"])
        self.assertIn("gravity", physics_block["variables"])
        self.assertIn("light_speed", physics_block["variables"])
        
        # Grid parameters common block
        grid_block = block_data["common_blocks"]["grid_params"]
        self.assertEqual(grid_block["name"], "grid_params")
        self.assertEqual(grid_block["description"], "Grid parameters common block\n")
        self.assertIsNone(grid_block["binding_type"])
        self.assertEqual(len(grid_block["variables"]), 6)
        self.assertIn("nx", grid_block["variables"])
        self.assertIn("dy", grid_block["variables"])

    def test_common_block_bindc_in_module(self):
        self.fs.create_file(
            "/fake/path/bindc_common_in_module.f90",
            contents="""\
module bindc_common_test
    use iso_c_binding
    implicit none
    
    !!* C-compatible physical constants *!
    real(c_double) :: pi, gravity, light_speed
    
    !!* Common block with explicit C binding and name *!
    common /physics/ pi, gravity, light_speed
    bind(c, name='c_physics_block') :: /physics/
    
    !!* Integration parameters *!
    integer(c_int) :: max_iter, method_type
    real(c_double) :: tolerance
    
    !!* Common block with C binding but default name *!
    common /solver_params/ max_iter, method_type, tolerance
    bind(c) :: /solver_params/
    
contains
    subroutine init_values() bind(c, name='init_physics')
        pi = 3.14159265358979d0
        gravity = 9.81d0
        light_speed = 299792458.0d0
    end subroutine
end module
"""
        )
        
        result = extract_file_data([Path('/fake/path/bindc_common_in_module.f90')])
        self.assertEqual(len(result), 1)
        file_data = result[0]
        
        # Check module structure
        self.assertIn("modules", file_data)  
        self.assertEqual(len(file_data["modules"]), 1)
        module = file_data["modules"]["bindc_common_test"]
        
        # Check common blocks
        self.assertIn("common_blocks", module)
        self.assertEqual(len(module["common_blocks"]), 2)
        
        # Physics common block with BIND(C)
        physics_block = module["common_blocks"]["physics"]
        self.assertEqual(physics_block["name"], "physics")
        self.assertEqual(physics_block["description"], "Common block with explicit C binding and name\n")
        self.assertEqual(physics_block["binding_type"]["type"], BindingTypeEnum.BIND_C)
        self.assertEqual(physics_block["binding_type"]["name"], "c_physics_block")
        self.assertEqual(len(physics_block["variables"]), 3)
        
        # Solver params common block with BIND(C) and default name
        solver_block = module["common_blocks"]["solver_params"]
        self.assertEqual(solver_block["name"], "solver_params")
        self.assertEqual(solver_block["description"], "Common block with C binding but default name\n")
        self.assertEqual(solver_block["binding_type"]["type"], BindingTypeEnum.BIND_C)
        self.assertIsNone(solver_block["binding_type"]["name"])
        self.assertEqual(len(solver_block["variables"]), 3)

    def test_common_block_bindc_in_block_data(self):
        self.fs.create_file(
            "/fake/path/bindc_common_in_block_data.f90",
            contents="""\
!!* Initialization of C-compatible constants *!
block data c_constants
    use iso_c_binding
    implicit none
    
    !!* C-compatible physical constants *!
    real(c_double) :: pi, gravity, light_speed
    
    !!* Common block with explicit C binding and name *!
    common /physics/ pi, gravity, light_speed
    bind(c, name='c_physics_block') :: /physics/
    
    !!* Integration parameters *!
    integer(c_int) :: max_iter, method_type
    real(c_double) :: tolerance
    
    !!* 
    ! Common block with C binding but default name 
    !*!
    common /solver_params/ max_iter, method_type, tolerance
    bind(c) :: /solver_params/
    
    data pi /3.14159265358979d0/
    data gravity /9.81d0/
    data light_speed /299792458.0d0/
    
    data max_iter /1000/
    data method_type /2/
    data tolerance /1.0d-6/
end block data
"""
        )
        
        result = extract_file_data([Path('/fake/path/bindc_common_in_block_data.f90')])
        self.assertEqual(len(result), 1)
        file_data = result[0]
        
        # Check block data structure
        self.assertIn("block_data", file_data)
        self.assertEqual(len(file_data["block_data"]), 1)
        block_data = file_data["block_data"]["c_constants"]
        
        # Check common blocks
        self.assertIn("common_blocks", block_data)
        self.assertEqual(len(block_data["common_blocks"]), 2)
        
        # Physics common block with BIND(C)
        physics_block = block_data["common_blocks"]["physics"]
        self.assertEqual(physics_block["name"], "physics")
        self.assertEqual(physics_block["description"], "Common block with explicit C binding and name\n")
        self.assertEqual(physics_block["binding_type"]["type"], BindingTypeEnum.BIND_C)
        self.assertEqual(physics_block["binding_type"]["name"], "c_physics_block")
        self.assertEqual(len(physics_block["variables"]), 3)
        
        # Solver params common block with BIND(C) and default name
        solver_block = block_data["common_blocks"]["solver_params"]
        self.assertEqual(solver_block["name"], "solver_params")
        self.assertEqual(solver_block["description"], "Common block with C binding but default name\n")
        self.assertEqual(solver_block["binding_type"]["type"], BindingTypeEnum.BIND_C)
        self.assertIsNone(solver_block["binding_type"]["name"])
        self.assertEqual(len(solver_block["variables"]), 3)
    
    def test_common_block_with_unusual_spacing(self):
        self.fs.create_file(
            "/fake/path/unusual_spacing.f90",
            contents="""\
module weird_formatting
    use iso_c_binding
    implicit none
    
    !!* Testing weird formatting with unusual spacing *!
    real(c_double) :: a, b, c
    
    common   /  weird_block  /   a  ,  b  ,  c  
    bind  (  c  ,  name = "c_weird"  ) :: /weird_block/
    
    !!* Multiple blocks on one line *!
    integer :: x, y, z, p, q
    common /block1/ x, y, z    /block2/ p, q
    bind(c) :: /block1/    
    bind(c, name='c_block2') :: /block2/
end module
"""
        )
        
        result = extract_file_data([Path('/fake/path/unusual_spacing.f90')])
        self.assertEqual(len(result), 1)
        file_data = result[0]
        module = file_data["modules"]["weird_formatting"]
        
        # Check weird spacing block
        weird_block = module["common_blocks"]["weird_block"]
        self.assertEqual(weird_block["name"], "weird_block")
        self.assertEqual(weird_block["binding_type"]["type"], BindingTypeEnum.BIND_C)
        self.assertEqual(weird_block["binding_type"]["name"], "c_weird")
        self.assertEqual(len(weird_block["variables"]), 3)
        
        # Check multiple blocks on one line
        block1 = module["common_blocks"]["block1"]
        self.assertEqual(block1["name"], "block1")
        self.assertEqual(block1["binding_type"]["type"], BindingTypeEnum.BIND_C)
        self.assertIsNone(block1["binding_type"]["name"])
        self.assertEqual(len(block1["variables"]), 3)
        
        block2 = module["common_blocks"]["block2"]
        self.assertEqual(block2["name"], "block2")
        self.assertEqual(block2["binding_type"]["type"], BindingTypeEnum.BIND_C)
        self.assertEqual(block2["binding_type"]["name"], "c_block2")
        self.assertEqual(len(block2["variables"]), 2)

    def test_blank_common_block(self):
        self.fs.create_file(
            "/fake/path/blank_common.f90",
            contents="""\
module blank_common_test
    implicit none
    
    !!* Variables in blank common block *!
    real :: a, b, c
    integer :: i, j
    
    !!* Blank common block *!
    common a, b, c, i, j
    
    !!* Named and blank on same line *!
    real :: x, y, z, p, q
    common /named/ x, y, z, // p, q
end module
"""
        )
        
        result = extract_file_data([Path('/fake/path/blank_common.f90')])
        self.assertEqual(len(result), 1)
        file_data = result[0]
        module = file_data["modules"]["blank_common_test"]
        
        # Check blank common block
        blank_common = module["common_blocks"][""]
        self.assertEqual(blank_common["name"], "")
        self.assertEqual(blank_common["description"], "Blank common block\n")
        self.assertIsNone(blank_common["binding_type"])
        self.assertEqual(len(blank_common["variables"]), 7)
        self.assertIn("a", blank_common["variables"])
        self.assertIn("b", blank_common["variables"])
        self.assertIn("c", blank_common["variables"])
        self.assertIn("i", blank_common["variables"])
        self.assertIn("j", blank_common["variables"])
        # Blank common should also have p and q from the second line
        self.assertIn("p", blank_common["variables"])
        self.assertIn("q", blank_common["variables"])
        
        # Check named block and trailing blank common on same line
        named_block = module["common_blocks"]["named"]
        self.assertEqual(named_block["name"], "named")
        self.assertEqual(len(named_block["variables"]), 3)
        
    def test_save_single_variable(self):
        self.fs.create_file(
            "/fake/path/save_common_module.f90",
            contents="""\
module save_test_module
    implicit none
    
    !!* Counter variables that persist between calls *!
    integer :: call_count, error_count
    real :: total_time
    save total_time

    !!* Stateful counters in common block *!
    common /counters/ call_count, error_count, total_time
    
    !!* Another stateful block with inline save *!
    real :: state_values(10)
    common /state/ state_values

contains
    subroutine increment_counter()
        call_count = call_count + 1
    end subroutine
end module
"""
        )
        
        result = extract_file_data([Path('/fake/path/save_common_module.f90')])
        file_data = result[0]
        module = file_data["modules"]["save_test_module"]
   
        variables = module["variables"]
        for name, desc in variables.items():
            if name == "total_time":
                self.assertTrue(desc["is_saved"])
            else:
                self.assertFalse(desc["is_saved"])

    def test_save_common_block(self):
        self.fs.create_file(
            "/fake/path/save_common_module.f90",
            contents="""\
module save_test_module
    implicit none
    
    !!* Counter variables that persist between calls *!
    integer :: call_count, error_count
    real :: total_time

    !!* Stateful counters in common block *!
    common /counters/ call_count, error_count, total_time
    save /counters/

    !!* Another stateful block with inline save *!
    real :: state_values(10)
    common /state/ state_values

contains
    subroutine increment_counter()
        call_count = call_count + 1
    end subroutine
end module
"""
        )
        
        result = extract_file_data([Path('/fake/path/save_common_module.f90')])
        file_data = result[0]
        module = file_data["modules"]["save_test_module"]

        variables_in_counters = ["call_count, error_count", "total_time"]
        variables_in_state = ["state_values"]

        variables = module["variables"]
        for name, desc in variables.items():
            if name in variables_in_counters:
                self.assertTrue(desc["is_saved"])
            elif name in variables_in_state:
                self.assertFalse(desc["is_saved"])

    def test_save_module(self):
        self.fs.create_file(
            "/fake/path/save_common_module.f90",
            contents="""\
module save_test_module
    implicit none
    
    !!* Counter variables that persist between calls *!
    integer :: call_count, error_count
    real :: total_time

    !!* Stateful counters in common block *!
    common /counters/ call_count, error_count, total_time
    save /counters/  ! SAVE attribute applied to common block
    
    !!* Another stateful block with inline save *!
    real :: state_values(10)
    common /state/ state_values
    save
contains
    subroutine increment_counter()
        call_count = call_count + 1
    end subroutine
end module
"""
        )
        
        result = extract_file_data([Path('/fake/path/save_common_module.f90')])
        file_data = result[0]
        module = file_data["modules"]["save_test_module"]
                
        variables = module["variables"]
        for _, desc in variables.items():
            self.assertTrue(desc["is_saved"])

    def test_save_in_block_data(self):
        self.fs.create_file(
            "/fake/path/save_common_block_data.f90",
            contents="""\
    !!* Global persistent storage *!
    block data persistence
        implicit none
        
        !!* Game state variables *!
        integer :: score, lives, level
        real :: elapsed_time
        
        !!* Persistent game state *!
        common /game_state/ score, lives, level, elapsed_time
        save /game_state/
        
        data score /0/
        data lives /3/
        data level /1/
        data elapsed_time /0.0/
    end block data
    """
        )
        
        result = extract_file_data([Path('/fake/path/save_common_block_data.f90')])
        file_data = result[0]
        block_data = file_data["block_data"]["persistence"]
        
        # Check that all variables in the game_state common block are saved
        game_state_block = block_data["common_blocks"]["game_state"]
        variables_in_game_state = ["score", "lives", "level", "elapsed_time"]
        
        # Variables are stored within the common block itself
        for var_name in variables_in_game_state:
            self.assertIn(var_name, game_state_block["variables"])
            var_desc = game_state_block["variables"][var_name]
            self.assertTrue(var_desc["is_saved"])

    def test_multiple_common_blocks_single_statement(self):
        self.fs.create_file(
            "/fake/path/multiple_commons.f90",
            contents="""\
    module multiple_commons
        implicit none
        
        !!* First block with scalars.
        ! Second block with array 
        ! Third block with single variable 
        !*!
        common /block1/ var1, var2 /block2/ var3, var4(10) /block3/ var5
        real :: var1, var2, var3, var4, var5
        
    end module
    """
        )
        
        result = extract_file_data([Path('/fake/path/multiple_commons.f90')])
        file_data = result[0]
        module = file_data["modules"]["multiple_commons"]
        
        # Check all three common blocks exist
        self.assertEqual(len(module["common_blocks"]), 3)
        
        # Block1 - two scalars
        block1 = module["common_blocks"]["block1"]
        self.assertEqual(block1["description"], "First block with scalars.\nSecond block with array\nThird block with single variable\n")
        self.assertEqual(len(block1["variables"]), 2)
        self.assertIn("var1", block1["variables"])
        self.assertIn("var2", block1["variables"])
        self.assertIsNone(block1["variables"]["var1"]["dimension"])
        self.assertIsNone(block1["variables"]["var2"]["dimension"])
        
        # Block2 - one scalar and one array
        block2 = module["common_blocks"]["block2"]
        self.assertEqual(block2["description"], "First block with scalars.\nSecond block with array\nThird block with single variable\n")
        self.assertEqual(len(block2["variables"]), 2)
        self.assertIn("var3", block2["variables"])
        self.assertIn("var4", block2["variables"])
        self.assertIsNone(block2["variables"]["var3"]["dimension"])
        
        # Check var4 array dimension
        var4_dim = block2["variables"]["var4"]["dimension"]
        self.assertIsNotNone(var4_dim)
        self.assertEqual(len(var4_dim["dimensions"]), 1)
        self.assertEqual(var4_dim["dimensions"][0].bound_type, BoundType.FIXED)
        self.assertEqual(var4_dim["dimensions"][0].upper.value, "10")
        
        # Block3 - single variable
        block3 = module["common_blocks"]["block3"]
        self.assertEqual(block3["description"], "First block with scalars.\nSecond block with array\nThird block with single variable\n")
        self.assertEqual(len(block3["variables"]), 1)
        self.assertIn("var5", block3["variables"])

    def test_mixed_named_and_blank_common_with_arrays(self):
        self.fs.create_file(
            "/fake/path/mixed_commons.f90",
            contents="""\
    module mixed_commons
        implicit none
        
        !!* Named data block followed by blank common
        ! with 2D array
        !*!
        common /data/ x, y // z, w(5,5)
        real :: x, y, z, w
        
    end module
    """
        )
        
        result = extract_file_data([Path('/fake/path/mixed_commons.f90')])
        file_data = result[0]
        module = file_data["modules"]["mixed_commons"]
        
        # Check two common blocks exist (named and blank)
        self.assertEqual(len(module["common_blocks"]), 2)
        
        # Named block
        data_block = module["common_blocks"]["data"]
        self.assertEqual(data_block["description"], "Named data block followed by blank common\nwith 2D array\n")
        self.assertEqual(len(data_block["variables"]), 2)
        self.assertIn("x", data_block["variables"])
        self.assertIn("y", data_block["variables"])
        
        # Blank block with 2D array
        blank_block = module["common_blocks"][""]
        self.assertEqual(len(blank_block["variables"]), 2)
        self.assertIn("z", blank_block["variables"])
        self.assertIn("w", blank_block["variables"])
        
        # Check 2D array dimensions
        w_dim = blank_block["variables"]["w"]["dimension"]
        self.assertIsNotNone(w_dim)
        self.assertEqual(len(w_dim["dimensions"]), 2)
        self.assertEqual(w_dim["dimensions"][0].bound_type, BoundType.FIXED)
        self.assertEqual(w_dim["dimensions"][0].upper.value, "5")
        self.assertEqual(w_dim["dimensions"][1].bound_type, BoundType.FIXED)
        self.assertEqual(w_dim["dimensions"][1].upper.value, "5")

    def test_array_bound_variations(self):
        self.fs.create_file(
            "/fake/path/bound_variations.f90",
            contents="""\
    module bound_variations
        implicit none
        
        !!* Various array bound specifications
        !*!
        common /bounds/ arr1(0:99), arr2(-5:5,1:10), arr3(100), arr4(50)
        real :: arr1, arr2, arr3, arr4
        
    end module
    """
        )
        
        result = extract_file_data([Path('/fake/path/bound_variations.f90')])
        file_data = result[0]
        module = file_data["modules"]["bound_variations"]
        
        bounds_block = module["common_blocks"]["bounds"]
        self.assertEqual(bounds_block["description"], "Various array bound specifications\n")
        
        # arr1(0:99) - explicit bounds
        arr1_dim = bounds_block["variables"]["arr1"]["dimension"]
        self.assertEqual(len(arr1_dim["dimensions"]), 1)
        self.assertEqual(arr1_dim["dimensions"][0].bound_type, BoundType.FIXED)
        self.assertEqual(arr1_dim["dimensions"][0].lower.value, "0")
        self.assertEqual(arr1_dim["dimensions"][0].upper.value, "99")
        
        # arr2(-5:5,1:10) - 2D with explicit bounds
        arr2_dim = bounds_block["variables"]["arr2"]["dimension"]
        self.assertEqual(len(arr2_dim["dimensions"]), 2)
        self.assertEqual(arr2_dim["dimensions"][0].bound_type, BoundType.FIXED)
        self.assertEqual(arr2_dim["dimensions"][0].lower.value, "-5")
        self.assertEqual(arr2_dim["dimensions"][0].upper.value, "5")
        self.assertEqual(arr2_dim["dimensions"][1].bound_type, BoundType.FIXED)
        self.assertEqual(arr2_dim["dimensions"][1].lower.value, "1")
        self.assertEqual(arr2_dim["dimensions"][1].upper.value, "10")
        
        # arr3(100) - simple fixed size
        arr3_dim = bounds_block["variables"]["arr3"]["dimension"]
        self.assertEqual(len(arr3_dim["dimensions"]), 1)
        self.assertEqual(arr3_dim["dimensions"][0].bound_type, BoundType.FIXED)
        self.assertEqual(arr3_dim["dimensions"][0].upper.value, "100")
        
        # arr4(50) - simple fixed size
        arr4_dim = bounds_block["variables"]["arr4"]["dimension"]
        self.assertEqual(len(arr4_dim["dimensions"]), 1)
        self.assertEqual(arr4_dim["dimensions"][0].bound_type, BoundType.FIXED)
        self.assertEqual(arr4_dim["dimensions"][0].upper.value, "50")

    def test_character_arrays_in_common(self):
        self.fs.create_file(
            "/fake/path/character_arrays.f90",
            contents="""\
    module character_arrays
        implicit none
        
        ! Regular comment - not part of description
        !!* Character variables and arrays with different length specs
        !*!
        common /chars/ str1, str_array, regular_char
        character(len=10) :: str1
        character(len=20) :: str_array(100)
        character(len=5) :: regular_char
        
    end module
    """
        )
        
        result = extract_file_data([Path('/fake/path/character_arrays.f90')])
        file_data = result[0]
        module = file_data["modules"]["character_arrays"]
        
        chars_block = module["common_blocks"]["chars"]
        self.assertEqual(chars_block["description"], "Character variables and arrays with different length specs\n")
        
        # str1 - character with length in type declaration
        str1 = chars_block["variables"]["str1"]
        self.assertEqual(str1["type"], "CHARACTER")
        self.assertEqual(str1["length"], "10")
        self.assertIsNone(str1["dimension"])
        
        # str_array - character array with length and dimension
        str_array = chars_block["variables"]["str_array"]
        self.assertEqual(str_array["type"], "CHARACTER")
        self.assertEqual(str_array["length"], "20")
        self.assertIsNotNone(str_array["dimension"])
        self.assertEqual(len(str_array["dimension"]["dimensions"]), 1)
        self.assertEqual(str_array["dimension"]["dimensions"][0].upper.value, "100")
        
        # regular_char - character with length
        regular_char = chars_block["variables"]["regular_char"]
        self.assertEqual(regular_char["type"], "CHARACTER")
        self.assertEqual(regular_char["length"], "5")
        self.assertIsNone(regular_char["dimension"])

    def test_multidimensional_arrays_in_common(self):
        self.fs.create_file(
            "/fake/path/multidim_arrays.f90",
            contents="""\
    module standard_arrays
        implicit none
        
        !!* Common block with various dimensional arrays
        ! including 1D, 2D and 3D arrays
        !*!
        common /data/ simple_var, array1d(100), array2d(50,50), array3d(10,10,10)
        real :: simple_var, array1d, array2d, array3d
        
    end module
    """
        )
        
        result = extract_file_data([Path('/fake/path/multidim_arrays.f90')])
        file_data = result[0]
        module = file_data["modules"]["standard_arrays"]
        
        data_block = module["common_blocks"]["data"]
        self.assertEqual(data_block["description"], "Common block with various dimensional arrays\nincluding 1D, 2D and 3D arrays\n")
        
        # Scalar
        self.assertIsNone(data_block["variables"]["simple_var"]["dimension"])
        
        # 1D array
        array1d_dim = data_block["variables"]["array1d"]["dimension"]
        self.assertEqual(len(array1d_dim["dimensions"]), 1)
        self.assertEqual(array1d_dim["dimensions"][0].upper.value, "100")
        
        # 2D array
        array2d_dim = data_block["variables"]["array2d"]["dimension"]
        self.assertEqual(len(array2d_dim["dimensions"]), 2)
        self.assertEqual(array2d_dim["dimensions"][0].upper.value, "50")
        self.assertEqual(array2d_dim["dimensions"][1].upper.value, "50")
        
        # 3D array
        array3d_dim = data_block["variables"]["array3d"]["dimension"]
        self.assertEqual(len(array3d_dim["dimensions"]), 3)
        self.assertEqual(array3d_dim["dimensions"][0].upper.value, "10")
        self.assertEqual(array3d_dim["dimensions"][1].upper.value, "10")
        self.assertEqual(array3d_dim["dimensions"][2].upper.value, "10")

#     def test_equivalence_with_common_in_module(self):
#         self.fs.create_file(
#             "/fake/path/equivalence_common_module.f90",
#             contents="""\
# module equivalence_test
#     implicit none
    
#     !!* Buffer that can be viewed as different types *!
#     real :: buffer(100)
#     integer :: int_view(100)
#     character(len=400) :: char_view
#     real :: first_element, last_element
    
#     !!* Make variables share same memory *!
#     equivalence (buffer, int_view, char_view)
#     equivalence (buffer(1), first_element)
#     equivalence (buffer(100), last_element)
    
#     !!* Common block containing the shared memory *!
#     common /shared_buffer/ buffer
# contains
#     subroutine write_string(text)
#         character(len=*), intent(in) :: text
#         char_view(1:len(text)) = text
#     end subroutine
# end module
# """
#         )
        
#         result = extract_file_data([Path('/fake/path/equivalence_common_module.f90')])
#         file_data = result[0]
#         module = file_data["modules"]["equivalence_test"]
        
#         # Check equivalence relationships in common block
#         buffer_block = module["common_blocks"]["shared_buffer"]
#         self.assertIn("equivalence_relationships", buffer_block)
        
#         # Should have 3 equivalence relationships
#         self.assertEqual(len(buffer_block["equivalence_relationships"]), 3)
        
#         # Check first relationship (buffer = int_view = char_view)
#         first_equiv = buffer_block["equivalence_relationships"][0]
#         self.assertEqual(first_equiv["base_variable"], "buffer")
#         self.assertIn("int_view", first_equiv["equivalent_variables"])
#         self.assertIn("char_view", first_equiv["equivalent_variables"])
        
#         # Check that variables in equivalence are marked
#         self.assertTrue(buffer_block["variables"]["buffer"]["has_equivalence"])
    
#     def test_equivalence_with_common_in_block_data(self):
#         self.fs.create_file(
#             "/fake/path/equivalence_common_block_data.f90",
#             contents="""\
# block data equiv_data
#     implicit none
    
#     !!* Union-like structure using equivalence *!
#     integer :: int_array(25)
#     real :: real_array(25)
#     complex :: complex_array(12)
#     character(len=100) :: string_view
    
#     !!* Create union-like memory sharing *!
#     equivalence (int_array, real_array, complex_array, string_view)
    
#     !!* Common block containing the shared memory *!
#     common /union_data/ int_array
    
#     data int_array /25*0/
# end block data
# """
#         )
        
#         result = extract_file_data([Path('/fake/path/equivalence_common_block_data.f90')])
#         file_data = result[0]
#         block_data = file_data["block_data"]["equiv_data"]
        
#         # Check equivalence relationships in common block
#         union_block = block_data["common_blocks"]["union_data"]
#         self.assertIn("equivalence_relationships", union_block)
        
#         # Should have 1 equivalence relationship with 4 variables
#         self.assertEqual(len(union_block["equivalence_relationships"]), 1)
        
#         equiv = union_block["equivalence_relationships"][0]
#         self.assertEqual(equiv["base_variable"], "int_array") 
#         self.assertEqual(len(equiv["equivalent_variables"]), 3)
#         self.assertIn("string_view", equiv["equivalent_variables"])
    
#     def test_save_and_equivalence_combined(self):
#         self.fs.create_file(
#             "/fake/path/save_equivalence_combined.f90",
#             contents="""\
# module combined_features
#     implicit none
    
#     !!* Persistent buffer with multiple views *!
#     real :: buffer(100)
#     integer :: int_buffer(100)
#     character(len=400) :: string_buffer
    
#     equivalence (buffer, int_buffer, string_buffer)
    
#     !!* Common block with both save and equivalenced variables *!
#     common /persistent_buffer/ buffer
#     save /persistent_buffer/
# contains
#     subroutine process_data()
#         buffer(1) = buffer(1) + 1.0
#     end subroutine
# end module
# """
#         )
        
#         result = extract_file_data([Path('/fake/path/save_equivalence_combined.f90')])
#         file_data = result[0]
#         module = file_data["modules"]["combined_features"]
        
#         # Check both features in the same common block
#         buffer_block = module["common_blocks"]["persistent_buffer"]
#         self.assertIn("has_save_attribute", buffer_block)
#         self.assertTrue(buffer_block["has_save_attribute"])
        
#         self.assertIn("equivalence_relationships", buffer_block)
#         self.assertEqual(len(buffer_block["equivalence_relationships"]), 1)
        
#         # Check that the buffer variable has both attributes
#         self.assertTrue(buffer_block["variables"]["buffer"]["has_equivalence"])
#         self.assertTrue(buffer_block["variables"]["buffer"]["is_saved"])
    
#     def test_partial_equivalence_with_common(self):
#         self.fs.create_file(
#             "/fake/path/partial_equivalence.f90",
#             contents="""\
# module partial_equiv_test
#     implicit none
    
#     !!* Common block with array variables *!
#     real :: full_array(100)
#     real :: first_half(50), second_half(50)
#     real :: first_element, last_element
    
#     !!* Partial equivalence with sections of array *!
#     equivalence (full_array(1), first_half(1))
#     equivalence (full_array(51), second_half(1))
#     equivalence (full_array(1), first_element)
#     equivalence (full_array(100), last_element)
    
#     common /array_data/ full_array
# contains
#     subroutine update_halves()
#         first_half = first_half * 2.0
#         second_half = second_half + 1.0
#         ! full_array is automatically updated due to equivalence
#     end subroutine
# end module
# """
#         )
        
#         result = extract_file_data([Path('/fake/path/partial_equivalence.f90')])
#         file_data = result[0]
#         module = file_data["modules"]["partial_equiv_test"]
        
#         # Check array subsection equivalence
#         array_block = module["common_blocks"]["array_data"]
#         self.assertIn("equivalence_relationships", array_block)
#         self.assertEqual(len(array_block["equivalence_relationships"]), 4)
        
#         # Check first relationship with offset info
#         first_relation = array_block["equivalence_relationships"][0]
#         self.assertEqual(first_relation["base_variable"], "full_array")
#         self.assertEqual(first_relation["base_offset"], 0)  # 1-indexed in Fortran
#         self.assertEqual(first_relation["equivalent_variables"][0], "first_half")
#         self.assertEqual(first_relation["equivalent_offsets"][0], 0)
        
#         # Check second relationship with offset info
#         second_relation = array_block["equivalence_relationships"][1]  
#         self.assertEqual(second_relation["base_variable"], "full_array")
#         self.assertEqual(second_relation["base_offset"], 50)  # 1-indexed in Fortran
#         self.assertEqual(second_relation["equivalent_variables"][0], "second_half")
#         self.assertEqual(second_relation["equivalent_offsets"][0], 0)

    # def test_common_block_with_save(self):
    #     self.fs.create_file(
    #         "/fake/path/common_with_save.f90",
    #         contents="""\
    # module saved_data
    #     implicit none
        
    #     !!* Constants that should persist between calls *!
    #     real :: constants(5)
    #     integer :: config_values(10)
        
    #     !!* Common block with SAVE attribute *!
    #     common /persisted_values/ constants, config_values
    #     save /persisted_values/
        
    #     real :: temp_history(100)
    #     integer :: counter
    #     !!* Another common block without explicit save *!
    #     common /history_data/ temp_history, counter
        
    #     real :: calibration(3), measurements(500)
    #     !!* COMMON with selective SAVE using :: syntax *!
    #     common /sensor_data/ calibration, measurements
    #     save :: calibration  ! Only calibration variable saved, not measurements
    # contains
    #     subroutine initialize()
    #         constants = [3.14159, 2.71828, 1.41421, 1.73205, 2.23607]
    #         config_values = 0
    #     end subroutine
    # end module
    # """
    #     )
        
    #     result = extract_file_data([Path('/fake/path/common_with_save.f90')])
    #     self.assertEqual(len(result), 1)
    #     file_data = result[0]
    #     module = file_data["modules"]["saved_data"]
        
    #     persisted_values = module["common_blocks"]["persisted_values"]
    #     self.assertEqual(persisted_values["description"], "Common block with SAVE attribute\n")
    #     history_data = module["common_blocks"]["history_data"]
    #     self.assertEqual(history_data["description"], "Another common block without explicit save\n")
    #     sensor_data = module["common_blocks"]["sensor_data"]
    #     self.assertEqual(sensor_data["description"], "COMMON with selective SAVE using :: syntax\n")

    #     # Variables that should be saved
    #     saved_variables = ["constants", "config_values", "calibration"]
    #     # Variables that should NOT be saved
    #     unsaved_variables = ["temp_history", "counter", "measurements"]
        
    #     variables = module["variables"]
        
    #     # Check saved variables
    #     for var_name in saved_variables:
    #         self.assertIn(var_name, variables)
    #         self.assertTrue(variables[var_name]["is_saved"])
        
    #     # Check unsaved variables
    #     for var_name in unsaved_variables:
    #         self.assertIn(var_name, variables)
    #         self.assertFalse(variables[var_name]["is_saved"])

#     def test_common_block_with_equivalence(self):
#         """Test that EQUIVALENCE relationships are properly captured."""
#         self.fs.create_file(
#             "/fake/path/common_with_equivalence.f90",
#             contents="""\
# module shared_memory
#     implicit none
    
#     !!* Multi-view data buffer *!
#     real :: data_buffer(1000)
#     integer :: int_view(1000)
#     character(len=4000) :: char_view
    
#     !!* Common block containing data buffer *!
#     common /data_storage/ data_buffer
    
#     !!* Make multiple views equivalent to the buffer *!
#     equivalence (data_buffer, int_view, char_view)
    
#     !!* Partial equivalence for subarray *!
#     real :: header(10), body(990)
#     equivalence (data_buffer, header), (data_buffer(11), body)
    
#     !!* Another common block with equivalenced members *!
#     complex :: complex_data(50)
#     real :: real_parts(50), imag_parts(50)
#     common /complex_storage/ complex_data
#     equivalence (complex_data, real_parts), (complex_data(1), imag_parts)
# end module
# """
#         )
        
#         result = extract_file_data([Path('/fake/path/common_with_equivalence.f90')])
#         self.assertEqual(len(result), 1)
#         file_data = result[0]
#         module = file_data["modules"]["shared_memory"]
        
#         # Check equivalence relationships for data_storage
#         data_block = module["common_blocks"]["data_storage"]
#         self.assertIn("equivalence_relationships", data_block)
        
#         # Check main equivalence relationship
#         self.assertGreaterEqual(len(data_block["equivalence_relationships"]), 1)
#         main_equiv = data_block["equivalence_relationships"][0]
#         self.assertEqual(main_equiv["base_variable"], "data_buffer")
#         self.assertIn("equivalent_variables", main_equiv)
#         equivalent_vars = main_equiv["equivalent_variables"]
#         self.assertIn("int_view", equivalent_vars)
#         self.assertIn("char_view", equivalent_vars)
        
#         # Check partial equivalence
#         self.assertIn("header", [rel.get("base_variable") for rel in data_block["equivalence_relationships"]])
        
#         # Check another common block with equivalence
#         complex_block = module["common_blocks"]["complex_storage"]
#         self.assertIn("equivalence_relationships", complex_block)
#         self.assertGreaterEqual(len(complex_block["equivalence_relationships"]), 1)
        
#     def test_combined_save_and_equivalence(self):
#         """Test SAVE and EQUIVALENCE used together with COMMON blocks."""
#         self.fs.create_file(
#             "/fake/path/combined_features.f90",
#             contents="""\
# module advanced_data_store
#     implicit none
    
#     !!* Persistent data buffer with multiple views *!
#     real :: buffer(500)
#     integer :: int_buffer(500)
#     character(len=2000) :: char_buffer
    
#     common /multi_view_data/ buffer
#     equivalence (buffer, int_buffer, char_buffer)
#     save /multi_view_data/
    
#     !!* Partial SAVE with EQUIVALENCE *!
#     real :: raw_readings(200), processed_readings(200)
#     integer :: status_flags(200)
    
#     common /sensor_readings/ raw_readings, processed_readings
#     equivalence (raw_readings, status_flags)
#     save :: raw_readings  ! Only raw readings are saved
# contains
#     subroutine update_data(new_value)
#         real, intent(in) :: new_value
#         integer :: i
        
#         ! Shift values and add new one
#         do i = 499, 1, -1
#             buffer(i+1) = buffer(i)
#         end do
#         buffer(1) = new_value
#     end subroutine
# end module
# """
#         )
        
#         result = extract_file_data([Path('/fake/path/combined_features.f90')])
#         self.assertEqual(len(result), 1)
#         file_data = result[0]
#         module = file_data["modules"]["advanced_data_store"]
        
#         # Check SAVE and EQUIVALENCE together
#         multi_view_block = module["common_blocks"]["multi_view_data"]
#         self.assertIn("has_save_attribute", multi_view_block)
#         self.assertTrue(multi_view_block["has_save_attribute"])
#         self.assertIn("equivalence_relationships", multi_view_block)
#         self.assertGreaterEqual(len(multi_view_block["equivalence_relationships"]), 1)
        
#         # Check complex case: partial SAVE with EQUIVALENCE
#         sensor_block = module["common_blocks"]["sensor_readings"]
#         self.assertIn("has_save_attribute", sensor_block)
#         self.assertTrue(sensor_block["has_save_attribute"])
#         self.assertIn("partially_saved", sensor_block)
#         self.assertTrue(sensor_block["partially_saved"])
#         self.assertIn("saved_variables", sensor_block)
#         self.assertIn("raw_readings", sensor_block["saved_variables"])
        
#         # Check that equivalenced variables inherit SAVE status
#         self.assertIn("equivalence_relationships", sensor_block)
#         relations = sensor_block["equivalence_relationships"]
#         for relation in relations:
#             if relation["base_variable"] == "raw_readings":
#                 self.assertIn("inherits_save_status", relation)
#                 self.assertTrue(relation["inherits_save_status"])
#                 self.assertIn("status_flags", relation["equivalent_variables"])


#     def test_equivalence_in_block_data(self):
#         """Test that EQUIVALENCE in block data is properly captured."""
#         self.fs.create_file(
#             "/fake/path/block_data_with_equivalence.f90",
#             contents="""\
# block data converter_init
#     implicit none
    
#     real :: table(100)
#     integer :: lookup_codes(100)
#     character(len=400) :: error_messages
    
#     common /lookup_data/ table
#     equivalence (table, lookup_codes, error_messages)
    
#     data table /100*0.0/
#     data lookup_codes /100*-1/
#     data error_messages /'No errors'/
# end block data
# """
#         )
        
#         result = extract_file_data([Path('/fake/path/block_data_with_equivalence.f90')])
#         self.assertEqual(len(result), 1)
#         file_data = result[0]
#         block_data = file_data["block_data"]["converter_init"]
        
#         # Check EQUIVALENCE in block data
#         lookup_block = block_data["common_blocks"]["lookup_data"]
#         self.assertIn("equivalence_relationships", lookup_block)
#         self.assertGreaterEqual(len(lookup_block["equivalence_relationships"]), 1)
#         self.assertEqual(lookup_block["equivalence_relationships"][0]["base_variable"], "table")
#         self.assertIn("lookup_codes", lookup_block["equivalence_relationships"][0]["equivalent_variables"])
#         self.assertIn("error_messages", lookup_block["equivalence_relationships"][0]["equivalent_variables"])

if __name__ == "__main__":
    unittest.main()


#     CommonBlockDescription = TypedDict(
#     "CommonBlockDescription",
#     {
#         # existing fields
#         "has_save_attribute": bool,
#         "partially_saved": bool,
#         "saved_variables": List[str],
#         "equivalence_relationships": List[Dict],
#     }
# )

# real :: data_buffer(100), processed_data(100)
# integer :: int_view(100)
# common /data_block/ data_buffer, processed_data
# equivalence (data_buffer, int_view)
# save :: data_buffer



