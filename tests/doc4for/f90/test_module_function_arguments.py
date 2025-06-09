import unittest
from pathlib import Path
from pyfakefs.fake_filesystem_unittest import TestCase
from doc4for.f90.generate_module_tree import extract_module_data
from doc4for.models.dimension_models import ArrayBound, BoundType
from doc4for.models.variable_models import PolymorphismType

class TestFunctionArguments(TestCase):
    maxDiff=None

    def setUp(self):
        self.setUpPyfakefs()

    def test_function_with_no_args(self):
        self.fs.create_file(
            "/fake/path/scalar_args.f90",
            contents="""\
    module no_args_functions
    contains
    !!*
    ! A function with no arguments
    !*!
    function add_numbers()
        real :: x
        real :: y
        real :: add_numbers
        x = 2
        y = 3
        add_numbers = x + y
    end function add_numbers
    end module no_args_functions
                            """,
        )
        result = extract_module_data([Path("/fake/path/scalar_args.f90")])
        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(module["module_name"], "no_args_functions")
        self.assertEqual(len(module["functions"]), 1)
        self.assertIn("add_numbers", module["functions"])
        function = module["functions"]["add_numbers"]
        inputs = function["in"]
        outputs = function["out"]
        returns = function["return"]
        self.assertEqual(len(inputs), 0)
        self.assertEqual(len(outputs), 0)
        expected_returns = {
            "type": "REAL",
            "description": "",
            "dimension": None,
            "enum_type": None,
            "interface_name": None,
            "attributes": [],
            "default_value": None,
            "kind": None,
            "length": None, 
            "polymorphism_type": PolymorphismType.NONE
        }
        self.assertEqual(returns, expected_returns)
        self.assertEqual(function["arguments"], [])

    def test_function_with_scalar_args_1(self):
        self.fs.create_file(
            "/fake/path/scalar_args.f90",
            contents="""\
    module scalar_args_functions
    contains
    !!*
    ! A function with scalar arguments
    !*!
    function add_numbers(x, y) result(res)
        real, intent(in) :: x, y
        real :: res
        res = x + y
    end function add_numbers
    end module scalar_args_functions
                            """,
        )
        result = extract_module_data([Path("/fake/path/scalar_args.f90")])

        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(module["module_name"], "scalar_args_functions")
        self.assertEqual(len(module["functions"]), 1)
        self.assertIn("add_numbers", module["functions"])
        function = module["functions"]["add_numbers"]
        inputs = function["in"]
        outputs = function["out"]
        results = function["return"]
        self.assertEqual(len(inputs), 2)
        self.assertEqual(inputs["x"], {"type": "REAL", 
                                       "description": "", 
                                       "dimension": None, 
                                       "enum_type": None, 
                                       "interface_name": None,
                                        "attributes": [],
                                        "default_value": None,
                                        "kind": None,
                                        "length": None, 
                                        "polymorphism_type": PolymorphismType.NONE                                                                              
                                       })
        self.assertEqual(inputs["y"], {"type": "REAL", 
                                       "description": "", 
                                       "dimension": None, 
                                       "enum_type": None, 
                                       "interface_name": None,            
                                       "attributes": [],
                                        "default_value": None,
                                        "kind": None,
                                        "length": None, 
                                        "polymorphism_type": PolymorphismType.NONE
                                       })
        self.assertCountEqual(function["arguments"], ["x", "y"])
        self.assertEqual(len(outputs), 0)
        self.assertEqual(results, {"type": "REAL", 
                                   "description": "", 
                                   "dimension": None,
                                   "enum_type": None, 
                                   "interface_name": None,
                                    "attributes": [],
                                    "default_value": None,
                                    "kind": None,
                                    "length": None, 
                                    "polymorphism_type": PolymorphismType.NONE
                                   })

    def test_function_with_return_prefix(self):
        self.fs.create_file(
            "/fake/path/scalar_args.f90",
            contents="""\
    module functions
    contains
    !!*
    ! A function with return type set in the declaration
    !*!
    real function add_numbers(a, b)
        real, intent(in) :: a, b
        add_numbers = x + y
    end function add_numbers
    end module functions
                            """,
        )
        result = extract_module_data([Path("/fake/path/scalar_args.f90")])
        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(module["module_name"], "functions")
        self.assertEqual(len(module["functions"]), 1)
        self.assertIn("add_numbers", module["functions"])
        function = module["functions"]["add_numbers"]
        self.assertEqual(function["description"], "A function with return type set in the declaration\n")
        inputs = function["in"]
        outputs = function["out"]
        returns = function["return"]
        self.assertEqual(len(inputs), 2)
        self.assertEqual(len(outputs), 0)
        expected_returns = {
            "type": "REAL",
            "description": "",
            "dimension": None,
            "enum_type": None,
            "interface_name": None,
            "attributes": [],
            "length": None,
            "kind": None,
            "default_value": None,
            "polymorphism_type": PolymorphismType.NONE
        }
        self.assertEqual(returns, expected_returns)
        self.assertEqual(function["arguments"], ["a", "b"])

    def test_function_with_scalar_args_2(self):
        self.fs.create_file(
            "/fake/path/scalar_args.f90",
            contents="""\
    module scalar_args_functions
    contains
    !!*
    ! A function with scalar arguments
    !*!
    function add_numbers(x, y) result(res)
        real, intent(in) :: x
        real, intent(in) :: y
        real :: res
        res = x + y
    end function add_numbers
    end module scalar_args_functions
                            """,
        )
        result = extract_module_data([Path("/fake/path/scalar_args.f90")])

        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(module["module_name"], "scalar_args_functions")
        self.assertEqual(len(module["functions"]), 1)
        self.assertIn("add_numbers", module["functions"])
        function = module["functions"]["add_numbers"]
        self.assertEqual(function["description"], "A function with scalar arguments\n")
        inputs = function["in"]
        outputs = function["out"]
        results = function["return"]
        self.assertEqual(len(inputs), 2)
        self.assertEqual(inputs["x"]["type"], "REAL")
        self.assertEqual(inputs["y"]["type"], "REAL")
        self.assertEqual(len(outputs), 0)
        self.assertEqual(results["type"], "REAL")

    def test_function_with_scalar_args_3(self):
        self.fs.create_file(
            "/fake/path/scalar_args.f90",
            contents="""\
    module scalar_args_functions
    contains
    !!*
    ! A function with scalar arguments
    !*!
    function add_numbers(x, y) 
        real, intent(in) :: x
        real, intent(in) :: y
        real :: add_numbers
        add_numbers = x + y
    end function add_numbers
    end module scalar_args_functions
                            """,
        )
        result = extract_module_data([Path("/fake/path/scalar_args.f90")])

        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(module["module_name"], "scalar_args_functions")
        self.assertEqual(len(module["functions"]), 1)
        self.assertIn("add_numbers", module["functions"])
        function = module["functions"]["add_numbers"]
        self.assertEqual(function["description"], "A function with scalar arguments\n")
        inputs = function["in"]
        outputs = function["out"]
        results = function["return"]
        self.assertEqual(len(inputs), 2)
        self.assertEqual(inputs["x"]["type"], "REAL")
        self.assertEqual(inputs["y"]["type"], "REAL")
        self.assertEqual(len(outputs), 0)
        self.assertEqual(results["type"], "REAL")

    def test_function_with_scalar_args_4(self):
        self.fs.create_file(
            "/fake/path/scalar_args.f90",
            contents="""\
    module scalar_args_functions
    contains
    !!*
    ! A function with scalar arguments
    !*!
    function add_numbers(x, y) 
        real, intent(in) :: x
        real, intent(in) :: y
        add_numbers = x + y
    end function add_numbers
    end module scalar_args_functions
                            """,
        )
        result = extract_module_data([Path("/fake/path/scalar_args.f90")])

        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(module["module_name"], "scalar_args_functions")
        self.assertEqual(len(module["functions"]), 1)
        self.assertIn("add_numbers", module["functions"])
        function = module["functions"]["add_numbers"]
        self.assertEqual(function["description"], "A function with scalar arguments\n")
        inputs = function["in"]
        outputs = function["out"]
        results = function["return"]
        self.assertEqual(len(inputs), 2)
        self.assertEqual(inputs["x"]["type"], "REAL")
        self.assertEqual(inputs["y"]["type"], "REAL")
        self.assertEqual(len(outputs), 0)
        # note that this is none because implicit return typing is used
        self.assertIsNone(results["type"])

    def test_function_with_array_args(self):
        self.fs.create_file(
            "/fake/path/array_args.f90",
            contents="""\
    module array_args_functions
    contains
    !!*
    ! A function with array arguments
    !*!
    function sum_array(arr) result(res)
        real, intent(in) :: arr(:)
        real :: res
        res = sum(arr)
    end function sum_array
    end module array_args_functions
                            """,
        )
        result = extract_module_data([Path("/fake/path/array_args.f90")])

        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(module["module_name"], "array_args_functions")
        self.assertEqual(len(module["functions"]), 1)
        self.assertIn("sum_array", module["functions"])
        function = module["functions"]["sum_array"]
        inputs = function["in"]
        outputs = function["out"]
        results = function["return"]
        self.assertEqual(len(inputs), 1)
        self.assertEqual(inputs["arr"]["type"], "REAL")
        self.assertEqual(inputs["arr"]["dimension"]["dimensions"], [ArrayBound(bound_type=BoundType.ASSUMED_SHAPE)])
        self.assertEqual(len(outputs), 0)
        self.assertEqual(results["type"], "REAL")

    def test_function_with_mixed_args(self):
        self.fs.create_file(
            "/fake/path/mixed_args.f90",
            contents="""\
    module mixed_args_functions
    contains
    !!*
    ! A function with mixed scalar and array arguments
    !*!
    function multiply_scalar_array(scalar, arr) result(res)
        real, intent(in) :: scalar
        real, intent(in) :: arr(:)
        real, allocatable :: res(:)
        res = scalar * arr
    end function multiply_scalar_array
    end module mixed_args_functions
                            """,
        )
        result = extract_module_data([Path("/fake/path/mixed_args.f90")])

        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(module["module_name"], "mixed_args_functions")
        self.assertEqual(len(module["functions"]), 1)
        self.assertIn("multiply_scalar_array", module["functions"])
        function = module["functions"]["multiply_scalar_array"]
        inputs = function["in"]
        outputs = function["out"]
        results = function["return"]
        self.assertEqual(len(inputs), 2)
        self.assertEqual(inputs["scalar"]["type"], "REAL")
        self.assertEqual(inputs["arr"]["type"], "REAL")
        self.assertEqual(inputs["arr"]["dimension"]["dimensions"], [ArrayBound(bound_type=BoundType.ASSUMED_SHAPE)])
        self.assertEqual(len(outputs), 0)
        self.assertEqual(results["type"], "REAL")
        self.assertEqual(results["dimension"]["dimensions"], [ArrayBound(bound_type=BoundType.ASSUMED_SHAPE)])

    def test_function_with_assumed_size_array(self):
        self.fs.create_file(
            "/fake/path/assumed_size.f90",
            contents="""\
    module assumed_size_module
    contains
    !!*
    ! Function that takes an assumed-size array
    !*!
    function sum_array(arr) result(total)
        real, intent(in) :: arr(*)  ! Assumed-size array
        real :: total
        integer :: i
        
        total = 0.0
        do i = 1, 10
            total = total + arr(i)
        end do
    end function sum_array
    end module assumed_size_module
                            """,
        )
        result = extract_module_data([Path("/fake/path/assumed_size.f90")])

        module = result[0]
        self.assertEqual(module["module_name"], "assumed_size_module")
        self.assertEqual(len(module["functions"]), 1)
        self.assertIn("sum_array", module["functions"])
        function = module["functions"]["sum_array"]
        inputs = function["in"]
        outputs = function["out"]
        results = function["return"]
        
        self.assertEqual(len(inputs), 1)
        self.assertEqual(inputs["arr"]["type"], "REAL")
        self.assertEqual(inputs["arr"]["dimension"]["dimensions"], 
                        [ArrayBound(bound_type=BoundType.ASSUMED_SIZE)])
        
        self.assertEqual(len(outputs), 0)
        self.assertEqual(results["type"], "REAL")
        self.assertEqual(results["dimension"], None)

    def test_function_with_assumed_shape_array(self):
        self.fs.create_file(
            "/fake/path/assumed_shape.f90",
            contents="""\
    module assumed_shape_module
    contains
    !!*
    ! Function that takes assumed-shape arrays
    !*!
    function process_matrix(matrix, vector) result(res)
        real, intent(in) :: matrix(:,:)  ! 2D assumed-shape array
        real, intent(in) :: vector(:)    ! 1D assumed-shape array
        real :: res
        
        res = sum(matrix) + sum(vector)
    end function process_matrix
    end module assumed_shape_module
                            """,
        )
        result = extract_module_data([Path("/fake/path/assumed_shape.f90")])

        module = result[0]
        self.assertEqual(module["module_name"], "assumed_shape_module")
        self.assertEqual(len(module["functions"]), 1)
        self.assertIn("process_matrix", module["functions"])
        function = module["functions"]["process_matrix"]
        inputs = function["in"]
        
        self.assertEqual(inputs["matrix"]["type"], "REAL")
        self.assertEqual(len(inputs["matrix"]["dimension"]["dimensions"]), 2)
        self.assertEqual(inputs["matrix"]["dimension"]["dimensions"], 
                        [ArrayBound(bound_type=BoundType.ASSUMED_SHAPE),
                        ArrayBound(bound_type=BoundType.ASSUMED_SHAPE)])
        
        self.assertEqual(inputs["vector"]["type"], "REAL")
        self.assertEqual(len(inputs["vector"]["dimension"]["dimensions"]), 1)
        self.assertEqual(inputs["vector"]["dimension"]["dimensions"], 
                        [ArrayBound(bound_type=BoundType.ASSUMED_SHAPE)])

    @unittest.skip("fparser doesn't like assumed rank arrays")
    def test_function_with_assumed_rank_array(self):
        self.fs.create_file(
            "/fake/path/assumed_rank.f90",
            contents="""\
    module assumed_rank_module
    contains
    !!*
    ! Function that takes an assumed-rank array (Fortran 2018)
    !*!
    function get_array_size(arr) result(size_val)
        real, intent(in) :: arr(..)  ! Assumed-rank array
        integer :: size_val
        
        size_val = size(arr)
    end function get_array_size
    end module assumed_rank_module
                            """,
        )
        result = extract_module_data([Path("/fake/path/assumed_rank.f90")])

        module = result[0]
        self.assertEqual(module["module_name"], "assumed_rank_module")
        self.assertEqual(len(module["functions"]), 1)
        self.assertIn("get_array_size", module["functions"])
        function = module["functions"]["get_array_size"]
        inputs = function["in"]
        
        self.assertEqual(inputs["arr"]["type"], "REAL")
        self.assertEqual(inputs["arr"]["dimension"]["dimensions"], 
                        [ArrayBound(bound_type=BoundType.ASSUMED_RANK)])
        
        self.assertEqual(function["return"]["type"], "INTEGER")

    def test_function_with_deferred_shape_array(self):
        self.fs.create_file(
            "/fake/path/deferred_shape.f90",
            contents="""\
    module deferred_shape_module
    contains
    !!*
    ! Function that returns an allocatable array
    !*!
    function create_sequence(n) result(seq)
        integer, intent(in) :: n
        real, allocatable :: seq(:)
        integer :: i
        
        allocate(seq(n))
        do i = 1, n
            seq(i) = real(i)
        end do
    end function create_sequence
    end module deferred_shape_module
                            """,
        )
        result = extract_module_data([Path("/fake/path/deferred_shape.f90")])

        module = result[0]
        self.assertEqual(module["module_name"], "deferred_shape_module")
        self.assertEqual(len(module["functions"]), 1)
        self.assertIn("create_sequence", module["functions"])
        function = module["functions"]["create_sequence"]
        inputs = function["in"]
        results = function["return"]
        
        self.assertEqual(inputs["n"]["type"], "INTEGER")
        self.assertEqual(inputs["n"]["dimension"], None)
        
        self.assertEqual(results["type"], "REAL")
        self.assertEqual(results["dimension"]["dimensions"], 
                        [ArrayBound(bound_type=BoundType.ASSUMED_SHAPE)])

    def test_function_with_explicit_shape_array(self):
        self.fs.create_file(
            "/fake/path/explicit_shape.f90",
            contents="""\
    module explicit_shape_module
    contains
    !!*
    ! Function that takes explicit-shape arrays
    !*!
    function process_fixed_array(arr) result(res)
        real, intent(in) :: arr(10)  ! Explicit-shape array
        real :: res
        
        res = sum(arr)
    end function process_fixed_array
    end module explicit_shape_module
                            """,
        )
        result = extract_module_data([Path("/fake/path/explicit_shape.f90")])

        module = result[0]
        self.assertEqual(module["module_name"], "explicit_shape_module")
        self.assertEqual(len(module["functions"]), 1)
        self.assertIn("process_fixed_array", module["functions"])
        function = module["functions"]["process_fixed_array"]
        inputs = function["in"]
        
        self.assertEqual(inputs["arr"]["type"], "REAL")
        self.assertEqual(len(inputs["arr"]["dimension"]["dimensions"]), 1)
        
        dimension = inputs["arr"]["dimension"]["dimensions"][0]
        self.assertEqual(dimension.bound_type, BoundType.FIXED)
        self.assertEqual(dimension.lower.value, "1")  # Fortran defaults to 1-based indexing
        self.assertEqual(dimension.upper.value, "10")

    def test_function_with_array_constructor_bounds(self):
        self.fs.create_file(
            "/fake/path/array_constructor.f90",
            contents="""\
    module array_constructor_module
    contains
    !!*
    ! Function with custom array bounds
    !*!
    function process_custom_bounds(arr) result(res)
        real, intent(in) :: arr(-5:5, 0:9)  ! Custom bounds
        real :: res
        
        res = sum(arr)
    end function process_custom_bounds
    end module array_constructor_module
                            """,
        )
        result = extract_module_data([Path("/fake/path/array_constructor.f90")])

        module = result[0]
        self.assertEqual(module["module_name"], "array_constructor_module")
        function = module["functions"]["process_custom_bounds"]
        inputs = function["in"]
        
        dim1 = inputs["arr"]["dimension"]["dimensions"][0]
        self.assertEqual(dim1.bound_type, BoundType.FIXED)
        self.assertEqual(dim1.lower.value, "-5")
        self.assertEqual(dim1.upper.value, "5")
        
        dim2 = inputs["arr"]["dimension"]["dimensions"][1]
        self.assertEqual(dim2.bound_type, BoundType.FIXED)
        self.assertEqual(dim2.lower.value, "0")
        self.assertEqual(dim2.upper.value, "9")
            
    def test_function_with_variable_bounds(self):
        self.fs.create_file(
            "/fake/path/variable_bounds.f90",
            contents="""\
    module variable_bounds_module
    contains
    !!*
    ! Function with variable bounds
    !*!
    function process_variable_array(arr, n, m) result(res)
        integer, intent(in) :: n, m
        real, intent(in) :: arr(n, m)  ! Variable bounds
        real :: res
        
        res = sum(arr)
    end function process_variable_array
    end module variable_bounds_module
                            """,
        )
        result = extract_module_data([Path("/fake/path/variable_bounds.f90")])
        
        module = result[0]
        function = module["functions"]["process_variable_array"]
        inputs = function["in"]
        
        dim1 = inputs["arr"]["dimension"]["dimensions"][0]
        self.assertEqual(dim1.bound_type, BoundType.VARIABLE)
        self.assertEqual(dim1.upper.value, "n")
        
        dim2 = inputs["arr"]["dimension"]["dimensions"][1]
        self.assertEqual(dim2.bound_type, BoundType.VARIABLE)
        self.assertEqual(dim2.upper.value, "m")            

    def test_function_with_variable_bounds(self):
        self.fs.create_file(
            "/fake/path/variable_bounds.f90",
            contents="""\
    module variable_bounds_module
    contains
    !!*
    ! Function with variable bounds
    !*!
    function process_variable_array(arr, n, m) result(res)
        integer, intent(in) :: n, m
        real, intent(in) :: arr(n, m)  ! Variable bounds
        real :: res
        
        res = sum(arr)
    end function process_variable_array
    end module variable_bounds_module
                            """,
        )
        result = extract_module_data([Path("/fake/path/variable_bounds.f90")])
        
        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(module["module_name"], "variable_bounds_module")
        self.assertEqual(len(module["functions"]), 1)
        self.assertIn("process_variable_array", module["functions"])
        function = module["functions"]["process_variable_array"]
        inputs = function["in"]
        
        self.assertEqual(inputs["arr"]["type"], "REAL")
        self.assertEqual(len(inputs["arr"]["dimension"]["dimensions"]), 2)
        
        dim1 = inputs["arr"]["dimension"]["dimensions"][0]
        self.assertEqual(dim1.bound_type, BoundType.VARIABLE)
        self.assertEqual(dim1.lower.value, "1")  # Default lower bound
        self.assertEqual(dim1.upper.value, "n")
        
        dim2 = inputs["arr"]["dimension"]["dimensions"][1]
        self.assertEqual(dim2.bound_type, BoundType.VARIABLE)
        self.assertEqual(dim2.lower.value, "1")  # Default lower bound
        self.assertEqual(dim2.upper.value, "m")
        
        self.assertEqual(inputs["n"]["type"], "INTEGER")
        self.assertEqual(inputs["n"]["dimension"], None)
        self.assertEqual(inputs["m"]["type"], "INTEGER")
        self.assertEqual(inputs["m"]["dimension"], None)

    def test_function_with_expression_bounds(self):
        self.fs.create_file(
            "/fake/path/expression_bounds.f90",
            contents="""\
    module expression_bounds_module
    contains
    !!*
    ! Function with expression bounds
    !*!
    function process_expr_array(arr, n) result(res)
        integer, intent(in) :: n
        real, intent(in) :: arr(2*n+1, n**2)  ! Expression bounds
        real :: res
        
        res = sum(arr)
    end function process_expr_array
    end module expression_bounds_module
                            """,
        )
        result = extract_module_data([Path("/fake/path/expression_bounds.f90")])
        
        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(module["module_name"], "expression_bounds_module")
        self.assertEqual(len(module["functions"]), 1)
        self.assertIn("process_expr_array", module["functions"])
        function = module["functions"]["process_expr_array"]
        inputs = function["in"]
        
        self.assertEqual(inputs["arr"]["type"], "REAL")
        self.assertEqual(len(inputs["arr"]["dimension"]["dimensions"]), 2)
        
        dim1 = inputs["arr"]["dimension"]["dimensions"][0]
        self.assertEqual(dim1.bound_type, BoundType.VARIABLE)  # Expression treated as variable
        self.assertEqual(dim1.lower.value, "1")
        self.assertEqual(dim1.upper.value, "2 * n + 1")
        
        dim2 = inputs["arr"]["dimension"]["dimensions"][1]
        self.assertEqual(dim2.bound_type, BoundType.VARIABLE)  # Expression treated as variable
        self.assertEqual(dim2.lower.value, "1")
        self.assertEqual(dim2.upper.value, "n ** 2")
        
        self.assertEqual(inputs["n"]["type"], "INTEGER")
        self.assertEqual(inputs["n"]["dimension"], None)

    def test_function_with_mixed_array_types(self):
        self.fs.create_file(
            "/fake/path/mixed_arrays.f90",
            contents="""\
    module mixed_arrays_module
    contains
    !!*
    ! Function with multiple array types
    !*!
    function process_mixed_arrays(fixed_arr, assumed_arr, variable_arr, n) result(res)
        integer, intent(in) :: n
        real, intent(in) :: fixed_arr(10)      ! Fixed
        real, intent(in) :: assumed_arr(:)     ! Assumed shape
        real, intent(in) :: variable_arr(n)    ! Variable bounds
        real, allocatable :: res(:)            ! Allocatable return
        
        allocate(res(size(assumed_arr)))
        res = fixed_arr(1:size(assumed_arr)) + assumed_arr + variable_arr(1:size(assumed_arr))
    end function process_mixed_arrays
    end module mixed_arrays_module
                            """,
        )
        result = extract_module_data([Path("/fake/path/mixed_arrays.f90")])
        
        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(module["module_name"], "mixed_arrays_module")
        self.assertEqual(len(module["functions"]), 1)
        self.assertIn("process_mixed_arrays", module["functions"])
        function = module["functions"]["process_mixed_arrays"]
        inputs = function["in"]
        results = function["return"]
        
        self.assertEqual(inputs["fixed_arr"]["type"], "REAL")
        self.assertEqual(len(inputs["fixed_arr"]["dimension"]["dimensions"]), 1)
        fixed_dim = inputs["fixed_arr"]["dimension"]["dimensions"][0]
        self.assertEqual(fixed_dim.bound_type, BoundType.FIXED)
        self.assertEqual(fixed_dim.lower.value, "1")
        self.assertEqual(fixed_dim.upper.value, "10")
        
        self.assertEqual(inputs["assumed_arr"]["type"], "REAL")
        self.assertEqual(len(inputs["assumed_arr"]["dimension"]["dimensions"]), 1)
        assumed_dim = inputs["assumed_arr"]["dimension"]["dimensions"][0]
        self.assertEqual(assumed_dim.bound_type, BoundType.ASSUMED_SHAPE)
        
        self.assertEqual(inputs["variable_arr"]["type"], "REAL")
        self.assertEqual(len(inputs["variable_arr"]["dimension"]["dimensions"]), 1)
        variable_dim = inputs["variable_arr"]["dimension"]["dimensions"][0]
        self.assertEqual(variable_dim.bound_type, BoundType.VARIABLE)
        self.assertEqual(variable_dim.lower.value, "1")
        self.assertEqual(variable_dim.upper.value, "n")
        
        self.assertEqual(inputs["n"]["type"], "INTEGER")
        self.assertEqual(inputs["n"]["dimension"], None)
        
        self.assertEqual(results["type"], "REAL")
        self.assertEqual(len(results["dimension"]["dimensions"]), 1)
        result_dim = results["dimension"]["dimensions"][0]
        self.assertEqual(result_dim.bound_type, BoundType.ASSUMED_SHAPE)

    def test_function_with_character_arrays(self):
        self.fs.create_file(
            "/fake/path/character_arrays.f90",
            contents="""\
    module character_module
    contains
    !!*
    ! Function with character arguments
    !*!
    function process_strings(str_array, fixed_str_array) result(total_len)
        character(len=*), intent(in) :: str_array(:)      ! Assumed length strings
        character(len=20), intent(in) :: fixed_str_array(5)  ! Fixed length strings
        integer :: total_len
        integer :: i
        
        total_len = 0
        do i = 1, size(str_array)
            total_len = total_len + len_trim(str_array(i))
        end do
        do i = 1, 5
            total_len = total_len + len_trim(fixed_str_array(i))
        end do
    end function process_strings
    end module character_module
                            """,
        )
        result = extract_module_data([Path("/fake/path/character_arrays.f90")])
        
        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(module["module_name"], "character_module")
        self.assertEqual(len(module["functions"]), 1)
        self.assertIn("process_strings", module["functions"])
        function = module["functions"]["process_strings"]
        inputs = function["in"]
        results = function["return"]
        
        self.assertEqual(inputs["str_array"]["type"], "CHARACTER")
        self.assertEqual(len(inputs["str_array"]["dimension"]["dimensions"]), 1)
        str_dim = inputs["str_array"]["dimension"]["dimensions"][0]
        self.assertEqual(str_dim.bound_type, BoundType.ASSUMED_SHAPE)
        
        self.assertEqual(inputs["fixed_str_array"]["type"], "CHARACTER")
        self.assertEqual(len(inputs["fixed_str_array"]["dimension"]["dimensions"]), 1)
        fixed_str_dim = inputs["fixed_str_array"]["dimension"]["dimensions"][0]
        self.assertEqual(fixed_str_dim.bound_type, BoundType.FIXED)
        self.assertEqual(fixed_str_dim.lower.value, "1")
        self.assertEqual(fixed_str_dim.upper.value, "5")
        
        self.assertEqual(results["type"], "INTEGER")
        self.assertEqual(results["dimension"], None)

    def test_function_with_complex_variable_bounds(self):
        self.fs.create_file(
            "/fake/path/complex_variable_bounds.f90",
            contents="""\
    module complex_variable_bounds_module
    contains
    !!*
    ! Function with complex variable bounds
    !*!
    function process_complex_bounds(arr, start_idx, end_idx, n) result(res)
        integer, intent(in) :: start_idx, end_idx, n
        real, intent(in) :: arr(start_idx:end_idx, -n:n)  ! Custom variable bounds
        real :: res
        
        res = sum(arr)
    end function process_complex_bounds
    end module complex_variable_bounds_module
                            """,
        )
        result = extract_module_data([Path("/fake/path/complex_variable_bounds.f90")])
        
        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(module["module_name"], "complex_variable_bounds_module")
        self.assertEqual(len(module["functions"]), 1)
        self.assertIn("process_complex_bounds", module["functions"])
        function = module["functions"]["process_complex_bounds"]
        inputs = function["in"]
        
        self.assertEqual(inputs["arr"]["type"], "REAL")
        self.assertEqual(len(inputs["arr"]["dimension"]["dimensions"]), 2)
        
        dim1 = inputs["arr"]["dimension"]["dimensions"][0]
        self.assertEqual(dim1.bound_type, BoundType.VARIABLE)
        self.assertEqual(dim1.lower.value, "start_idx")
        self.assertEqual(dim1.upper.value, "end_idx")
        
        dim2 = inputs["arr"]["dimension"]["dimensions"][1]
        self.assertEqual(dim2.bound_type, BoundType.VARIABLE)
        self.assertEqual(dim2.lower.value, "-n")
        self.assertEqual(dim2.upper.value, "n")

    def test_function_with_intent_out_array(self):
        self.fs.create_file(
            "/fake/path/intent_out.f90",
            contents="""\
    module intent_out_module
    contains
    !!*
    ! Function with intent(out) array
    !*!
    function create_array(output_arr, n) result(success)
        integer, intent(in) :: n
        real, intent(out) :: output_arr(:)
        logical :: success
        integer :: i
        
        if (size(output_arr) >= n) then
            do i = 1, n
                output_arr(i) = real(i)
            end do
            success = .true.
        else
            success = .false.
        end if
    end function create_array
    end module intent_out_module
                            """,
        )
        result = extract_module_data([Path("/fake/path/intent_out.f90")])
        
        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(module["module_name"], "intent_out_module")
        self.assertEqual(len(module["functions"]), 1)
        self.assertIn("create_array", module["functions"])
        function = module["functions"]["create_array"]
        inputs = function["in"]
        outputs = function["out"]
        results = function["return"]
        
        self.assertEqual(len(inputs), 1)
        self.assertEqual(inputs["n"]["type"], "INTEGER")
        self.assertEqual(inputs["n"]["dimension"], None)
        
        self.assertEqual(len(outputs), 1)
        self.assertEqual(outputs["output_arr"]["type"], "REAL")
        self.assertEqual(len(outputs["output_arr"]["dimension"]["dimensions"]), 1)
        out_dim = outputs["output_arr"]["dimension"]["dimensions"][0]
        self.assertEqual(out_dim.bound_type, BoundType.ASSUMED_SHAPE)
        
        self.assertEqual(results["type"], "LOGICAL")
        self.assertEqual(results["dimension"], None)

    def test_function_with_intent_inout_array(self):
        self.fs.create_file(
            "/fake/path/intent_inout.f90",
            contents="""\
    module intent_inout_module
    contains
    !!*
    ! Function with intent(inout) array
    !*!
    function modify_array(arr, multiplier) result(success)
        real, intent(inout) :: arr(:)
        real, intent(in) :: multiplier
        logical :: success
        integer :: i
        
        do i = 1, size(arr)
            arr(i) = arr(i) * multiplier
        end do
        success = .true.
    end function modify_array
    end module intent_inout_module
                            """,
        )
        result = extract_module_data([Path("/fake/path/intent_inout.f90")])
        
        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(module["module_name"], "intent_inout_module")
        self.assertEqual(len(module["functions"]), 1)
        self.assertIn("modify_array", module["functions"])
        function = module["functions"]["modify_array"]
        inputs = function["in"]
        outputs = function["out"]
        results = function["return"]
        
        self.assertEqual(inputs["multiplier"]["type"], "REAL")
        self.assertEqual(inputs["multiplier"]["dimension"], None)
        
        self.assertIn("arr", inputs)
        self.assertEqual(inputs["arr"]["type"], "REAL")
        self.assertEqual(len(inputs["arr"]["dimension"]["dimensions"]), 1)
        input_dim = inputs["arr"]["dimension"]["dimensions"][0]
        self.assertEqual(input_dim.bound_type, BoundType.ASSUMED_SHAPE)
        
        self.assertIn("arr", outputs)
        self.assertEqual(outputs["arr"]["type"], "REAL")
        self.assertEqual(len(outputs["arr"]["dimension"]["dimensions"]), 1)
        output_dim = outputs["arr"]["dimension"]["dimensions"][0]
        self.assertEqual(output_dim.bound_type, BoundType.ASSUMED_SHAPE)
        
        self.assertEqual(results["type"], "LOGICAL")
        self.assertEqual(results["dimension"], None)

    def test_function_with_mixed_intents(self):
        self.fs.create_file(
            "/fake/path/mixed_intents.f90",
            contents="""\
    module mixed_intents_module
    contains
    !!*
    ! Function with mixed intent arguments
    !*!
    function process_mixed_intents(input_arr, output_arr, inout_arr) result(success)
        real, intent(in) :: input_arr(:)
        real, intent(out) :: output_arr(:)
        real, intent(inout) :: inout_arr(:)
        logical :: success
        integer :: i, n
        
        n = min(size(input_arr), size(output_arr), size(inout_arr))
        do i = 1, n
            output_arr(i) = input_arr(i) + inout_arr(i)
            inout_arr(i) = inout_arr(i) * 2.0
        end do
        success = .true.
    end function process_mixed_intents
    end module mixed_intents_module
                            """,
        )
        result = extract_module_data([Path("/fake/path/mixed_intents.f90")])
        
        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(module["module_name"], "mixed_intents_module")
        self.assertEqual(len(module["functions"]), 1)
        self.assertIn("process_mixed_intents", module["functions"])
        function = module["functions"]["process_mixed_intents"]
        inputs = function["in"]
        outputs = function["out"]
        
        self.assertIn("input_arr", inputs)
        self.assertEqual(inputs["input_arr"]["type"], "REAL")
        self.assertNotIn("input_arr", outputs)
        
        self.assertIn("output_arr", outputs)
        self.assertEqual(outputs["output_arr"]["type"], "REAL")
        self.assertNotIn("output_arr", inputs)
        
        self.assertIn("inout_arr", inputs)
        self.assertIn("inout_arr", outputs)
        self.assertEqual(inputs["inout_arr"]["type"], "REAL")
        self.assertEqual(outputs["inout_arr"]["type"], "REAL")
        
        for arr_name in ["input_arr"]:
            dim = inputs[arr_name]["dimension"]["dimensions"][0]
            self.assertEqual(dim.bound_type, BoundType.ASSUMED_SHAPE)
        
        for arr_name in ["output_arr", "inout_arr"]:
            dim = outputs[arr_name]["dimension"]["dimensions"][0]
            self.assertEqual(dim.bound_type, BoundType.ASSUMED_SHAPE)        

    def test_function_with_parameterized_derived_types(self):
        self.fs.create_file(
            "/fake/path/parameterized_types.f90",
            contents="""\
    module parameterized_types_module
        use, intrinsic :: iso_fortran_env, only: real32, real64
        
        ! Define a parameterized derived type
        type :: matrix_type(k, rows, cols)
            integer, kind :: k = real32      ! Kind parameter with default
            integer, len :: rows, cols       ! Length parameters
            real(k) :: data(rows, cols)      ! Array component with parameters
            real(k) :: scale_factor
        end type matrix_type
        
        ! Another parameterized type
        type :: vector_type(precision, length)
            integer, kind :: precision
            integer, len :: length
            real(precision) :: components(length)
        end type vector_type
        
    contains
    !!*
    ! Function that processes parameterized derived types
    !*!
    function process_matrices(fixed_matrix, variable_matrix, vectors) result(total_sum)
        ! Fixed parameters - specific instantiation
        type(matrix_type(k=real64, rows=3, cols=3)), intent(in) :: fixed_matrix
        ! Variable parameters - deferred shape
        type(matrix_type(k=real32, :, :)), intent(in) :: variable_matrix
        ! Array of parameterized types
        type(vector_type(precision=real64, :)), intent(in) :: vectors(:)
        real :: total_sum
        integer :: i
        
        total_sum = sum(fixed_matrix%data) + sum(variable_matrix%data)
        do i = 1, size(vectors)
            total_sum = total_sum + sum(vectors(i)%components)
        end do
    end function process_matrices

    !!*
    ! Function with allocatable parameterized type
    !*!
    function create_dynamic_matrix(rows, cols) result(mat)
        integer, intent(in) :: rows, cols
        type(matrix_type(k=real64, :, :)), allocatable :: mat
        integer :: i, j
        
        allocate(matrix_type(k=real64, rows=rows, cols=cols) :: mat)
        do i = 1, rows
            do j = 1, cols
                mat%data(i, j) = real(i * cols + j, real64)
            end do
        end do
        mat%scale_factor = 1.0_real64
    end function create_dynamic_matrix

    end module parameterized_types_module
                                """,
        )
        result = extract_module_data([Path("/fake/path/parameterized_types.f90")])
        
        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(module["module_name"], "parameterized_types_module")
        self.assertEqual(len(module["functions"]), 2)
        
        self.assertIn("process_matrices", module["functions"])
        function1 = module["functions"]["process_matrices"]
        inputs1 = function1["in"]
        results1 = function1["return"]
        
        self.assertIn("fixed_matrix", inputs1)
        self.assertEqual(inputs1["fixed_matrix"]["type"], "matrix_type(k=real64, rows=3, cols=3)")
        
        self.assertIn("variable_matrix", inputs1)
        self.assertEqual(inputs1["variable_matrix"]["type"], "matrix_type(k=real32, :, :)")
        
        self.assertIn("vectors", inputs1)
        self.assertEqual(inputs1["vectors"]["type"], "vector_type(precision=real64, :)")
        if inputs1["vectors"]["dimension"]:
            self.assertEqual(len(inputs1["vectors"]["dimension"]["dimensions"]), 1)
            dim = inputs1["vectors"]["dimension"]["dimensions"][0]
            self.assertEqual(dim.bound_type, BoundType.ASSUMED_SHAPE)
        
        self.assertEqual(results1["type"], "REAL")
        self.assertEqual(results1["dimension"], None)
        
        self.assertIn("create_dynamic_matrix", module["functions"])
        function2 = module["functions"]["create_dynamic_matrix"]
        inputs2 = function2["in"]
        results2 = function2["return"]
        
        self.assertEqual(inputs2["rows"]["type"], "INTEGER")
        self.assertEqual(inputs2["rows"]["dimension"], None)
        self.assertEqual(inputs2["cols"]["type"], "INTEGER")
        self.assertEqual(inputs2["cols"]["dimension"], None)
        
        self.assertEqual(results2["type"], "matrix_type(k=real64, :, :)")

    def test_function_with_simple_parameterized_types(self):
        """Test simpler case of parameterized types that might be more commonly supported"""
        self.fs.create_file(
            "/fake/path/simple_parameterized.f90",
            contents="""\
    module simple_parameterized_module
        
        ! Simple parameterized type with just length parameter
        type :: string_type(max_len)
            integer, len :: max_len
            character(len=max_len) :: value
            integer :: actual_len
        end type string_type
        
    contains
    !!*
    ! Function with simple parameterized type
    !*!
    function process_strings(str_array, max_length) result(total_length)
        integer, intent(in) :: max_length
        type(string_type(max_len=max_length)), intent(in) :: str_array(:)
        integer :: total_length
        integer :: i
        
        total_length = 0
        do i = 1, size(str_array)
            total_length = total_length + str_array(i)%actual_len
        end do
    end function process_strings

    end module simple_parameterized_module
                                """,
        )
        result = extract_module_data([Path("/fake/path/simple_parameterized.f90")])
        
        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(module["module_name"], "simple_parameterized_module")
        self.assertEqual(len(module["functions"]), 1)
        
        self.assertIn("process_strings", module["functions"])
        function = module["functions"]["process_strings"]
        inputs = function["in"]
        results = function["return"]
        
        self.assertEqual(inputs["max_length"]["type"], "INTEGER")
        self.assertEqual(inputs["max_length"]["dimension"], None)
        
        self.assertIn("str_array", inputs)
        self.assertEqual(inputs["str_array"]["type"], "string_type(max_len=max_length)")
        if inputs["str_array"]["dimension"]:
            self.assertEqual(len(inputs["str_array"]["dimension"]["dimensions"]), 1)
            dim = inputs["str_array"]["dimension"]["dimensions"][0]
            self.assertEqual(dim.bound_type, BoundType.ASSUMED_SHAPE)
        
        self.assertEqual(results["type"], "INTEGER")
        self.assertEqual(results["dimension"], None)            

if __name__ == "__main__":
    unittest.main()
