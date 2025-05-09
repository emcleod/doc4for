import unittest
from pathlib import Path
from pyfakefs.fake_filesystem_unittest import TestCase
from doc4for.f90.generate_module_tree import extract_module_data

class TestImportStatements(TestCase):
    maxDiff=None

    def setUp(self):
        self.setUpPyfakefs()

        """Test import statement in interfaces."""
        self.fs.create_file(
            "/fake/path/import_interfaces.f90",
            contents="""\
    !!*
    ! Module for optimization algorithms
    ! Provides interfaces for objective functions, constraints, and optimizers
    !*!
    module optimization_mod
        implicit none
        private
        
        !!* 
        ! Interface for objective functions
        ! Functions that take a vector and return a scalar value to minimize
        !*!
        public :: objective_func
        abstract interface
            !!*
            ! Objective function interface
            ! @in x Input vector
            ! @return Function value
            !*!
            function objective_func(x) result(f)
                real, dimension(:), intent(in) :: x
                real :: f
            end function objective_func
        end interface
        
        !!* 
        ! Interface for constraint functions
        ! Functions that return true if constraints are satisfied
        !*!
        public :: constraint_func
        abstract interface
            !!*
            ! Constraint function interface
            ! @in x Input vector
            ! @return True if constraints are satisfied
            !*!
            function constraint_func(x) result(c)
                real, dimension(:), intent(in) :: x
                logical :: c
            end function constraint_func
        end interface
        
        !!* 
        ! Interface for optimization algorithms
        ! Standard interface for all optimization methods
        !*!
        public :: optimizer_interface
        interface optimizer_interface
            !!* 
            ! Minimize an objective function subject to constraints
            ! @in obj Function to minimize
            ! @in constraint Constraint function
            ! @in x0 Initial guess
            ! @return Optimal solution
            !*!
            function minimize(obj, constraint, x0) result(x_opt)
                import :: objective_func, constraint_func
                procedure(objective_func) :: obj
                procedure(constraint_func) :: constraint
                real, dimension(:), intent(in) :: x0
                real, dimension(size(x0)) :: x_opt
            end function
        end interface
        
    contains
        !!* 
        ! Implementation of conjugate gradient optimizer
        ! Uses Fletcher-Reeves formula for beta calculation
        ! @in obj Function to minimize
        ! @in constraint Constraint function
        ! @in x0 Initial guess
        ! @return Optimal solution
        !*!
        function conjugate_gradient(obj, constraint, x0) result(x_opt)
            procedure(objective_func) :: obj
            procedure(constraint_func) :: constraint
            real, dimension(:), intent(in) :: x0
            real, dimension(size(x0)) :: x_opt
            
            ! Simplified implementation
            x_opt = x0  ! Just return initial guess
        end function
    end module optimization_mod
    """
        )
        
        result = extract_module_data([Path("/fake/path/import_interfaces.f90")])
        self.assertEqual(len(result), 1)
        module = result[0]
        
        # Check module description
        self.assertEqual(module["module_description"], 
                        "\nModule for optimization algorithms\n"
                        "Provides interfaces for objective functions, constraints, and optimizers\n\n")
        
        # Check that all interfaces are parsed
        self.assertEqual(len(module["interfaces"]), 3)
        
        # Check interface descriptions
        objective_interface = None
        constraint_interface = None
        optimizer_interface = None
        
        for interface in module["interfaces"]:
            if "abstract" in interface["attributes"] and interface["procedures"].get("objective_func"):
                objective_interface = interface
            elif "abstract" in interface["attributes"] and interface["procedures"].get("constraint_func"):
                constraint_interface = interface
            elif "name" in interface and interface["name"] == "optimizer_interface":
                optimizer_interface = interface
        
        self.assertIsNotNone(objective_interface)
        self.assertIsNotNone(constraint_interface)
        self.assertIsNotNone(optimizer_interface)
        
        # Check descriptions
        self.assertEqual(objective_interface["description"], 
                        "\nInterface for objective functions\n"
                        "Functions that take a vector and return a scalar value to minimize\n\n")
        self.assertEqual(constraint_interface["description"], 
                        "\nInterface for constraint functions\n"
                        "Functions that return true if constraints are satisfied\n\n")
        self.assertEqual(optimizer_interface["description"], 
                        "\nInterface for optimization algorithms\n"
                        "Standard interface for all optimization methods\n\n")
        
        # Check the minimize function within the interface
        minimize = optimizer_interface["procedures"]["minimize"]
        
        # Check function description
        self.assertEqual(minimize["description"], 
                        "\nMinimize an objective function subject to constraints\n\n")
        
        # Check parameter descriptions
        self.assertEqual(minimize["in"]["obj"]["description"], "Function to minimize")
        self.assertEqual(minimize["in"]["constraint"]["description"], "Constraint function")
        self.assertEqual(minimize["in"]["x0"]["description"], "Initial guess")
        self.assertEqual(minimize["return"]["x_opt"]["description"], "Optimal solution")
        
        # Check that import statements are tracked
        self.assertEqual(len(minimize["imports"]), 2)
        self.assertIn("objective_func", minimize["imports"])
        self.assertIn("constraint_func", minimize["imports"])
        
        # Check that imported interfaces are correctly referenced
        self.assertEqual(minimize["in"]["obj"]["interface_name"], "objective_func")
        self.assertEqual(minimize["in"]["constraint"]["interface_name"], "constraint_func")
        
        # Check that the `imported_interfaces` field is populated
        self.assertEqual(len(minimize["imported_interfaces"]), 2)
        self.assertIn("objective_func", minimize["imported_interfaces"])
        self.assertIn("constraint_func", minimize["imported_interfaces"])
        
        # Verify that the module function also correctly references the interfaces
        self.assertIn("conjugate_gradient", module["functions"])
        cg_func = module["functions"]["conjugate_gradient"]
        self.assertEqual(cg_func["description"], 
                        "\nImplementation of conjugate gradient optimizer\n"
                        "Uses Fletcher-Reeves formula for beta calculation\n\n")
        self.assertEqual(cg_func["in"]["obj"]["interface_name"], "objective_func")
        self.assertEqual(cg_func["in"]["constraint"]["interface_name"], "constraint_func")

    def test_import_with_partial_list(self):
        """Test import statement with a partial list of entities."""
        self.fs.create_file(
            "/fake/path/partial_import.f90",
            contents="""\
    !!*
    ! Linear algebra module
    ! Provides types and interfaces for linear algebra operations
    !*!
    module linear_algebra_mod
        implicit none
        private
        
        !!* 
        ! Vector type
        ! Represents a mathematical vector with basic operations
        !*!
        type, public :: vector_t
            real, allocatable :: elements(:)
        end type
        
        !!* 
        ! Matrix type
        ! Represents a mathematical matrix with basic operations
        !*!
        type, public :: matrix_t
            real, allocatable :: elements(:,:)
        end type
        
        !!* 
        ! Complex type
        ! Represents a complex number
        !*!
        type, public :: complex_t
            real :: real_part
            real :: imag_part
        end type
        
        !!* 
        ! Interface for linear solvers
        ! Defines the standard interface for linear system solvers
        !*!
        public :: linear_solver_interface
        interface linear_solver_interface
            !!*
            ! Solve a linear system Ax = b
            ! @in A Coefficient matrix
            ! @in b Right-hand side vector
            ! @return Solution vector x
            !*!
            function solve_linear_system(A, b) result(x)
                import :: matrix_t, vector_t   ! Note: not importing complex_t
                type(matrix_t), intent(in) :: A
                type(vector_t), intent(in) :: b
                type(vector_t) :: x
            end function
        end interface
        
    end module linear_algebra_mod
    """
        )
        
        result = extract_module_data([Path("/fake/path/partial_import.f90")])
        self.assertEqual(len(result), 1)
        module = result[0]
        
        # Check module description
        self.assertEqual(module["module_description"], 
                        "\nLinear algebra module\n"
                        "Provides types and interfaces for linear algebra operations\n\n")
        
        # Check type descriptions
        vector_type = module["typedef"]["vector_t"]
        self.assertEqual(vector_type["description"], 
                        "\nVector type\n"
                        "Represents a mathematical vector with basic operations\n\n")
        
        matrix_type = module["typedef"]["matrix_t"]
        self.assertEqual(matrix_type["description"], 
                        "\nMatrix type\n"
                        "Represents a mathematical matrix with basic operations\n\n")
        
        complex_type = module["typedef"]["complex_t"]
        self.assertEqual(complex_type["description"], 
                        "\nComplex type\n"
                        "Represents a complex number\n\n")
        
        # Check interface description
        solver_interface = None
        for interface in module["interfaces"]:
            if "name" in interface and interface["name"] == "linear_solver_interface":
                solver_interface = interface
                break
        
        self.assertIsNotNone(solver_interface)
        self.assertEqual(solver_interface["description"], 
                        "\nInterface for linear solvers\n"
                        "Defines the standard interface for linear system solvers\n\n")
        
        # Check the solve function within the interface
        solve_func = solver_interface["procedures"]["solve_linear_system"]
        
        # Check function description
        self.assertEqual(solve_func["description"], 
                        "\nSolve a linear system Ax = b\n\n")
        
        # Check parameter descriptions
        self.assertEqual(solve_func["in"]["A"]["description"], "Coefficient matrix")
        self.assertEqual(solve_func["in"]["b"]["description"], "Right-hand side vector")
        self.assertEqual(solve_func["return"]["x"]["description"], "Solution vector x")
        
        # Check that only the specified imports are tracked 
        self.assertEqual(len(solve_func["imports"]), 2)
        self.assertIn("matrix_t", solve_func["imports"])
        self.assertIn("vector_t", solve_func["imports"])
        self.assertNotIn("complex_t", solve_func["imports"])  # Not imported
        
        # Check imported types are tracked
        self.assertEqual(len(solve_func["imported_types"]), 2)
        self.assertIn("matrix_t", solve_func["imported_types"])
        self.assertIn("vector_t", solve_func["imported_types"])
        
        # Verify type information is recorded
        self.assertEqual(solve_func["in"]["A"]["type"], "matrix_t")
        self.assertEqual(solve_func["in"]["A"]["module_origin"], "linear_algebra_mod")
        self.assertEqual(solve_func["in"]["b"]["type"], "vector_t")
        self.assertEqual(solve_func["in"]["b"]["module_origin"], "linear_algebra_mod")

    def test_import_with_renamed_entities(self):
        """Test import statement with renamed entities."""
        self.fs.create_file(
            "/fake/path/renamed_import.f90",
            contents="""\
    !!*
    ! Generic mathematical types module
    ! Provides basic types for mathematical operations
    !*!
    module generic_math_types
        implicit none
        private
        
        !!* 
        ! Generic real precision selector
        ! Determines the precision for all calculations
        !*!
        integer, parameter, public :: r_kind = selected_real_kind(15, 307)
        
        !!*
        ! Generic array type
        ! Can be used for vectors or other 1D arrays
        !*!
        type, public :: array_type
            real(kind=r_kind), allocatable :: data(:)
        end type
        
        !!*
        ! 2D array type
        ! Can be used for matrices or other 2D data
        !*!
        type, public :: array_2d_type
            real(kind=r_kind), allocatable :: data(:,:)
        end type
        
    end module generic_math_types
    
    !!*
    ! Specialized math module
    ! Uses generic types with domain-specific names
    !*!
    module specialized_math
        implicit none
        
        !!*
        ! Interface for domain-specific vector operations
        ! Renames generic types for clarity in this context
        !*!
        interface
            !!*
            ! Multiplies a matrix by a vector
            ! @in M Matrix (2D array)
            ! @in v Vector (1D array)
            ! @return Result vector
            !*!
            function matrix_vector_multiply(M, v) result(result)
                import :: arr_2d => array_2d_type, vec => array_type
                type(arr_2d), intent(in) :: M
                type(vec), intent(in) :: v
                type(vec) :: result
            end function
        end interface
        
    contains
        !!*
        ! Example function using the renamed imports
        ! @in M Input matrix
        ! @return Diagonal as a vector
        !*!
        function extract_diagonal(M) result(diag)
            use generic_math_types, only: matrix => array_2d_type, vector => array_type
            type(matrix), intent(in) :: M
            type(vector) :: diag
            integer :: i, n
            
            n = min(size(M%data, 1), size(M%data, 2))
            allocate(diag%data(n))
            
            do i = 1, n
                diag%data(i) = M%data(i, i)
            end do
        end function
    end module specialized_math
    """
        )
        
        result = extract_module_data([Path("/fake/path/renamed_import.f90")])
        self.assertEqual(len(result), 2)
        
        # Find both modules
        generic_module = None
        specialized_module = None
        for module in result:
            if module["name"] == "generic_math_types":
                generic_module = module
            elif module["name"] == "specialized_math":
                specialized_module = module
        
        self.assertIsNotNone(generic_module)
        self.assertIsNotNone(specialized_module)
        
        # Check module descriptions
        self.assertEqual(generic_module["module_description"], 
                        "\nGeneric mathematical types module\n"
                        "Provides basic types for mathematical operations\n\n")
        self.assertEqual(specialized_module["module_description"], 
                        "\nSpecialized math module\n"
                        "Uses generic types with domain-specific names\n\n")
        
        # Check type descriptions in generic module
        array_type = generic_module["typedef"]["array_type"]
        self.assertEqual(array_type["description"], 
                        "\nGeneric array type\n"
                        "Can be used for vectors or other 1D arrays\n\n")
        
        array_2d_type = generic_module["typedef"]["array_2d_type"]
        self.assertEqual(array_2d_type["description"], 
                        "\n2D array type\n"
                        "Can be used for matrices or other 2D data\n\n")
        
        # Check interface in specialized module
        interface = specialized_module["interfaces"][0]
        self.assertEqual(interface["description"], 
                        "\nInterface for domain-specific vector operations\n"
                        "Renames generic types for clarity in this context\n\n")
        
        # Check the function within the interface
        mult_func = interface["procedures"]["matrix_vector_multiply"]
        self.assertEqual(mult_func["description"], 
                        "\nMultiplies a matrix by a vector\n\n")
        
        # Check parameter descriptions
        self.assertEqual(mult_func["in"]["M"]["description"], "Matrix (2D array)")
        self.assertEqual(mult_func["in"]["v"]["description"], "Vector (1D array)")
        self.assertEqual(mult_func["return"]["result"]["description"], "Result vector")
        
        # Check import renames
        self.assertEqual(len(mult_func["imports"]), 2)
        self.assertEqual(len(mult_func["import_renames"]), 2)
        
        rename_dict = {r["local"]: r["original"] for r in mult_func["import_renames"]}
        self.assertEqual(rename_dict["arr_2d"], "array_2d_type")
        self.assertEqual(rename_dict["vec"], "array_type")
        
        # Check that parameters use the renamed types
        self.assertEqual(mult_func["in"]["M"]["type"], "arr_2d")
        self.assertEqual(mult_func["in"]["M"]["original_type"], "array_2d_type")
        self.assertEqual(mult_func["in"]["v"]["type"], "vec")
        self.assertEqual(mult_func["in"]["v"]["original_type"], "array_type")
        
        # Check the module function with USE renames
        diag_func = specialized_module["functions"]["extract_diagonal"]
        self.assertEqual(diag_func["description"], 
                        "\nExample function using the renamed imports\n\n")
        
        # Check USE statement renames
        self.assertEqual(len(diag_func["uses"]), 1)
        self.assertEqual(diag_func["uses"][0]["module"], "generic_math_types")
        
        rename_list = diag_func["uses"][0]["renames"]
        rename_dict = {r["local"]: r["original"] for r in rename_list}
        self.assertEqual(rename_dict["matrix"], "array_2d_type")
        self.assertEqual(rename_dict["vector"], "array_type")

if __name__ == "__main__":
    unittest.main()