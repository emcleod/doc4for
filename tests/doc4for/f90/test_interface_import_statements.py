import unittest
from pathlib import Path
from pyfakefs.fake_filesystem_unittest import TestCase
from doc4for.f90.generate_module_tree import extract_module_data
from doc4for.models.common import ImportType

class TestImportStatements(TestCase):
    maxDiff=None

    def setUp(self):
        self.setUpPyfakefs()

#     def test_import_all_implicit(self):
#         self.fs.create_file(
#             "/fake/path/import_interfaces.f90",
#             contents="""\
# !!*
# ! Module for optimization algorithms
# ! Provides interfaces for objective functions, constraints, and optimizers
# !*!
# module optimization_mod
#     implicit none
#     private
    
#     ! Kind parameter that needs to be imported
#     integer, parameter, public :: wp = selected_real_kind(15)
    
#     ! Custom type that needs to be imported  
#     type, public :: bounds_type
#         real(wp) :: lower, upper
#     end type
    
#     !!* 
#     ! Interface for objective functions
#     ! Functions that take a vector and return a scalar value to minimize
#     !*!
#     public :: objective_func
#     abstract interface
#         !!*
#         ! Objective function interface
#         ! @in x Input vector
#         ! @return Function value
#         !*!
#         function objective_func(x) result(f)
#             real(wp), dimension(:), intent(in) :: x
#             real(wp) :: f
#         end function objective_func
#     end interface
    
#     !!* 
#     ! Interface for constraint functions
#     ! Functions that return true if constraints are satisfied
#     !*!
#     public :: constraint_func
#     abstract interface
#         !!*
#         ! Constraint function interface
#         ! @in x Input vector
#         ! @in bounds Variable bounds
#         ! @return True if constraints are satisfied
#         !*!
#         function constraint_func(x, bounds) result(c)
#             real(wp), dimension(:), intent(in) :: x
#             type(bounds_type), intent(in) :: bounds
#             logical :: c
#         end function constraint_func
#     end interface
    
#     !!* 
#     ! Interface for optimization algorithms
#     ! Standard interface for all optimization methods
#     !*!
#     public :: optimizer_interface
#     interface optimizer_interface
#         !!* 
#         ! Minimize an objective function subject to constraints
#         ! @in obj Function to minimize
#         ! @in constraint Constraint function
#         ! @in x0 Initial guess
#         ! @in bounds Variable bounds
#         ! @return Optimal solution
#         !*!
#         function minimize(obj, constraint, x0, bounds) result(x_opt)
#             import                                    ! Import ALL implicitly
#             procedure(objective_func) :: obj
#             procedure(constraint_func) :: constraint
#             real(wp), dimension(:), intent(in) :: x0
#             type(bounds_type), intent(in) :: bounds
#             real(wp), dimension(size(x0)) :: x_opt
#         end function
#     end interface
    
# contains
#     !!* 
#     ! Implementation of conjugate gradient optimizer
#     ! Uses Fletcher-Reeves formula for beta calculation
#     ! @in obj Function to minimize
#     ! @in constraint Constraint function
#     ! @in x0 Initial guess
#     ! @in bounds Variable bounds
#     ! @return Optimal solution
#     !*!
#     function conjugate_gradient(obj, constraint, x0, bounds) result(x_opt)
#         procedure(objective_func) :: obj
#         procedure(constraint_func) :: constraint
#         real(wp), dimension(:), intent(in) :: x0
#         type(bounds_type), intent(in) :: bounds
#         real(wp), dimension(size(x0)) :: x_opt
        
#         ! Simplified implementation
#         x_opt = x0  ! Just return initial guess
#     end function
# end module optimization_mod
#     """
#         )
            
#         result = extract_module_data([Path("/fake/path/import_interfaces.f90")])
#         self.assertEqual(len(result), 1)
#         module = result[0]
        
#         # Check module description
#         self.assertEqual(module["module_description"], 
#                         "Module for optimization algorithms\n"
#                         "Provides interfaces for objective functions, constraints, and optimizers\n")
        
#         # Check that all interfaces are parsed
#         self.assertEqual(len(module["interfaces"]), 3)
                
#         # Find the optimizer interface
#         optimizer_interface = None
#         for interface in module["interfaces"]:
#             if interface.get("name") == "optimizer_interface":
#                 optimizer_interface = interface
#                 break

#         self.assertIsNotNone(optimizer_interface)

#         # Check the minimize function within the interface
#         self.assertIn("minimize", optimizer_interface["procedures"])
#         minimize = optimizer_interface["procedures"]["minimize"]

#         # Check function description
#         self.assertEqual(minimize["description"], 
#                         "Minimize an objective function subject to constraints\n\n")

#         # Check parameter descriptions
#         self.assertEqual(minimize["in"]["obj"]["description"], "Function to minimize")
#         self.assertEqual(minimize["in"]["constraint"]["description"], "Constraint function")
#         self.assertEqual(minimize["in"]["x0"]["description"], "Initial guess")
#         self.assertEqual(minimize["in"]["bounds"]["description"], "Variable bounds")
#         self.assertEqual(minimize["return"]["description"], "Optimal solution")

#         # Check that import statements are tracked - NOW IN THE PROCEDURE
#         self.assertEqual(len(minimize["imports"]), 1)  # One import statement

#         import_stmt = minimize["imports"][0]
#         self.assertEqual(import_stmt["import_type"], ImportType.IMPLICIT)  # "import" without :: means import all
#         self.assertEqual(len(import_stmt["entities"]), 0)  # No specific entities listed
#         self.assertEqual(len(import_stmt["renames"]), 0)   # No renames

#         # Check that procedure parameters correctly reference interfaces
#         self.assertEqual(minimize["in"]["obj"]["interface_name"], "objective_func")
#         self.assertEqual(minimize["in"]["constraint"]["interface_name"], "constraint_func")

#     def test_import_all(self):
#         self.fs.create_file(
#             "/fake/path/import_interfaces.f90",
#             contents="""\
# !!*
# ! Module for optimization algorithms
# ! Provides interfaces for objective functions, constraints, and optimizers
# !*!
# module optimization_mod
#     implicit none
#     private
    
#     ! Kind parameter that needs to be imported
#     integer, parameter, public :: wp = selected_real_kind(15)
    
#     ! Custom type that needs to be imported  
#     type, public :: bounds_type
#         real(wp) :: lower, upper
#     end type
    
#     !!* 
#     ! Interface for objective functions
#     ! Functions that take a vector and return a scalar value to minimize
#     !*!
#     public :: objective_func
#     abstract interface
#         !!*
#         ! Objective function interface
#         ! @in x Input vector
#         ! @return Function value
#         !*!
#         function objective_func(x) result(f)
#             real(wp), dimension(:), intent(in) :: x
#             real(wp) :: f
#         end function objective_func
#     end interface
    
#     !!* 
#     ! Interface for constraint functions
#     ! Functions that return true if constraints are satisfied
#     !*!
#     public :: constraint_func
#     abstract interface
#         !!*
#         ! Constraint function interface
#         ! @in x Input vector
#         ! @in bounds Variable bounds
#         ! @return True if constraints are satisfied
#         !*!
#         function constraint_func(x, bounds) result(c)
#             real(wp), dimension(:), intent(in) :: x
#             type(bounds_type), intent(in) :: bounds
#             logical :: c
#         end function constraint_func
#     end interface
    
#     !!* 
#     ! Interface for optimization algorithms
#     ! Standard interface for all optimization methods
#     !*!
#     public :: optimizer_interface
#     interface optimizer_interface
#         !!* 
#         ! Minimize an objective function subject to constraints
#         ! @in obj Function to minimize
#         ! @in constraint Constraint function
#         ! @in x0 Initial guess
#         ! @in bounds Variable bounds
#         ! @return Optimal solution
#         !*!
#         function minimize(obj, constraint, x0, bounds) result(x_opt)
#             import :: all
#             procedure(objective_func) :: obj
#             procedure(constraint_func) :: constraint
#             real(wp), dimension(:), intent(in) :: x0
#             type(bounds_type), intent(in) :: bounds
#             real(wp), dimension(size(x0)) :: x_opt
#         end function
#     end interface
    
# contains
#     !!* 
#     ! Implementation of conjugate gradient optimizer
#     ! Uses Fletcher-Reeves formula for beta calculation
#     ! @in obj Function to minimize
#     ! @in constraint Constraint function
#     ! @in x0 Initial guess
#     ! @in bounds Variable bounds
#     ! @return Optimal solution
#     !*!
#     function conjugate_gradient(obj, constraint, x0, bounds) result(x_opt)
#         procedure(objective_func) :: obj
#         procedure(constraint_func) :: constraint
#         real(wp), dimension(:), intent(in) :: x0
#         type(bounds_type), intent(in) :: bounds
#         real(wp), dimension(size(x0)) :: x_opt
        
#         ! Simplified implementation
#         x_opt = x0  ! Just return initial guess
#     end function
# end module optimization_mod
#     """
#         )
            
#         result = extract_module_data([Path("/fake/path/import_interfaces.f90")])
#         self.assertEqual(len(result), 1)
#         module = result[0]
        
#         # Check module description
#         self.assertEqual(module["module_description"], 
#                         "Module for optimization algorithms\n"
#                         "Provides interfaces for objective functions, constraints, and optimizers\n")
        
#         # Check that all interfaces are parsed
#         self.assertEqual(len(module["interfaces"]), 3)
                
#         # Find the optimizer interface
#         optimizer_interface = None
#         for interface in module["interfaces"]:
#             if interface.get("name") == "optimizer_interface":
#                 optimizer_interface = interface
#                 break

#         self.assertIsNotNone(optimizer_interface)

#         # Check the minimize function within the interface
#         self.assertIn("minimize", optimizer_interface["procedures"])
#         minimize = optimizer_interface["procedures"]["minimize"]

#         # Check function description
#         self.assertEqual(minimize["description"], 
#                         "Minimize an objective function subject to constraints\n\n")

#         # Check parameter descriptions
#         self.assertEqual(minimize["in"]["obj"]["description"], "Function to minimize")
#         self.assertEqual(minimize["in"]["constraint"]["description"], "Constraint function")
#         self.assertEqual(minimize["in"]["x0"]["description"], "Initial guess")
#         self.assertEqual(minimize["in"]["bounds"]["description"], "Variable bounds")
#         self.assertEqual(minimize["return"]["description"], "Optimal solution")

#         # Check that import statements are tracked - NOW IN THE PROCEDURE
#         self.assertEqual(len(minimize["imports"]), 1)  # One import statement

#         import_stmt = minimize["imports"][0]
#         self.assertEqual(import_stmt["import_type"], ImportType.ALL)  
#         self.assertEqual(len(import_stmt["entities"]), 0)  # No specific entities listed
#         self.assertEqual(len(import_stmt["renames"]), 0)   # No renames

#         # Check that procedure parameters correctly reference interfaces
#         self.assertEqual(minimize["in"]["obj"]["interface_name"], "objective_func")
#         self.assertEqual(minimize["in"]["constraint"]["interface_name"], "constraint_func")

    # def test_import_with_partial_list(self):
    #     """Test import statement with a partial list of entities."""
    #     self.fs.create_file(
    #         "/fake/path/partial_import.f90",
    #         contents="""\
    # !!*
    # ! Linear algebra module
    # ! Provides types and interfaces for linear algebra operations
    # !*!
    # module linear_algebra_mod
    #     implicit none
    #     private
        
    #     !!* 
    #     ! Vector type
    #     ! Represents a mathematical vector with basic operations
    #     !*!
    #     type, public :: vector_t
    #         real, allocatable :: elements(:)
    #     end type
        
    #     !!* 
    #     ! Matrix type
    #     ! Represents a mathematical matrix with basic operations
    #     !*!
    #     type, public :: matrix_t
    #         real, allocatable :: elements(:,:)
    #     end type
        
    #     !!* 
    #     ! Complex type
    #     ! Represents a complex number
    #     !*!
    #     type, public :: complex_t
    #         real :: real_part
    #         real :: imag_part
    #     end type
        
    #     !!* 
    #     ! Interface for linear solvers
    #     ! Defines the standard interface for linear system solvers
    #     !*!
    #     public :: linear_solver_interface
    #     interface linear_solver_interface
    #         !!*
    #         ! Solve a linear system Ax = b
    #         ! @in A Coefficient matrix
    #         ! @in b Right-hand side vector
    #         ! @return Solution vector x
    #         !*!
    #         function solve_linear_system(A, b) result(x)
    #             import :: matrix_t, vector_t   ! Note: not importing complex_t
    #             type(matrix_t), intent(in) :: A
    #             type(vector_t), intent(in) :: b
    #             type(vector_t) :: x
    #         end function
    #     end interface
        
    # end module linear_algebra_mod
    # """
    #     )
        
    #     result = extract_module_data([Path("/fake/path/partial_import.f90")])
    #     self.assertEqual(len(result), 1)
    #     module = result[0]
        
    #     # Check module description
    #     self.assertEqual(module["module_description"], 
    #                     "Linear algebra module\n"
    #                     "Provides types and interfaces for linear algebra operations\n")
        
    #     # Check type descriptions
    #     vector_type = module["types"]["vector_t"]
    #     self.assertEqual(vector_type["description"], 
    #                     "Vector type\n"
    #                     "Represents a mathematical vector with basic operations\n")
        
    #     matrix_type = module["types"]["matrix_t"]
    #     self.assertEqual(matrix_type["description"], 
    #                     "Matrix type\n"
    #                     "Represents a mathematical matrix with basic operations\n")
        
    #     complex_type = module["types"]["complex_t"]
    #     self.assertEqual(complex_type["description"], 
    #                     "Complex type\n"
    #                     "Represents a complex number\n")
        
    #     # Check interface description
    #     solver_interface = None
    #     for interface in module["interfaces"]:
    #         if interface.get("name") == "linear_solver_interface":
    #             solver_interface = interface
    #             break
        
    #     self.assertIsNotNone(solver_interface)
    #     self.assertEqual(solver_interface["description"], 
    #                     "Interface for linear solvers\n"
    #                     "Defines the standard interface for linear system solvers\n")
        
    #     # Check the solve function within the interface
    #     solve_func = solver_interface["procedures"]["solve_linear_system"]
        
    #     # Check function description
    #     self.assertEqual(solve_func["description"], 
    #                     "Solve a linear system Ax = b\n\n")
        
    #     # Check parameter descriptions
    #     self.assertEqual(solve_func["in"]["A"]["description"], "Coefficient matrix")
    #     self.assertEqual(solve_func["in"]["b"]["description"], "Right-hand side vector")
    #     self.assertEqual(solve_func["return"]["description"], "Solution vector x")
        
    #     # Check that import statement is tracked correctly
    #     self.assertEqual(len(solve_func["imports"]), 1)  # One import statement
        
    #     import_stmt = solve_func["imports"][0]
    #     self.assertEqual(import_stmt["import_type"], ImportType.EXPLICIT)  
    #     self.assertEqual(len(import_stmt["entities"]), 2)
    #     self.assertIn("matrix_t", import_stmt["entities"])
    #     self.assertIn("vector_t", import_stmt["entities"])
    #     self.assertNotIn("complex_t", import_stmt["entities"])  # Not imported
    #     self.assertEqual(len(import_stmt["renames"]), 0)  # No renames in IMPORT
        
    #     # Check that the function parameters use the correct types
    #     self.assertEqual(solve_func["in"]["A"]["type"], "matrix_t")
    #     self.assertEqual(solve_func["in"]["b"]["type"], "vector_t")
    #     self.assertEqual(solve_func["return"]["type"], "vector_t")

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
                import :: array_2d_type, array_type
                type(array_2d_type), intent(in) :: M
                type(array_type), intent(in) :: v
                type(array_type) :: result
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
            if module["module_name"] == "generic_math_types":
                generic_module = module
            elif module["module_name"] == "specialized_math":
                specialized_module = module
        
        self.assertIsNotNone(generic_module)
        self.assertIsNotNone(specialized_module)
        
        # Check module descriptions
        self.assertEqual(generic_module["module_description"], 
                        "Generic mathematical types module\n"
                        "Provides basic types for mathematical operations\n")
        self.assertEqual(specialized_module["module_description"], 
                        "Specialized math module\n"
                        "Uses generic types with domain-specific names\n")
        
        # Check type descriptions in generic module
        array_type = generic_module["types"]["array_type"]
        self.assertEqual(array_type["description"], 
                        "Generic array type\n"
                        "Can be used for vectors or other 1D arrays\n")
        
        array_2d_type = generic_module["types"]["array_2d_type"]
        self.assertEqual(array_2d_type["description"], 
                        "2D array type\n"
                        "Can be used for matrices or other 2D data\n")
        
        # Check interface in specialized module
        interface = specialized_module["interfaces"][0]
        self.assertEqual(interface["description"], 
                        "Interface for domain-specific vector operations\n"
                        "Renames generic types for clarity in this context\n")
        
        # Check the function within the interface
        mult_func = interface["procedures"]["matrix_vector_multiply"]
        self.assertEqual(mult_func["description"], 
                        "Multiplies a matrix by a vector\n\n")
        
        # Check parameter descriptions
        self.assertEqual(mult_func["in"]["M"]["description"], "Matrix (2D array)")
        self.assertEqual(mult_func["in"]["v"]["description"], "Vector (1D array)")
        self.assertEqual(mult_func["return"]["description"], "Result vector")
        
        # Check import renames
        self.assertEqual(mult_func["imports"][0]["import_type"], ImportType.EXPLICIT)
        self.assertEqual(mult_func["imports"][0]["entities"], ["array_2d_type", "array_type"])
                
        # Check that parameters use the renamed types
        self.assertEqual(mult_func["in"]["M"]["type"], "array_2d_type")
        self.assertEqual(mult_func["in"]["v"]["type"], "array_type")
        
        # Check the module function with USE renames
        diag_func = specialized_module["functions"]["extract_diagonal"]
        self.assertEqual(diag_func["description"], 
                        "Example function using the renamed imports\n\n")
        
        # Check USE statement renames
        self.assertEqual(len(diag_func["uses"]), 1)
        self.assertIn("generic_math_types", diag_func["uses"])
        
        uses = diag_func["uses"]["generic_math_types"]
        rename_list = uses["renames"]
        rename_dict = {r["local"]: r["original"] for r in rename_list}
        self.assertEqual(rename_dict["matrix"], "array_2d_type")
        self.assertEqual(rename_dict["vector"], "array_type")

    def test_generic_interface_with_multiple_imports(self):
        """Test generic interface with operator overloading and multiple import statements."""
        self.fs.create_file(
            "/fake/path/operator_overload.f90",
            contents="""\
    !!*
    ! Vector operations module
    ! Provides vector types and overloaded operators
    !*!
    module vector_operations_mod
        implicit none
        private
        
        !!*
        ! 3D vector type
        ! Represents a vector in 3D space
        !*!
        type, public :: vector_type
            real :: x, y, z
        end type
        
        !!*
        ! Metadata type
        ! Contains additional information about operations
        !*!
        type, public :: other_type
            character(len=50) :: operation_name
            integer :: dimension
        end type
        
        !!*
        ! Vector addition operator
        ! Overloads + operator for vector addition
        !*!
        public :: operator(+)
        interface operator(+)
            !!*
            ! Add two vectors component-wise
            ! @in a First vector
            ! @in b Second vector
            ! @return Sum of the two vectors
            !*!
            function add_vectors(a, b) result(c)
                import :: vector_type
                import :: other_type
                type(vector_type), intent(in) :: a, b
                type(vector_type) :: c
            end function
        end interface
        
    contains
        !!*
        ! Implementation of vector addition
        ! @in a First vector
        ! @in b Second vector
        ! @return Component-wise sum
        !*!
        function add_vectors_impl(a, b) result(c)
            type(vector_type), intent(in) :: a, b
            type(vector_type) :: c
            
            c%x = a%x + b%x
            c%y = a%y + b%y
            c%z = a%z + b%z
        end function
        
    end module vector_operations_mod
    """
        )
        
        result = extract_module_data([Path("/fake/path/operator_overload.f90")])
        self.assertEqual(len(result), 1)
        module = result[0]
        
        # Check module description
        self.assertEqual(module["module_description"], 
                        "Vector operations module\n"
                        "Provides vector types and overloaded operators\n")
        
        # Check that types are parsed
        self.assertIn("vector_type", module["types"])
        self.assertIn("other_type", module["types"])
        
        vector_type = module["types"]["vector_type"]
        self.assertEqual(vector_type["description"], 
                        "3D vector type\n"
                        "Represents a vector in 3D space\n")
        
        other_type = module["types"]["other_type"]
        self.assertEqual(other_type["description"], 
                        "Metadata type\n"
                        "Contains additional information about operations\n")
        
        # Check that interface is parsed
        self.assertEqual(len(module["interfaces"]), 1)
        
        operator_interface = module["interfaces"][0]
        
        # Check interface description
        self.assertEqual(operator_interface["description"], 
                        "Vector addition operator\n"
                        "Overloads + operator for vector addition\n")
        
        # Check that it's recognized as an operator interface
        self.assertEqual(operator_interface["operator_symbol"], "+")
        self.assertFalse(operator_interface["name"])  # Operator interfaces don't have names
        
        # Check the function within the interface
        self.assertIn("add_vectors", operator_interface["procedures"])
        add_func = operator_interface["procedures"]["add_vectors"]
        
        # Check function description
        self.assertEqual(add_func["description"], 
                        "Add two vectors component-wise\n\n")
        
        # Check parameter descriptions
        self.assertEqual(add_func["in"]["a"]["description"], "First vector")
        self.assertEqual(add_func["in"]["b"]["description"], "Second vector")
        self.assertEqual(add_func["return"]["description"], "Sum of the two vectors")
        
        # Check that multiple import statements are tracked correctly
        self.assertEqual(len(add_func["imports"]), 2)  # Two separate import statements
        
        # Check first import statement
        import1 = add_func["imports"][0]
        self.assertEqual(import1["import_type"], ImportType.EXPLICIT)
        self.assertEqual(len(import1["entities"]), 1)
        self.assertEqual(import1["entities"][0], "vector_type")
        
        # Check second import statement  
        import2 = add_func["imports"][1]
        self.assertEqual(import2["import_type"], ImportType.EXPLICIT)
        self.assertEqual(len(import2["entities"]), 1)
        self.assertEqual(import2["entities"][0], "other_type")
        
        # Check that function parameters use the correct types
        self.assertEqual(add_func["in"]["a"]["type"], "vector_type")
        self.assertEqual(add_func["in"]["b"]["type"], "vector_type")
        self.assertEqual(add_func["return"]["type"], "vector_type")
        
        # Check that the module function is also parsed (for completeness)
        self.assertIn("add_vectors_impl", module["functions"])
        impl_func = module["functions"]["add_vectors_impl"]
        self.assertEqual(len(impl_func.get("imports", [])), 0)  # Module procedures don't have imports

if __name__ == "__main__":
    unittest.main()