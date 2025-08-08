import unittest
from pathlib import Path
from pyfakefs.fake_filesystem_unittest import TestCase
from doc4for.f90.generate_module_tree import extract_module_data

class TestUseStatements(TestCase):
    maxDiff=None

    def setUp(self):
        self.setUpPyfakefs()

    def test_use_statement_in_procedure(self):
        """Test tracking of USE statements within procedures."""
        # Create two module files
        self.fs.create_file(
            "/fake/path/function_utils.f90",
            contents="""\
    !!* 
    ! Utility module containing standard function interfaces
    ! Used throughout the numerical library
    !*!
    module function_utils
        implicit none
        private

        !!* 
        ! Standard interface for numerical functions
        ! @in x Value at which to evaluate
        ! @return Value of function at x
        !*!
        public :: func_interface
        abstract interface
            function func_interface(x) result(y)
                real, intent(in) :: x
                real :: y
            end function
        end interface

    end module function_utils
    """
        )
        
        self.fs.create_file(
            "/fake/path/numerical_integrator.f90",
            contents="""\
    !!*
    ! Numerical integration module
    ! Provides various integration methods
    !*!
    module numerical_integrator
        implicit none
        private
        
        public :: integrate

    contains
        !!*
        ! Integrates a function over an interval
        ! @in func Function to integrate
        ! @in a Lower bound
        ! @in b Upper bound
        ! @return Integral value
        !*!
        function integrate(func, a, b) result(integral)
            !!* Imports only the function interface *!
            use function_utils, only: func_interface
            
            procedure(func_interface) :: func
            real, intent(in) :: a, b
            real :: integral, h, sum
            integer :: i, n
            
            n = 100
            h = (b - a) / n
            sum = 0.5 * (func(a) + func(b))
            
            do i = 1, n-1
                sum = sum + func(a + i*h)
            end do
            
            integral = h * sum
        end function
    end module numerical_integrator
    """
        )
        
        result = extract_module_data([
            Path("/fake/path/function_utils.f90"),
            Path("/fake/path/numerical_integrator.f90")
        ])
        
        # First check that both modules are parsed
        self.assertEqual(len(result), 2)
        
        # Check function_utils module documentation
        func_utils_module = None
        num_module = None
        for module in result:
            if module["module_name"] == "function_utils":
                func_utils_module = module
            elif module["module_name"] == "numerical_integrator":
                num_module = module
        
        assert func_utils_module is not None
        assert num_module is not None
        
        # Check module descriptions
        self.assertEqual(func_utils_module["module_description"], 
                        "Utility module containing standard function interfaces\nUsed throughout the numerical library\n")
        self.assertEqual(num_module["module_description"], 
                        "Numerical integration module\nProvides various integration methods\n")
        
        # Check the function in the numerical_integrator module
        integrate_func = num_module["functions"]["integrate"]
        
        # Check function description
        self.assertEqual(integrate_func["description"], 
                        "Integrates a function over an interval\n\n")
        
        # Check function basic info
        self.assertEqual(integrate_func["arguments"], ["func", "a", "b"])
        
        # Check that the USE statement was detected and recorded
        self.assertEqual(len(integrate_func["uses"]), 1)
        self.assertEqual(integrate_func["uses"]["function_utils"]["module_name"], "function_utils")
        self.assertEqual(integrate_func["uses"]["function_utils"]["selections"], ["func_interface"])
        self.assertEqual(integrate_func["uses"]["function_utils"]["description"], "") # shouldn't document anything inside the procedure body
        
        # Check that the procedure argument correctly references the imported interface
        self.assertEqual(integrate_func["in"]["func"]["type"], "PROCEDURE")
        self.assertEqual(integrate_func["in"]["func"]["description"], "Function to integrate")
        self.assertEqual(integrate_func["in"]["func"]["interface_name"], "func_interface")
        
        # Check other parameter descriptions
        self.assertEqual(integrate_func["in"]["a"]["description"], "Lower bound")
        self.assertEqual(integrate_func["in"]["b"]["description"], "Upper bound")
        self.assertEqual(integrate_func["return"]["description"], "Integral value")     

    def test_use_statement_rename_in_procedure(self):
        """Test tracking of USE statements with renames within procedures."""
        # First module defines a type
        self.fs.create_file(
            "/fake/path/types_mod.f90",
            contents="""\
    !!*
    ! Module containing custom types for mathematical operations
    ! These types are designed to work with BLAS/LAPACK routines
    !*!
    module types_mod
        implicit none
        private
        
        !!* 
        ! A vector of real numbers
        ! Supports various mathematical operations
        !*!
        type, public :: vector_t
            real, allocatable :: values(:)
        contains
            procedure :: norm => vector_norm
            procedure :: dot => vector_dot
        end type
        
    contains
        !!*
        ! Calculates the Euclidean norm of the vector
        ! @in this The vector
        ! @return Euclidean norm
        !*!
        function vector_norm(this) result(n)
            class(vector_t), intent(in) :: this
            real :: n
            
            n = sqrt(sum(this%values**2))
        end function
        
        !!*
        ! Calculates the dot product of two vectors
        ! @in this First vector
        ! @in other Second vector
        ! @return Dot product value
        !*!
        function vector_dot(this, other) result(d)
            class(vector_t), intent(in) :: this, other
            real :: d
            
            d = sum(this%values * other%values)
        end function
    end module types_mod
    """
        )
        
        # Second module uses the type with renaming
        self.fs.create_file(
            "/fake/path/math_ops.f90",
            contents="""\
    !!*
    ! Mathematical operations module
    ! Provides high-level mathematical functions
    !*!
    module math_ops
        implicit none
        private
        
        public :: calc_angle
        
    contains
        !!*
        ! Calculates angle between two vectors
        ! @in v1 First vector
        ! @in v2 Second vector
        ! @return Angle in radians
        !*!
        function calc_angle(v1, v2) result(angle)
            use types_mod, only: vec => vector_t
            
            type(vec), intent(in) :: v1, v2
            real :: angle, dot_prod, norm1, norm2
            
            ! Calculate using a.b = |a|.|b|.cos(angle)
            dot_prod = v1%dot(v2)
            norm1 = v1%norm()
            norm2 = v2%norm()
            
            angle = acos(dot_prod / (norm1 * norm2))
        end function
    end module math_ops
    """
        )
        
        result = extract_module_data([
            Path("/fake/path/types_mod.f90"),
            Path("/fake/path/math_ops.f90")
        ])
        
        # Find both modules
        types_module = None
        math_module = None
        for module in result:
            if module["module_name"] == "types_mod":
                types_module = module
            elif module["module_name"] == "math_ops":
                math_module = module
        
        assert types_module is not None
        assert math_module is not None
        
        # Check module descriptions
        self.assertEqual(types_module["module_description"], 
                        "Module containing custom types for mathematical operations\nThese types are designed to work with BLAS/LAPACK routines\n")
        self.assertEqual(math_module["module_description"], 
                        "Mathematical operations module\nProvides high-level mathematical functions\n")
        
        # Check the type documentation in types_mod
        vector_type = types_module["types"]["vector_t"]
        self.assertEqual(vector_type["description"], 
                        "A vector of real numbers\nSupports various mathematical operations\n")
        
        # Check the function in math_ops
        angle_func = math_module["functions"]["calc_angle"]
        
        # Check function description
        self.assertEqual(angle_func["description"], 
                        "Calculates angle between two vectors\n\n")
        
        # Check that the USE statement with rename was detected
        self.assertEqual(len(angle_func["uses"]), 1)
        self.assertIn("types_mod", angle_func["uses"])
        self.assertEqual(len(angle_func["uses"]["types_mod"]["renames"]), 1)
        self.assertEqual(angle_func["uses"]["types_mod"]["selections"], ["vector_t"])
        self.assertEqual(angle_func["uses"]["types_mod"]["renames"][0]["local"], "vec")
        self.assertEqual(angle_func["uses"]["types_mod"]["renames"][0]["original"], "vector_t")
        
        # Check parameter descriptions
        self.assertEqual(angle_func["in"]["v1"]["description"], "First vector")
        self.assertEqual(angle_func["in"]["v2"]["description"], "Second vector")
        self.assertEqual(angle_func["return"]["description"], "Angle in radians")
        
        # Check that the type references correctly point to the imported type
        self.assertEqual(angle_func["in"]["v1"]["type"], "vec")

    # def test_use_statement_with_submodules(self):
    #     """Test tracking of USE statements with Fortran 2008 submodules."""
    #     # Parent module with interface
    #     self.fs.create_file(
    #         "/fake/path/parent_mod.f90",
    #         contents="""\
    # !!*
    # ! Parent module defining interfaces
    # ! Implementations are provided in submodules for encapsulation
    # !*!
    # module parent_mod
    #     implicit none
        
    #     interface
    #         !!* 
    #         ! Calculate distance between points
    #         ! @in a First point
    #         ! @in b Second point
    #         ! @return Distance
    #         !*!
    #         function distance(a, b) result(d)
    #             real, dimension(:), intent(in) :: a, b
    #             real :: d
    #         end function
    #     end interface
        
    # end module parent_mod
    # """
    #     )
        
    #     # Submodule implementation
    #     self.fs.create_file(
    #         "/fake/path/parent_submod.f90",
    #         contents="""\
    # !!*
    # ! Implementation submodule for parent_mod
    # ! Contains the actual implementation of distance calculation
    # !*!
    # submodule (parent_mod) parent_implementation
    #     implicit none
        
    # contains
    #     !!*
    #     ! Implementation of Euclidean distance calculation
    #     !*!
    #     module function distance(a, b) result(d)
    #         real, dimension(:), intent(in) :: a, b
    #         real :: d
            
    #         d = sqrt(sum((a - b)**2))
    #     end function
    # end submodule parent_implementation
    # """
    #     )
        
    #     # Module that uses the parent module
    #     self.fs.create_file(
    #         "/fake/path/user_mod.f90",
    #         contents="""\
    # !!*
    # ! User module that demonstrates usage of parent module functions
    # ! Shows how to use functionality defined in submodules
    # !*!
    # module user_mod
    #     implicit none
        
    # contains
    #     !!*
    #     ! Calculate average distance between multiple points
    #     ! @in points Array of points
    #     ! @return Average distance
    #     !*!
    #     function avg_distance(points) result(avg)
    #         use parent_mod
            
    #         real, dimension(:,:), intent(in) :: points
    #         real :: avg
    #         real :: total_dist
    #         integer :: i, j, n
            
    #         n = size(points, 1)
    #         total_dist = 0.0
            
    #         do i = 1, n
    #             do j = i+1, n
    #                 total_dist = total_dist + distance(points(i,:), points(j,:))
    #             end do
    #         end do
            
    #         avg = total_dist / (n * (n-1) / 2)
    #     end function
    # end module user_mod
    # """
    #     )
        
    #     result = extract_module_data([
    #         Path("/fake/path/parent_mod.f90"),
    #         Path("/fake/path/parent_submod.f90"),
    #         Path("/fake/path/user_mod.f90")
    #     ])
        
    #     # Find the modules
    #     parent_module = None
    #     user_module = None
    #     for module in result:
    #         if module["name"] == "parent_mod":
    #             parent_module = module
    #         elif module["name"] == "user_mod":
    #             user_module = module
        
    #     self.assertIsNotNone(parent_module)
    #     self.assertIsNotNone(user_module)
        
    #     # Check module descriptions
    #     self.assertEqual(parent_module["module_description"], 
    #                     "\nParent module defining interfaces\n"
    #                     "Implementations are provided in submodules for encapsulation\n\n")
    #     self.assertEqual(user_module["module_description"], 
    #                     "\nUser module that demonstrates usage of parent module functions\n"
    #                     "Shows how to use functionality defined in submodules\n\n")
        
    #     # Check parent module interface description
    #     distance_interface = parent_module["interfaces"][0]
    #     self.assertEqual(distance_interface["procedures"]["distance"]["description"], 
    #                     "\nCalculate distance between points\n\n")
        
    #     # Check the function and its documentation
    #     avg_func = user_module["functions"]["avg_distance"]
        
    #     # Check function description
    #     self.assertEqual(avg_func["description"], 
    #                     "\nCalculate average distance between multiple points\n\n")
        
    #     # Check parameter descriptions
    #     self.assertEqual(avg_func["in"]["points"]["description"], "Array of points")
    #     self.assertEqual(avg_func["return"]["avg"]["description"], "Average distance")
        
    #     # Check that the USE statement was detected
    #     self.assertEqual(len(avg_func["uses"]), 1)
    #     self.assertEqual(avg_func["uses"][0]["module"], "parent_mod")
    #     self.assertEqual(avg_func["uses"][0]["only"], [])  # No 'only' clause, imports everything
        
    #     # Check that imported procedures are tracked
    #     self.assertTrue("imported_procedures" in avg_func)
    #     self.assertIn("distance", avg_func["imported_procedures"])
    #     self.assertEqual(avg_func["imported_procedures"]["distance"]["module"], "parent_mod")

if __name__ == '__main__':
    unittest.main()
