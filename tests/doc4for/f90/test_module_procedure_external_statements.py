import unittest
from pathlib import Path
from pyfakefs.fake_filesystem_unittest import TestCase
from doc4for.f90.generate_module_tree import extract_module_data

class TestExternalStatements(TestCase):
    maxDiff=None

    def setUp(self):
        self.setUpPyfakefs()

    def test_external_statement_in_procedure(self):
        """Test external statements within procedures."""
        self.fs.create_file(
            "/fake/path/external_functions.f90",
            contents="""\
    !!*
    ! Module demonstrating external function declarations
    ! Shows how to interface with external libraries or legacy code
    !*!
    module external_functions_mod
        implicit none
        private
        
        public :: integrate
        
    contains
        !!*
        ! Integrates a function using external libraries
        ! @in func External function to integrate
        ! @in a Lower bound
        ! @in b Upper bound
        ! @return Integral value
        !*!
        function integrate(func, a, b) result(integral)
            external :: func  ! Declares func as external
            real :: func      ! Return type declaration
            real, intent(in) :: a, b
            real :: integral
            integer :: i, n
            real :: h, sum
            
            n = 100
            h = (b - a) / n
            sum = 0.5 * (func(a) + func(b))
            
            do i = 1, n-1
                sum = sum + func(a + i*h)
            end do
            
            integral = h * sum
        end function
        
        !!*
        ! Wrapper for BLAS dgemm routine
        ! Shows external declaration for well-known library functions
        ! @in A First matrix
        ! @in B Second matrix
        ! @return Product matrix C = A*B
        !*! 
        function matrix_multiply(A, B) result(C)
            real, dimension(:,:), intent(in) :: A, B
            real, dimension(size(A,1), size(B,2)) :: C
            
            !!* Declares BLAS dgemm as external *!
            external :: dgemm
            
            ! Simplified call to dgemm
            call dgemm('N', 'N', size(A,1), size(B,2), size(A,2), &
                      1.0, A, size(A,1), B, size(B,1), &
                      0.0, C, size(C,1))
        end function
    end module external_functions_mod
    """
        )
        
        result = extract_module_data([Path("/fake/path/external_functions.f90")])
        self.assertEqual(len(result), 1)
        module = result[0]
        
        # Check module description
        self.assertEqual(module["module_description"], 
                        "Module demonstrating external function declarations\n"
                        "Shows how to interface with external libraries or legacy code\n")
        
        # Check the integrate function
        integrate_func = module["functions"]["integrate"]
        self.assertEqual(integrate_func["description"], 
                        "Integrates a function using external libraries\n\n")
        
        # Check parameter descriptions
        self.assertEqual(integrate_func["in"]["func"]["description"], "External function to integrate")
        self.assertEqual(integrate_func["in"]["a"]["description"], "Lower bound")
        self.assertEqual(integrate_func["in"]["b"]["description"], "Upper bound")
        self.assertEqual(integrate_func["return"]["description"], "Integral value")
        
        # Check that func is marked as procedure and is intent in
        self.assertEqual(integrate_func["in"]["func"]["type"], "PROCEDURE")
        self.assertFalse(integrate_func["in"]["func"]["attributes"])
        self.assertEqual(integrate_func["external_procedures"]["func"]["name"], "func")
        self.assertEqual(integrate_func["external_procedures"]["func"]["procedure_type"], "FUNCTION")

        # Check the matrix_multiply function
        multiply_func = module["functions"]["matrix_multiply"]
        self.assertEqual(multiply_func["description"], 
                        "Wrapper for BLAS dgemm routine\n"
                        "Shows external declaration for well-known library functions\n\n")
        
        # Check that dgemm external declaration was detected
        self.assertNotIn("dgemm", multiply_func["in"])
        self.assertEqual(multiply_func["external_procedures"]["dgemm"]["name"], "dgemm")
        self.assertEqual(multiply_func["external_procedures"]["dgemm"]["procedure_type"], "SUBROUTINE")

    def test_external_and_intrinsic_declarations(self):
        """Test both external and intrinsic statements."""
        self.fs.create_file(
            "/fake/path/mixed_declarations.f90", 
            contents="""\
    !!*
    ! Module demonstrating mixed external and intrinsic declarations
    ! Shows how to explicitly declare procedures as external or intrinsic
    !*!
    module mixed_declarations
        implicit none
        
    contains
        !!*
        ! Function demonstrating explicit intrinsic and external declarations
        ! @in x Input value
        ! @in user_func User-defined function
        ! @return Processed result
        !*!
        function process_with_mixed(x, user_func) result(y)
            real, intent(in) :: x
            real :: y
            
            !!* Explicitly declare as intrinsic to avoid conflicts *!
            intrinsic :: sin, cos, log
            
            !!* Declare the user function as external *!
            external :: user_func
            real :: user_func  ! Return type
            
            ! Use both intrinsic and external functions
            y = sin(x) + cos(x) + log(abs(x) + 1.0) + user_func(x)
        end function
        
        !!*
        ! Subroutine with multiple external declarations
        ! @inout array Data array to process
        ! @in transform1 First transformation function
        ! @in transform2 Second transformation function
        !*!
        subroutine apply_transformations(array, transform1, transform2)
            real, dimension(:), intent(inout) :: array
            
            !!* Multiple external declarations *!
            external :: transform1, transform2
            real :: transform1, transform2
            
            integer :: i
            
            !!* Mark common math functions as intrinsic *!
            intrinsic :: abs, sqrt
            
            do i = 1, size(array)
                array(i) = sqrt(abs(transform1(array(i)) + transform2(array(i))))
            end do
        end subroutine
    end module mixed_declarations
    """
        )
        
        result = extract_module_data([Path("/fake/path/mixed_declarations.f90")])
        self.assertEqual(len(result), 1)
        module = result[0]
        
        # Check module description
        self.assertEqual(module["module_description"], 
                        "Module demonstrating mixed external and intrinsic declarations\n"
                        "Shows how to explicitly declare procedures as external or intrinsic\n")
        
        # Check the process_with_mixed function
        process_func = module["functions"]["process_with_mixed"]
        self.assertEqual(process_func["description"], 
                        "Function demonstrating explicit intrinsic and external declarations\n\n")
        
        # Check parameter descriptions
        self.assertEqual(process_func["in"]["x"]["description"], "Input value")
        self.assertEqual(process_func["in"]["user_func"]["description"], "User-defined function")
        self.assertEqual(process_func["return"]["description"], "Processed result")
        
        # Check that user_func is marked as procedure and is intent in
        self.assertEqual(process_func["in"]["user_func"]["type"], "PROCEDURE")
        self.assertFalse(process_func["in"]["user_func"]["attributes"])
        
        # Check external procedures
        self.assertEqual(process_func["external_procedures"]["user_func"]["name"], "user_func")
        self.assertEqual(process_func["external_procedures"]["user_func"]["procedure_type"], "FUNCTION")
        
        # Check the apply_transformations subroutine
        apply_sub = module["subroutines"]["apply_transformations"]
        self.assertEqual(apply_sub["description"], 
                        "Subroutine with multiple external declarations\n\n")
        
        # Check parameter documentation
        self.assertEqual(apply_sub["in"]["transform1"]["description"], "First transformation function")
        self.assertEqual(apply_sub["in"]["transform2"]["description"], "Second transformation function")
        self.assertEqual(apply_sub["in"]["array"]["description"], "Data array to process")
        self.assertEqual(apply_sub["out"]["array"]["description"], "Data array to process")
        
        # Check that transform functions are marked as procedures
        self.assertEqual(apply_sub["in"]["transform1"]["type"], "PROCEDURE")
        self.assertFalse(apply_sub["in"]["transform1"]["attributes"])
        self.assertEqual(apply_sub["in"]["transform2"]["type"], "PROCEDURE")
        self.assertFalse(apply_sub["in"]["transform2"]["attributes"])
        
        # Check external procedures
        self.assertEqual(apply_sub["external_procedures"]["transform1"]["name"], "transform1")
        self.assertEqual(apply_sub["external_procedures"]["transform1"]["procedure_type"], "FUNCTION")
        
        self.assertEqual(apply_sub["external_procedures"]["transform2"]["name"], "transform2")
        self.assertEqual(apply_sub["external_procedures"]["transform2"]["procedure_type"], "FUNCTION")
                
    def test_complex_external_interface(self):
        """Test complex case with external functions and specific interfaces."""
        self.fs.create_file(
            "/fake/path/complex_external.f90",
            contents="""\
    !!*
    ! Module demonstrating complex external interface patterns
    ! Shows interaction between external declarations and interface blocks
    !*!
    module complex_external_mod
        implicit none
        
        !!*
        ! Interface for external optimization library functions
        ! Defines the expected signature for external optimizers
        !*!
        interface
            !!*
            ! External minimization routine interface
            ! @in func Function to minimize
            ! @in n Dimension of problem
            ! @inout x Starting point and solution
            ! @out fval Final function value
            ! @return Convergence status
            !*!
            subroutine external_minimize(func, n, x, fval)
                external :: func
                real(8) :: func  ! Double precision function
                integer, intent(in) :: n
                real(8), intent(inout) :: x(n)
                real(8), intent(out) :: fval
            end subroutine
        end interface
        
    contains
        !!*
        ! Wrapper for external optimization library
        ! @in objective Function to minimize
        ! @inout x0 Initial guess and solution
        ! @return True if optimization succeeded
        !*!
        function optimize(objective, x0) result(success)
            real(8), dimension(:), intent(inout) :: x0
            logical :: success
            
            !!* User-provided objective function *!
            external :: objective
            real(8) :: objective
            
            !!* External optimization routine from library *!
            external :: lbfgs_minimize  ! L-BFGS from external library
            
            real(8) :: fval
            integer :: n
            
            !!* Interface aliases for convenience *!
            procedure(external_minimize), pointer :: minimize_ptr => null()
            
            n = size(x0)
            
            ! Point to the external routine
            minimize_ptr => lbfgs_minimize
            
            ! Call external optimizer
            call minimize_ptr(objective, n, x0, fval)
            
            success = (fval < 1.0d-6)
        end function
        
        !!*
        ! Function using callback with external declaration
        ! @in data Input data array
        ! @in callback External processing function
        ! @return Processed sum
        !*!
        function process_data_external(data, callback) result(sum)
            real, dimension(:), intent(in) :: data
            real :: sum
            
            !!* External callback function *!
            external :: callback
            real :: callback  ! Return type
            !!* Alternative interface style *!
            interface
                !!* Expected signature for callback *!
                function callback_interface(x) result(y)
                    real, intent(in) :: x
                    real :: y
                end function
            end interface
            
            integer :: i
            
            sum = 0.0
            do i = 1, size(data)
                sum = sum + callback(data(i))
            end do
        end function
    end module complex_external_mod
    """
        )
        
            #TODO callback appears in 'out'
            #TODO the interface isn't being picked up - maybe because of place?
        result = extract_module_data([Path("/fake/path/complex_external.f90")])
        self.assertEqual(len(result), 1)
        module = result[0]
        
        # Check module description
        self.assertEqual(module["module_description"], 
                        "Module demonstrating complex external interface patterns\n"
                        "Shows interaction between external declarations and interface blocks\n")
        
        # Check interface block
        external_interface = module["interfaces"][0]
        self.assertEqual(external_interface["description"], 
                        "Interface for external optimization library functions\n"
                        "Defines the expected signature for external optimizers\n")
        
        # Check the external_minimize subroutine interface
        minimize_sub = external_interface["procedures"]["external_minimize"]
        self.assertEqual(minimize_sub["description"], 
                        "External minimization routine interface\n\n")
        
        # Check parameter descriptions
        self.assertEqual(minimize_sub["in"]["func"]["description"], "Function to minimize")
        self.assertEqual(minimize_sub["in"]["n"]["description"], "Dimension of problem")
        self.assertEqual(minimize_sub["in"]["x"]["description"], "Starting point and solution")
        self.assertEqual(minimize_sub["out"]["x"]["description"], "Starting point and solution")
        self.assertEqual(minimize_sub["out"]["fval"]["description"], "Final function value")
        
        # Check that func is marked as a procedure
        self.assertEqual(minimize_sub["in"]["func"]["type"], "PROCEDURE")
        self.assertFalse(minimize_sub["in"]["func"]["attributes"])
        
        # Check external procedures in interface
        self.assertEqual(minimize_sub["external_procedures"]["func"]["name"], "func")
        self.assertEqual(minimize_sub["external_procedures"]["func"]["procedure_type"], "FUNCTION")
        
        # Check the optimize function
        optimize_func = module["functions"]["optimize"]
        self.assertEqual(optimize_func["description"], 
                        "Wrapper for external optimization library\n\n")
        
        # Check external procedure parameters
        self.assertEqual(optimize_func["in"]["objective"]["type"], "PROCEDURE")
        self.assertFalse(optimize_func["in"]["objective"]["attributes"])
        
        # Check external procedures
        self.assertEqual(len(optimize_func["external_procedures"]), 2)
        
        # Check objective external procedure
        self.assertEqual(optimize_func["external_procedures"]["objective"]["name"], "objective")
        self.assertEqual(optimize_func["external_procedures"]["objective"]["procedure_type"], "FUNCTION")
        
        # Check lbfgs_minimize external procedure
        self.assertEqual(optimize_func["external_procedures"]["lbfgs_minimize"]["name"], "lbfgs_minimize")
        self.assertEqual(optimize_func["external_procedures"]["lbfgs_minimize"]["procedure_type"], "SUBROUTINE")
                
        # Check process_data_external function
        process_func = module["functions"]["process_data_external"]
        self.assertEqual(process_func["description"], 
                        "Function using callback with external declaration\n\n")
        
        # Check callback parameter
        self.assertEqual(process_func["in"]["callback"]["type"], "PROCEDURE")
        self.assertFalse(process_func["in"]["callback"]["attributes"])
        
        # Check callback external procedure
        self.assertEqual(process_func["external_procedures"]["callback"]["name"], "callback")
        self.assertEqual(process_func["external_procedures"]["callback"]["procedure_type"], "FUNCTION")
        
        # Check that nested interface is also captured (keeping this since it's not intrinsic-related)
        self.assertEqual(len(process_func["argument_interfaces"]), 1)
        self.assertIn("callback", process_func["argument_interfaces"])
        callback_interface = process_func["argument_interfaces"]["callback"]
        self.assertEqual(callback_interface["description"], "Alternative interface style\n")
        
        # Check function signature in nested interface
        callback_func = callback_interface["procedures"]["callback_interface"]
        self.assertEqual(callback_func["description"], "Expected signature for callback\n")
        

if __name__ == "__main__":
    unittest.main()