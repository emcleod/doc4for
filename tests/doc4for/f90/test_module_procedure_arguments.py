import unittest
from pathlib import Path
from pyfakefs.fake_filesystem_unittest import TestCase
from doc4for.f90.generate_module_tree import extract_module_data
from doc4for.models.common import BindingTypeEnum

class TestInterfaces(TestCase):
    maxDiff=None

    def setUp(self):
        self.setUpPyfakefs()

    def test_abstract_interface_with_import(self):
        """Test abstract interface that imports other interfaces for procedure types."""
        self.fs.create_file(
            "/fake/path/interface_imports.f90",
            contents="""\
    module optimization_mod
        implicit none

        !!* First, define abstract interfaces *!
        abstract interface
            function objective_func(x)
                real, intent(in) :: x
                real :: objective_func
            end function objective_func
            
            function gradient_func(x)
                real, intent(in) :: x
                real :: gradient_func
            end function gradient_func
        end interface

        !!* The optimization interface *!
        interface minimization_interface
            function minimize(obj, grad, x0) result(xmin)
                import :: objective_func, gradient_func
                implicit none
                procedure(objective_func) :: obj
                procedure(gradient_func) :: grad
                real, intent(in) :: x0
                real :: xmin
            end function minimize
        end interface

    end module optimization_mod
    """
        )
        result = extract_module_data([Path("/fake/path/interface_imports.f90")])
        module = result[0]
        
        # Check that we have two interfaces
        self.assertEqual(len(module["interfaces"]), 2)
        
        # First interface should contain both objective_func and gradient_func
        first_interface = module["interfaces"][0]
        self.assertEqual(first_interface["attributes"], ["abstract"])
        self.assertEqual(len(first_interface["procedures"]), 2)
        self.assertIn("objective_func", first_interface["procedures"])
        self.assertIn("gradient_func", first_interface["procedures"])
        
        # Second interface should contain minimize function and have a name
        second_interface = module["interfaces"][1]
        self.assertEqual(second_interface["name"], "minimization_interface")
        self.assertNotIn("abstract", second_interface["attributes"])
        self.assertEqual(len(second_interface["procedures"]), 1)
        
        minimize = second_interface["procedures"]["minimize"]
        self.assertEqual(minimize["arguments"], ["obj", "grad", "x0"])
        self.assertEqual(minimize["return"]["xmin"]["type"], "real")
        
        # Check that procedure arguments reference the imported interfaces correctly
        self.assertEqual(minimize["in"]["obj"]["type"], "procedure")
        self.assertEqual(minimize["in"]["obj"]["interface_name"], "objective_func")
        self.assertEqual(minimize["in"]["grad"]["type"], "procedure")
        self.assertEqual(minimize["in"]["grad"]["interface_name"], "gradient_func")

    def test_reversed_order_nested_interfaces(self):
        """Test when procedure declarations come before interface definitions."""
        self.fs.create_file(
            "/fake/path/reversed_interfaces.f90",
            contents="""\
    module callback_reverse_order_mod
        implicit none

        abstract interface
            subroutine matrix_ops(transform1, transform2, data)
                implicit none
                
                ! First declare all procedure arguments
                procedure(matrix_func) :: transform1, transform2
                real, dimension(:,:), intent(inout) :: data
                
                ! Now define interfaces, but in reverse order!
                interface
                    function matrix_func(x)
                        real, dimension(:,:), intent(in) :: x
                        real, dimension(size(x,1), size(x,2)) :: matrix_func
                    end function matrix_func
                end interface
                
            end subroutine matrix_ops
        end interface

    end module callback_reverse_order_mod
    """
        )
        result = extract_module_data([Path("/fake/path/reversed_interfaces.f90")])
        module = result[0]
        self.assertEqual(module["module_description"], "")
        interface = module["interfaces"][0]
        self.assertEqual(interface["description"], "")
        # Check the matrix_ops subroutine
        matrix_ops = interface["procedures"]["matrix_ops"]
        self.assertEqual(matrix_ops["description"], "")
        self.assertEqual(matrix_ops["arguments"], ["transform1", "transform2", "data"])
        
        # Check that both transform arguments reference the matrix_func interface
        self.assertEqual(matrix_ops["in"]["transform1"]["description"], "")
        self.assertEqual(matrix_ops["in"]["transform1"]["type"], "procedure")
        self.assertEqual(matrix_ops["in"]["transform1"]["interface_name"], "matrix_func")
        self.assertEqual(matrix_ops["in"]["transform2"]["description"], "")
        self.assertEqual(matrix_ops["in"]["transform2"]["type"], "procedure")
        self.assertEqual(matrix_ops["in"]["transform2"]["interface_name"], "matrix_func")
        self.assertEqual(matrix_ops["in"]["data"]["type"], "real")
        self.assertEqual(matrix_ops["in"]["data"]["dimension"], ": (allocatable) &times; : (allocatable)")
                        
        # Check that we have the nested interface for both arguments
        self.assertEqual(len(matrix_ops["argument_interfaces"]), 2)

        # Check transform1's interface
        matrix_func_interface1 = matrix_ops["argument_interfaces"]["transform1"]
        self.assertEqual(matrix_func_interface1["description"], "")
        matrix_func1 = matrix_func_interface1["procedures"]["matrix_func"]
        self.assertEqual(matrix_func1["return"]["matrix_func"]["type"], "real")
        self.assertEqual(matrix_func1["return"]["matrix_func"]["dimension"], "1:size(x,1) &times; 1:size(x,2)")

        # Check transform2's interface (should be the same)
        matrix_func_interface2 = matrix_ops["argument_interfaces"]["transform2"]
        self.assertEqual(matrix_func_interface2["description"], "")
        matrix_func2 = matrix_func_interface2["procedures"]["matrix_func"]
        self.assertEqual(matrix_func2["return"]["matrix_func"]["type"], "real")
        self.assertEqual(matrix_func2["return"]["matrix_func"]["dimension"], "1:size(x,1) &times; 1:size(x,2)")

    def test_reversed_order_nested_interfaces_with_comments(self):
        """Test when procedure declarations come before interface definitions."""
        self.fs.create_file(
            "/fake/path/reversed_interfaces.f90",
            contents="""\
    module callback_reverse_order_mod
        implicit none

        !!*
        ! Defines an interface for matrix operations that performs two transforms
        ! on the data.
        !*!
        abstract interface
            !!*
            ! A function definition
            ! @inout transform1 the first transformation
            ! @inout transform2 the second transformation
            ! @inout data the data to be transformed
            !*!
            function matrix_ops(transform1, transform2, data)
                implicit none
                
                procedure(matrix_func) :: transform1, transform2
                real, dimension(:,:), intent(inout) :: data
                
                !!* 
                ! Defines the matrix function
                !*!
                interface
                    function matrix_func(x)
                        real, dimension(:,:), intent(in) :: x
                        real, dimension(size(x,1), size(x,2)) :: matrix_func
                    end function matrix_func
                end interface
                
            end function matrix_ops
        end interface

    end module callback_reverse_order_mod
    """
        )
        result = extract_module_data([Path("/fake/path/reversed_interfaces.f90")])
        module = result[0]
        self.assertEqual(module["module_description"], "")
        interface = module["interfaces"][0]
        self.assertEqual(interface["description"], "\nDefines an interface for matrix operations that performs two transforms\non the data.\n\n")
        # Check the matrix_ops subroutine
        matrix_ops = interface["procedures"]["matrix_ops"]
        self.assertEqual(matrix_ops["description"], "\nA function definition\n\n")
        self.assertEqual(matrix_ops["arguments"], ["transform1", "transform2", "data"])
        
        # Check that both transform arguments reference the matrix_func interface
        self.assertEqual(matrix_ops["in"]["transform1"]["description"], "the first transformation")
        self.assertEqual(matrix_ops["in"]["transform1"]["type"], "procedure")
        self.assertEqual(matrix_ops["in"]["transform1"]["interface_name"], "matrix_func")
        self.assertEqual(matrix_ops["in"]["transform2"]["description"], "the second transformation")
        self.assertEqual(matrix_ops["in"]["transform2"]["type"], "procedure")
        self.assertEqual(matrix_ops["in"]["transform2"]["interface_name"], "matrix_func")
        self.assertEqual(matrix_ops["in"]["data"]["type"], "real")
        self.assertEqual(matrix_ops["in"]["data"]["dimension"], ": (allocatable) &times; : (allocatable)")
                
        # Check that we have the nested interface for the transform1 argument
        self.assertEqual(len(matrix_ops["argument_interfaces"]), 2)
        matrix_func_interface = matrix_ops["argument_interfaces"]["transform1"]
        self.assertEqual(matrix_func_interface["description"], "\nDefines the matrix function\n\n")
        matrix_func = matrix_func_interface["procedures"]["matrix_func"]
        self.assertEqual(matrix_func["return"]["matrix_func"]["type"], "real")
        self.assertEqual(matrix_func["return"]["matrix_func"]["dimension"], "1:size(x,1) &times; 1:size(x,2)")     

        matrix_func_interface = matrix_ops["argument_interfaces"]["transform2"]
        self.assertEqual(matrix_func_interface["description"], "\nDefines the matrix function\n\n")
        matrix_func = matrix_func_interface["procedures"]["matrix_func"]
        self.assertEqual(matrix_func["return"]["matrix_func"]["type"], "real")
        self.assertEqual(matrix_func["return"]["matrix_func"]["dimension"], "1:size(x,1) &times; 1:size(x,2)")     

    def test_non_sequential_interface_matching(self):
        """Test when interface blocks are not in argument order."""
        self.fs.create_file(
            "/fake/path/scrambled_interfaces.f90",
            contents="""\
    module mixed_interface_order_mod
        implicit none

        abstract interface
            subroutine complex_processor(func1, data, func2, settings, func3)
                implicit none
                
                ! Declare procedure arguments first
                procedure(scalar_func) :: func1
                procedure(vector_func) :: func2
                procedure(matrix_func) :: func3
                
                real, dimension(:), intent(inout) :: data
                integer, intent(in) :: settings
                
                ! Interface blocks not in order of arguments
                interface  ! This is for func3, not func1
                    function matrix_func(x)
                        real, dimension(:,:), intent(in) :: x
                        real, dimension(size(x,1), size(x,2)) :: matrix_func
                    end function matrix_func
                end interface

                interface  ! This is for func1, not func2
                    function scalar_func(x)
                        real, intent(in) :: x
                        real :: scalar_func
                    end function scalar_func
                end interface
                
                interface  ! This is for func2, not func3
                    function vector_func(x)
                        real, dimension(:), intent(in) :: x
                        real, dimension(size(x)) :: vector_func
                    end function vector_func
                end interface
                
            end subroutine complex_processor
        end interface

    end module mixed_interface_order_mod
    """
        )
        result = extract_module_data([Path("/fake/path/scrambled_interfaces.f90")])
        module = result[0]
        interface = module["interfaces"][0]
        
        # Check the complex_processor subroutine
        processor = interface["procedures"]["complex_processor"]
        self.assertEqual(processor["arguments"], ["func1", "data", "func2", "settings", "func3"])
        
        # Check that each procedure argument is correctly matched to its interface
        self.assertEqual(processor["in"]["func1"]["type"], "procedure")
        self.assertEqual(processor["in"]["func1"]["interface_name"], "scalar_func")
        
        self.assertEqual(processor["in"]["func2"]["type"], "procedure")
        self.assertEqual(processor["in"]["func2"]["interface_name"], "vector_func")
        
        self.assertEqual(processor["in"]["func3"]["type"], "procedure")
        self.assertEqual(processor["in"]["func3"]["interface_name"], "matrix_func")
        
        # Check non-procedure arguments
        self.assertEqual(processor["in"]["data"]["type"], "real")
        self.assertEqual(processor["in"]["settings"]["type"], "integer")
                
        # Check that we have all three nested interfaces
        self.assertEqual(len(processor["argument_interfaces"]), 3)
        
        # Verify the interfaces are correctly parsed despite order mismatch
        scalar_interface = processor["argument_interfaces"]["func1"]
        self.assertEqual(scalar_interface["procedures"]["scalar_func"]["return"]["scalar_func"]["type"], "real")
        
        vector_interface = processor["argument_interfaces"]["func2"]
        self.assertEqual(vector_interface["procedures"]["vector_func"]["return"]["vector_func"]["dimension"], "1:size(x)")
        
        matrix_interface = processor["argument_interfaces"]["func3"]
        self.assertEqual(matrix_interface["procedures"]["matrix_func"]["return"]["matrix_func"]["dimension"], "1:size(x,1) &times; 1:size(x,2)")

    def test_procedure_declarations_after_interfaces(self):
        """Test when procedure declarations come after interface definitions."""
        self.fs.create_file(
            "/fake/path/declaration_order.f90",
            contents="""\
    module interface_order_edge_case_mod
        implicit none

        abstract interface
            subroutine process_data(callback1, callback2, data)
                implicit none
                
                ! Declare the dummy arguments first
                ! These are the formal parameters
                real, dimension(:), intent(inout) :: data
                
                ! Define interfaces in REVERSE order from how they appear in argument list
                interface
                    ! This is the interface for the 2nd argument (callback2)
                    subroutine second_callback(x) 
                        real, dimension(:), intent(inout) :: x
                    end subroutine second_callback
                end interface
                
                interface 
                    ! This is the interface for the 1st argument (callback1)
                    subroutine first_callback(x)
                        real, dimension(:), intent(inout) :: x
                    end subroutine first_callback
                end interface
                
                ! Now declare which interface applies to which argument
                procedure(first_callback) :: callback1
                procedure(second_callback) :: callback2
                
            end subroutine process_data
        end interface
    end module interface_order_edge_case_mod
    """
        )
        result = extract_module_data([Path("/fake/path/declaration_order.f90")])
        module = result[0]
        interface = module["interfaces"][0]
        
        # Check the process_data subroutine
        process_data = interface["procedures"]["process_data"]
        self.assertEqual(process_data["arguments"], ["callback1", "callback2", "data"])
        
        # Check that callback1 is correctly matched to first_callback
        self.assertEqual(process_data["in"]["callback1"]["type"], "procedure")
        self.assertEqual(process_data["in"]["callback1"]["interface_name"], "first_callback")
        
        # Check that callback2 is correctly matched to second_callback
        self.assertEqual(process_data["in"]["callback2"]["type"], "procedure")
        self.assertEqual(process_data["in"]["callback2"]["interface_name"], "second_callback")
        
        # Check the data argument
        self.assertEqual(process_data["in"]["data"]["type"], "real")
        self.assertEqual(process_data["in"]["data"]["dimension"], ": (allocatable)")
        self.assertEqual(process_data["out"]["data"]["type"], "real")
        self.assertEqual(process_data["out"]["data"]["dimension"], ": (allocatable)")
        
        # Check that we have both nested interfaces
        self.assertEqual(len(process_data["argument_interfaces"]), 2)
                
        # Check that we have both nested interfaces
        self.assertEqual(len(process_data["argument_interfaces"]), 2)

        # Verify the interfaces are correctly parsed and matched
        callback1_interface = process_data["argument_interfaces"]["callback1"]  # Changed from first_callback
        self.assertEqual(callback1_interface["procedures"]["first_callback"]["arguments"], ["x"])
        self.assertEqual(callback1_interface["procedures"]["first_callback"]["in"]["x"]["dimension"], ": (allocatable)")
        self.assertEqual(callback1_interface["procedures"]["first_callback"]["out"]["x"]["dimension"], ": (allocatable)")

        callback2_interface = process_data["argument_interfaces"]["callback2"]  # Changed from second_callback
        self.assertEqual(callback2_interface["procedures"]["second_callback"]["arguments"], ["x"])
        self.assertEqual(callback2_interface["procedures"]["second_callback"]["in"]["x"]["dimension"], ": (allocatable)")
        self.assertEqual(callback2_interface["procedures"]["second_callback"]["out"]["x"]["dimension"], ": (allocatable)")    

if __name__ == "__main__":
    unittest.main()
