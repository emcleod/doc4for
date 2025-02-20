import unittest
from pathlib import Path
from pyfakefs.fake_filesystem_unittest import TestCase
from doc4for.f90.generate_module_tree import extract_module_data
from doc4for.models.common import BindingTypeEnum

class TestInterfaces(TestCase):
    maxDiff=None

    def setUp(self):
        self.setUpPyfakefs()

    def test_abstract_interface(self):
        self.fs.create_file(
            '/fake/path/abstract_interface.f90',
            contents='''\
module abstract_interface_mod
    implicit none

    !!*
    ! An abstract interface containing a function <code>func</code>
    !*!
    abstract interface
        !!* Transforms x into y somehow 
        !*!
        function func(x) result(y)
            real, intent(in) :: x
            real :: y
        end function func
    end interface

end module abstract_interface_mod
'''
        )
        result = extract_module_data([Path('/fake/path/abstract_interface.f90')])
        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(len(module['interfaces']), 1)
        interface = module['interfaces'][0]        
        self.assertEqual(interface['description'], "\nAn abstract interface containing a function &lt;code&gt;func&lt;/code&gt;\n\n")
        self.assertEqual(interface['attributes'], ['abstract'])
        self.assertNotIn('operator_symbol', interface)
        self.assertEqual(len(interface['procedures']), 1)
        function = interface['procedures']['func']
        self.assertEqual(function['arguments'], ['x'])
        self.assertEqual(function['attributes'], [])
        self.assertEqual(function['binding_type'], { "type": BindingTypeEnum.DEFAULT, "name": None})
        self.assertEqual(function['description'], "Transforms x into y somehow\n\n")
        self.assertEqual(function['in'], {'x': {'type': 'real', 'description': '', 'dimension': ''}})
        self.assertEqual(function['out'], {})
        self.assertEqual(function['return'], {'y': {'type': 'real', 'description': '', 'dimension': ''}})


    def test_abstract_interface_with_nested_interface(self):
        self.fs.create_file(
            "/fake/path/nested_interface.f90",
            contents="""\
module test_mod
    implicit none

    !!* Interface for numerical integration functions *!
    abstract interface
        !!*
        ! @in f Function to integrate
        ! @in a Lower bound
        ! @in b Upper bound
        ! @return Integral value
        !*!
        function integrand(f, a, b) result(integral)
            implicit none
            !!* Defines the signature for the argument f *!
            interface
                !!* 
                ! Required signature for the function to be integrated
                ! @in x Point at which to evaluate the function
                ! @return Value of the function at x
                !*!            
                function f(x)
                    real, intent(in) :: x
                    real :: f
                end function f
            end interface
            real, intent(in) :: a, b
            real :: integral
        end function integrand
    end interface

end module test_mod
"""
        )
        result = extract_module_data([Path('/fake/path/nested_interface.f90')])
        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(len(module['interfaces']), 1)
        interface = module['interfaces'][0]

        # Check main interface properties
        self.assertEqual(interface['description'], "Interface for numerical integration functions\n")
        self.assertEqual(interface['attributes'], ['abstract'])
        self.assertNotIn('operator_symbol', interface)
        
        # Check the integrand function
        self.assertEqual(len(interface['procedures']), 1)
        function = interface['procedures']['integrand']
        self.assertEqual(function['arguments'], ['f', 'a', 'b'])
        self.assertEqual(function['attributes'], [])
        self.assertEqual(function['binding_type'], { "type": BindingTypeEnum.DEFAULT, "name": None})
        
        # Check input parameters
        self.assertEqual(function['in'], {
            'a': {'type': 'real', 'description': 'Lower bound', 'dimension': ''},
            'b': {'type': 'real', 'description': 'Upper bound', 'dimension': ''},
            'f': {'type': 'procedure', 'description': 'Function to integrate', 'dimension': '', 'interface_name': 'f'}
        })
        self.assertEqual(function['out'], {})
        self.assertEqual(function['return'], {
            'integral': {'type': 'real', 'description': 'Integral value', 'dimension': ''}
        })

        # Check the nested interface for f
        self.assertTrue('f' in function['argument_interfaces'])
        nested_interface = function['argument_interfaces']['f']
        self.assertEqual(nested_interface['description'], 'Defines the signature for the argument f\n')

        # Check the function within the nested interface
        self.assertEqual(len(nested_interface['procedures']), 1)
        nested_function = nested_interface['procedures']['f']
        self.assertEqual(nested_function['arguments'], ['x'])
        self.assertEqual(nested_function['in'], {
            'x': {'type': 'real', 'description': 'Point at which to evaluate the function', 'dimension': ''}
        })
        self.assertEqual(nested_function['out'], {})
        self.assertEqual(nested_function['return'], {
            'f': {'type': 'real', 'description': 'Value of the function at x', 'dimension': ''}
        })
        self.assertEqual(nested_interface['attributes'], [])  # not abstract
#         self.assertNotIn('operator_symbol', nested_interface)


    def test_modern_procedure_interface(self):
        self.fs.create_file(
            "/fake/path/modern_interface.f90",
            contents="""\
    module integration_mod
        implicit none

        !!* Interface for numerical functions to integrate *!
        abstract interface
            !!*
            ! @in x Point to evaluate
            ! @return Function value
            !*!
            function integrand(x) result(y)
                real, intent(in) :: x
                real :: y
            end function
        end interface

        !!*
        ! Integrates a function using Simpson's rule
        ! @in f Function to integrate
        ! @in a Lower bound
        ! @in b Upper bound
        ! @return Integral value
        !*!
        function simpson(f, a, b) result(integral)
            procedure(integrand) :: f
            real, intent(in) :: a, b
            real :: integral
        end function simpson

    end module integration_mod
    """
        )
        result = extract_module_data([Path('/fake/path/modern_interface.f90')])
        self.assertEqual(len(result), 1)
        module = result[0]
        
        # Check that both the interface and function exist
        self.assertEqual(len(module['interfaces']), 1)
        interface = module['interfaces'][0]
        
        # Check integrand interface
        self.assertEqual(interface['description'], "Interface for numerical functions to integrate\n")
        self.assertEqual(interface['attributes'], ['abstract'])
        self.assertEqual(len(interface['procedures']), 1)
        
        integrand = interface['procedures']['integrand']
        self.assertEqual(integrand['arguments'], ['x'])
        self.assertEqual(integrand['in'], {
            'x': {'type': 'real', 'description': 'Point to evaluate', 'dimension': ''}
        })
        self.assertEqual(integrand['return'], {
            'y': {'type': 'real', 'description': 'Function value', 'dimension': ''}
        })

        # Check simpson function and its use of the procedure interface
        simpson = module['functions']['simpson']
        self.assertEqual(simpson['arguments'], ['f', 'a', 'b'])
        self.assertEqual(simpson['in'], {
            'f': {'type': 'procedure', 'description': 'Function to integrate', 'dimension': '', 'interface_name': 'integrand'},
            'a': {'type': 'real', 'description': 'Lower bound', 'dimension': ''},
            'b': {'type': 'real', 'description': 'Upper bound', 'dimension': ''}
        })
        self.assertEqual(simpson['return'], {
            'integral': {'type': 'real', 'description': 'Integral value', 'dimension': ''}
        })
        
    def test_subroutine_interface(self):
        self.fs.create_file(
            '/fake/path/subroutine_interface.f90',
            contents='''\
    module callback_mod
        implicit none

        !!* Interface for data processing routines *!
        abstract interface
            !!*
            ! @in processor Callback subroutine to process data
            ! @inout data Input/output array to process
            !*!
            subroutine process_data(processor, data)
                implicit none
                !!* Required signature for the processing subroutine *!
                interface
                    !!*
                    ! @inout x Data array to modify
                    !*!
                    subroutine processor(x)
                        real, dimension(:), intent(inout) :: x
                    end subroutine processor
                end interface
                real, dimension(:), intent(inout) :: data
            end subroutine process_data
        end interface

    end module callback_mod
    '''
        )
        result = extract_module_data([Path('/fake/path/subroutine_interface.f90')])
        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(len(module['interfaces']), 1)
        interface = module['interfaces'][0]

        # Check main interface
        self.assertEqual(interface['description'], "Interface for data processing routines\n")
        self.assertEqual(interface['attributes'], ['abstract'])
        self.assertEqual(len(interface['procedures']), 1)
        
        # Check process_data subroutine
        process_data = interface['procedures']['process_data']
        self.assertEqual(process_data['arguments'], ['processor', 'data'])
        self.assertEqual(process_data['in'], {
            'processor': {'type': 'procedure', 'description': 'Callback subroutine to process data', 'dimension': '', 'interface_name': 'processor'},
            'data': {'type': 'real', 'description': 'Input/output array to process', 'dimension': ': (allocatable)'}
        })
        self.assertEqual(process_data['out'], {
            'data': {'type': 'real', 'description': 'Input/output array to process', 'dimension': ': (allocatable)'}
        })
        self.assertNotIn('return', process_data)  # It's a subroutine, so no return value

        # Check nested interface
        self.assertTrue('argument_interfaces' in process_data)
        self.assertEqual(len(process_data['argument_interfaces']), 1)
        
        processor_interface = process_data['argument_interfaces']['processor']
        self.assertEqual(processor_interface['description'], "Required signature for the processing subroutine\n")
        
        # Check the processor subroutine specification
        processor_proc = processor_interface['procedures']['processor']
        self.assertEqual(processor_proc['arguments'], ['x'])
        self.assertEqual(processor_proc['in'], {
            'x': {'type': 'real', 'description': 'Data array to modify', 'dimension': ': (allocatable)'}
        })
        self.assertEqual(processor_proc['out'], {
            'x': {'type': 'real', 'description': 'Data array to modify', 'dimension': ': (allocatable)'}
        })
        self.assertNotIn('return', processor_proc)

    def test_multiple_function_interfaces(self):
        self.fs.create_file(
            '/fake/path/multiple_interfaces.f90',
            contents='''\
    module optimization_mod
        implicit none

        !!* Interface for optimization routines
        !*!
        abstract interface
            !!*
            ! An optimization function
            ! @in f  Objective function to minimize
            ! @in df  Derivative of objective function
            ! @in constraint  Constraint function
            ! @in x0  Initial guess
            ! @return  Optimized value 
            !*!
            function optimize(f, df, constraint, x0) result(xmin)
                implicit none
                
                !!* Objective function interface *!
                interface
                    !!* 
                    ! @in x Point to evaluate
                    ! @return Function value
                    !*!
                    function f(x)
                        real, intent(in) :: x
                        real :: f
                    end function f
                end interface

                !!* Derivative function interface *!
                interface
                    !!* 
                    ! @in x Point to evaluate derivative
                    ! @return Derivative value
                    !*!
                    function df(x)
                        real, intent(in) :: x
                        real :: df
                    end function df
                end interface

                !!* Constraint function interface *!
                interface
                    !!* 
                    ! @in x Point to check
                    ! @return True if constraint is satisfied
                    !*!
                    function constraint(x)
                        real, intent(in) :: x
                        logical :: constraint
                    end function constraint
                end interface

                real, intent(in) :: x0
                real :: xmin
            end function optimize
        end interface

    end module optimization_mod
    '''
        )
        result = extract_module_data([Path('/fake/path/multiple_interfaces.f90')])
        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(len(module['interfaces']), 1)
        interface = module['interfaces'][0]

        # Check main interface
        self.assertEqual(interface['attributes'], ['abstract'])
        self.assertEqual(len(interface['procedures']), 1)
        
        # Check optimize function
        optimize = interface['procedures']['optimize']
        self.assertEqual(optimize['arguments'], ['f', 'df', 'constraint', 'x0'])
        self.assertEqual(optimize['in'], {
            'f': {'type': 'procedure', 'description': 'Objective function to minimize', 'dimension': '', 'interface_name': 'f'},
            'df': {'type': 'procedure', 'description': 'Derivative of objective function', 'dimension': '', 'interface_name': 'df'},
            'constraint': {'type': 'procedure', 'description': 'Constraint function', 'dimension': '', 'interface_name': 'constraint'},
            'x0': {'type': 'real', 'description': 'Initial guess', 'dimension': ''}
        })
        self.assertEqual(optimize['return'], {
            'xmin': {'type': 'real', 'description': 'Optimized value', 'dimension': ''}
        })

        # Check nested interfaces
        self.assertEqual(len(optimize['argument_interfaces']), 3)
        
        # Check objective function interface
        f_interface = optimize['argument_interfaces']['f']
        self.assertEqual(f_interface['description'], "Objective function interface\n")
        f_proc = f_interface['procedures']['f']
        self.assertEqual(f_proc['arguments'], ['x'])
        self.assertEqual(f_proc['in'], {
            'x': {'type': 'real', 'description': 'Point to evaluate', 'dimension': ''}
        })
        self.assertEqual(f_proc['return'], {
            'f': {'type': 'real', 'description': 'Function value', 'dimension': ''}
        })

        # Check derivative function interface
        df_interface = optimize['argument_interfaces']['df']
        self.assertEqual(df_interface['description'], "Derivative function interface\n")
        df_proc = df_interface['procedures']['df']
        self.assertEqual(df_proc['arguments'], ['x'])
        self.assertEqual(df_proc['in'], {
            'x': {'type': 'real', 'description': 'Point to evaluate derivative', 'dimension': ''}
        })
        self.assertEqual(df_proc['return'], {
            'df': {'type': 'real', 'description': 'Derivative value', 'dimension': ''}
        })

        # Check constraint function interface
        constraint_interface = optimize['argument_interfaces']['constraint']
        self.assertEqual(constraint_interface['description'], "Constraint function interface\n")
        constraint_proc = constraint_interface['procedures']['constraint']
        self.assertEqual(constraint_proc['arguments'], ['x'])
        self.assertEqual(constraint_proc['in'], {
            'x': {'type': 'real', 'description': 'Point to check', 'dimension': ''}
        })
        self.assertEqual(constraint_proc['return'], {
            'constraint': {'type': 'logical', 'description': 'True if constraint is satisfied', 'dimension': ''}
        })

    def test_interface_position_matching(self):
        self.fs.create_file(
            '/fake/path/position_matching.f90',
            contents='''\
    module optimization_mod
        implicit none

        !!* Interface for optimization routines *!
        abstract interface
            !!*
            ! @in objective Function to minimize
            ! @in gradient Gradient function
            ! @in x0 Initial guess
            ! @return Optimized value
            !*!
            function minimize(objective, gradient, x0) result(xmin)
                implicit none
                
                !!* Interface for objective function *!
                interface
                    !!*
                    ! @in x Evaluation point
                    ! @return Function value
                    !*!
                    function func1(x)  ! Different name from parameter
                        real, intent(in) :: x
                        real :: func1
                    end function func1
                end interface

                !!* Interface for gradient function *!
                interface
                    !!*
                    ! @in x Point to evaluate gradient
                    ! @return Gradient value
                    !*!
                    function grad(x)  ! Different name from parameter
                        real, intent(in) :: x
                        real :: grad
                    end function grad
                end interface

                real, intent(in) :: x0
                real :: xmin
            end function minimize
        end interface

    end module optimization_mod
    '''
        )
        result = extract_module_data([Path('/fake/path/position_matching.f90')])
        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(len(module['interfaces']), 1)
        interface = module['interfaces'][0]

        # Check main interface
        self.assertEqual(interface['description'], "Interface for optimization routines\n")
        self.assertEqual(interface['attributes'], ['abstract'])
        self.assertEqual(len(interface['procedures']), 1)
        
        # Check minimize function
        minimize = interface['procedures']['minimize']
        self.assertEqual(minimize['arguments'], ['objective', 'gradient', 'x0'])
        self.assertEqual(minimize['in'], {
            'objective': {'type': 'procedure', 'description': 'Function to minimize', 'dimension': '', 'interface_name': 'objective'},
            'gradient': {'type': 'procedure', 'description': 'Gradient function', 'dimension': '', 'interface_name': 'gradient'},
            'x0': {'type': 'real', 'description': 'Initial guess', 'dimension': ''}
        })
        self.assertEqual(minimize['return'], {
            'xmin': {'type': 'real', 'description': 'Optimized value', 'dimension': ''}
        })

        # Check nested interfaces - they should be associated with arguments by position
        self.assertEqual(len(minimize['argument_interfaces']), 2)
        
        # First interface should be associated with 'objective' parameter
        objective_interface = minimize['argument_interfaces']['objective']
        self.assertEqual(objective_interface['description'], "Interface for objective function\n")
        self.assertEqual(objective_interface['attributes'], [])
        self.assertNotIn('operator_symbol', objective_interface)
        
        objective_proc = objective_interface['procedures']['func1']  # Note different name
        self.assertEqual(objective_proc['arguments'], ['x'])
        self.assertEqual(objective_proc['in'], {
            'x': {'type': 'real', 'description': 'Evaluation point', 'dimension': ''}
        })
        self.assertEqual(objective_proc['return'], {
            'func1': {'type': 'real', 'description': 'Function value', 'dimension': ''}
        })

        # Second interface should be associated with 'gradient' parameter
        gradient_interface = minimize['argument_interfaces']['gradient']
        self.assertEqual(gradient_interface['description'], "Interface for gradient function\n")
        self.assertEqual(gradient_interface['attributes'], [])
        self.assertNotIn('operator_symbol', gradient_interface)
        
        gradient_proc = gradient_interface['procedures']['grad']  # Note different name
        self.assertEqual(gradient_proc['arguments'], ['x'])
        self.assertEqual(gradient_proc['in'], {
            'x': {'type': 'real', 'description': 'Point to evaluate gradient', 'dimension': ''}
        })
        self.assertEqual(gradient_proc['return'], {
            'grad': {'type': 'real', 'description': 'Gradient value', 'dimension': ''}
        })        


    def test_named_interface_explicit_procedure(self):
        self.fs.create_file(
            "/fake/path/vector_ops.f90",
            contents="""\
    module test_mod
        implicit none

        !!* 
        ! Interface for vector operations
        ! Contains functions for various vector manipulations
        !*!
        interface vector_ops
            !!*
            ! Normalizes a vector
            ! @in v Input vector
            ! @return Normalized vector
            !*!
            function normalize(v) result(normalized)
                real, dimension(:), intent(in) :: v
                real, dimension(size(v)) :: normalized
            end function normalize

            !!*
            ! Calculates the magnitude of a vector
            ! @in v Input vector
            ! @return Scalar magnitude
            !*!
            function magnitude(v) result(mag)
                real, dimension(:), intent(in) :: v
                real :: mag
            end function magnitude

            !!*
            ! Scales a vector by a factor
            ! @inout v Vector to scale
            ! @in factor Scale factor to apply
            !*!
            subroutine scale(v, factor)
                real, dimension(:), intent(inout) :: v
                real, intent(in) :: factor
            end subroutine scale
        end interface vector_ops

    contains
        function normalize(v) result(normalized)
            real, dimension(:), intent(in) :: v
            real, dimension(size(v)) :: normalized
            real :: mag
            mag = sqrt(sum(v**2))
            normalized = v / mag
        end function normalize

        function magnitude(v) result(mag)
            real, dimension(:), intent(in) :: v
            real :: mag
            mag = sqrt(sum(v**2))
        end function magnitude

        subroutine scale(v, factor)
            real, dimension(:), intent(inout) :: v
            real, intent(in) :: factor
            v = v * factor
        end subroutine scale
    end module test_mod
    """
        )
        result = extract_module_data([Path('/fake/path/vector_ops.f90')])
        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(len(module['interfaces']), 1)
        interface = module['interfaces'][0]

        # Check interface properties
        self.assertEqual(interface['description'], "\nInterface for vector operations\nContains functions for various vector manipulations\n\n")
        self.assertEqual(interface['attributes'], [])
        self.assertEqual(interface['name'], 'vector_ops')
        self.assertNotIn('operator_symbol', interface)
        
        # Check that we have three procedures
        self.assertEqual(len(interface['procedures']), 3)
        
        # Check normalize function
        normalize = interface['procedures']['normalize']
        self.assertEqual(normalize['arguments'], ['v'])
        self.assertEqual(normalize['description'], "\nNormalizes a vector\n\n")
        self.assertEqual(normalize['in'], {
            'v': {'type': 'real', 'description': 'Input vector', 'dimension': ': (allocatable)'}
        })
        self.assertEqual(normalize['out'], {})
        self.assertEqual(normalize['return'], {
            'normalized': {'type': 'real', 'description': 'Normalized vector', 'dimension': '1:size(v)'}
        })

        # Check magnitude function
        magnitude = interface['procedures']['magnitude']
        self.assertEqual(magnitude['arguments'], ['v'])
        self.assertEqual(magnitude['description'], "\nCalculates the magnitude of a vector\n\n")
        self.assertEqual(magnitude['in'], {
            'v': {'type': 'real', 'description': 'Input vector', 'dimension': ': (allocatable)'}
        })
        self.assertEqual(magnitude['out'], {})
        self.assertEqual(magnitude['return'], {
            'mag': {'type': 'real', 'description': 'Scalar magnitude', 'dimension': ''}
        })

        # Check scale subroutine
        scale = interface['procedures']['scale']
        self.assertEqual(scale['arguments'], ['v', 'factor'])
        self.assertEqual(scale['description'], "\nScales a vector by a factor\n\n")
        self.assertEqual(scale['in'], {
            'factor': {'type': 'real', 'description': 'Scale factor to apply', 'dimension': ''},
            'v': {'type': 'real', 'description': 'Vector to scale', 'dimension': ': (allocatable)'}
        })
        self.assertEqual(scale['out'], {
            'v': {'type': 'real', 'description': 'Vector to scale', 'dimension': ': (allocatable)'}
        })
        self.assertNotIn('return', scale)  # It's a subroutine, so no return value

    def test_named_interface_module_procedure(self):
        self.fs.create_file(
            '/fake/path/module_procedures.f90',
            contents='''\
module matrix_ops_mod
    implicit none

    !!* 
    ! Interface for matrix operations
    ! Provides operations for matrix manipulation
    !*!
    interface matrix_ops
        module procedure transpose_matrix, noop
        !!* Scales a matrix *!
        module procedure scale_matrix
        !!*
        ! Adds two matrices
        ! @in a First matrix
        ! @in b Second matrix
        ! @return Sum of matrices
        !*!
        function add_matrices(a, b) result(c)
            real, dimension(:,:), intent(in) :: a, b
            real, dimension(size(a,1),size(a,2)) :: c
        end function add_matrices
    end interface matrix_ops

contains
    !!* 
    ! Does nothing to a matrix
    ! @in mat Input matrix
    !*!
    subroutine noop(mat)
      real, dimension(:, :), intent(in) :: mat
    end subroutine noop

    !!*
    ! Transposes a matrix
    ! @in mat Input matrix
    ! @return Transposed matrix
    !*!
    function transpose_matrix(mat) result(trans)
        real, dimension(:,:), intent(in) :: mat
        real, dimension(size(mat,2),size(mat,1)) :: trans
        trans = transpose(mat)
    end function transpose_matrix

    !!*
    ! Scales a matrix by a factor
    ! @in mat Matrix to scale
    ! @in factor Scaling factor
    !*!
    subroutine scale_matrix(mat, factor)
        real, dimension(:,:), intent(inout) :: mat
        real, intent(in) :: factor
        mat = mat * factor
    end subroutine scale_matrix

    function add_matrices(a, b) result(c)
        real, dimension(:,:), intent(in) :: a, b
        real, dimension(size(a,1),size(a,2)) :: c
        c = a + b
    end function add_matrices
end module matrix_ops_mod
'''
    )
        result = extract_module_data([Path('/fake/path/module_procedures.f90')])
        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(len(module['interfaces']), 1)
        interface = module['interfaces'][0]

        self.assertEqual(interface['description'], "\nInterface for matrix operations\nProvides operations for matrix manipulation\n\n")
        self.assertEqual(interface['attributes'], [])
        self.assertEqual(interface['name'], 'matrix_ops')
        self.assertNotIn('operator_symbol', interface)
        
        self.assertEqual(len(interface['module_procedures']), 3)
        self.assertEqual(len(interface['procedures']), 1)

        noop = interface['module_procedures']['noop']
        self.assertEqual(noop['description'], "")

        transpose = interface['module_procedures']['transpose_matrix']
        self.assertEqual(transpose['description'], "")

        scale = interface['module_procedures']['scale_matrix']
        self.assertEqual(scale['description'], "Scales a matrix\n")

        add = interface['procedures']['add_matrices']
        self.assertEqual(add['arguments'], ['a', 'b'])
        self.assertEqual(add['description'], "\nAdds two matrices\n\n")
        self.assertEqual(add['in'], {
            'a': {'type': 'real', 'description': 'First matrix', 'dimension': ': (allocatable) &times; : (allocatable)'},
            'b': {'type': 'real', 'description': 'Second matrix', 'dimension': ': (allocatable) &times; : (allocatable)'}
        })
        self.assertEqual(add['out'], {})
        self.assertEqual(add['return'], {
            'c': {'type': 'real', 'description': 'Sum of matrices', 'dimension': '1:size(a,1) &times; 1:size(a,2)'}
        })

    def test_operator_interface_explicit_procedure(self):
        self.fs.create_file(
            "/fake/path/operator_interface.f90",
            contents="""\
    module test_mod
        implicit none

        !!* Interface for vector addition *!
        interface operator(+)
            !!*
            ! Adds two vectors of equal size
            ! @in a First vector
            ! @in b Second vector
            ! @return Sum of vectors
            !*!
            function add_vectors(a, b)
            real, dimension(:), intent(in) :: a, b
            real, dimension(size(a)) :: add_vectors
            end function add_vectors

            !!*
            ! Adds a scalar to each element of a vector
            ! @in a Input vector
            ! @in b Scalar value
            ! @return Scaled vector
            !*!
            function add_vector_scalar(a, b)
            real, dimension(:), intent(in) :: a
            real, intent(in) :: b
            real, dimension(size(a)):: add_vector_scalar
            end function
        end interface

    contains
        function add_vectors(a, b) result(c)
            real, intent(in) :: a(:), b(:)
            real, allocatable :: c(:)
            c = a + b
        end function add_vectors

        function add_vector_scalar(a, b) result(c)
            real, intent(in) :: a(:), b
            real, allocatable :: c(:)
            c = a + b
        end function add_vector_scalar
    end module test_mod
    """
        )
        result = extract_module_data([Path('/fake/path/operator_interface.f90')])
        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(len(module['interfaces']), 1)
        interface = module['interfaces'][0]

        # Check interface properties
        self.assertEqual(interface['description'], "Interface for vector addition\n")
        self.assertEqual(interface['attributes'], [])
        self.assertEqual(interface['operator_symbol'], '+')

        # Check that we have two procedures
        self.assertEqual(len(interface['procedures']), 2)

        # Check add_vectors function
        add_vectors = interface['procedures']['add_vectors']
        self.assertEqual(add_vectors['arguments'], ['a', 'b'])
        self.assertEqual(add_vectors['description'], "\nAdds two vectors of equal size\n\n")
        self.assertEqual(add_vectors['in'], {
            'a': {'type': 'real', 'description': 'First vector', 'dimension': ': (allocatable)'},
            'b': {'type': 'real', 'description': 'Second vector', 'dimension': ': (allocatable)'}
        })
        self.assertEqual(add_vectors['out'], {})
        self.assertEqual(add_vectors['return'], {
            'add_vectors': {'type': 'real', 'description': 'Sum of vectors', 'dimension': '1:size(a)'}
        })

        # Check add_vector_scalar function
        add_vector_scalar = interface['procedures']['add_vector_scalar']
        self.assertEqual(add_vector_scalar['arguments'], ['a', 'b'])
        self.assertEqual(add_vector_scalar['description'], "\nAdds a scalar to each element of a vector\n\n")
        self.assertEqual(add_vector_scalar['in'], {
            'a': {'type': 'real', 'description': 'Input vector', 'dimension': ': (allocatable)'},
            'b': {'type': 'real', 'description': 'Scalar value', 'dimension': ''}
        })
        self.assertEqual(add_vector_scalar['out'], {})
        self.assertEqual(add_vector_scalar['return'], {
            'add_vector_scalar': {'type': 'real', 'description': 'Scaled vector', 'dimension': '1:size(a)'}
        })

    def test_operator_interface_module_procedure(self):
        self.fs.create_file(
            "/fake/path/interfaces.f90",
            contents="""\
    module test_mod
        implicit none

        !!* 
        ! Interface for vector addition operations
        ! Provides overloaded + operator for vector-vector and vector-scalar addition
        !*!
        interface operator(+)
            !!*
            ! Adds two vectors of equal size
            !*!
            module procedure add_vectors

            !!*
            ! Adds a scalar to each element of a vector
            !*!
            module procedure add_vector_scalar
        end interface

    contains
        function add_vectors(a, b) result(c)
            real, intent(in) :: a(:), b(:)
            real, allocatable :: c(:)
            c = a + b
        end function add_vectors

        function add_vector_scalar(a, b) result(c)
            real, intent(in) :: a(:), b
            real, allocatable :: c(:)
            c = a + b
        end function add_vector_scalar
    end module test_mod
    """
            )
        result = extract_module_data([Path("/fake/path/interfaces.f90")])
        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(len(module['interfaces']), 1)
        
        # Check interface properties
        interface = module['interfaces'][0]
        self.assertEqual(
            interface['description'], 
            "\nInterface for vector addition operations\n"
            "Provides overloaded + operator for vector-vector and vector-scalar addition\n\n"
        )
        self.assertEqual(interface['attributes'], [])
        self.assertEqual(interface['operator_symbol'], '+')
        
        # Check module procedures are correctly recorded
        self.assertEqual(len(interface['module_procedures']), 2)
        
        # Check add_vectors module procedure
        add_vectors_proc = interface['module_procedures']['add_vectors']
        self.assertEqual(add_vectors_proc['name'], 'add_vectors')
        self.assertEqual(
            add_vectors_proc['description'],
            "\nAdds two vectors of equal size\n\n"
        )
        
        # Check add_vector_scalar module procedure
        add_scalar_proc = interface['module_procedures']['add_vector_scalar']
        self.assertEqual(add_scalar_proc['name'], 'add_vector_scalar')
        self.assertEqual(
            add_scalar_proc['description'],
            "\nAdds a scalar to each element of a vector\n\n"
        )
        
        # Check that procedures dictionary is empty (as these are module procedures)
        self.assertEqual(len(interface['procedures']), 0)

    def test_assignment_interface(self):
        self.fs.create_file(
            "/fake/path/interfaces.f90",
            contents="""\
    module test_mod
        implicit none

        !!*
        ! Custom assignment interface for vector types
        ! Allows direct assignment between allocatable and non-allocatable vectors
        !*!
        interface assignment(=)
            !!*
            ! Assigns values from one vector to another, handling allocation
            !*!
            module procedure assign_vector
        end interface

    contains
        subroutine assign_vector(a, b)
            real, allocatable, intent(out) :: a(:)
            real, intent(in) :: b(:)
            a = b
        end subroutine assign_vector
    end module test_mod
    """
            )
        result = extract_module_data([Path("/fake/path/interfaces.f90")])
        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(len(module['interfaces']), 1)
        
        # Check interface properties
        interface = module['interfaces'][0]
        self.assertEqual(
            interface['description'], 
            "\nCustom assignment interface for vector types\n"
            "Allows direct assignment between allocatable and non-allocatable vectors\n\n"
        )
        self.assertEqual(interface['attributes'], [])
        self.assertEqual(interface['operator_symbol'], '=')
        
        # Check module procedures are correctly recorded
        self.assertEqual(len(interface['module_procedures']), 1)
        
        # Check assign_vector module procedure
        assign_vector_mp = interface['module_procedures']['assign_vector']
        self.assertEqual(assign_vector_mp['name'], 'assign_vector')
        self.assertEqual(
            assign_vector_mp['description'],
            "\nAssigns values from one vector to another, handling allocation\n\n"
        )
        
        # No explicit procedures in this interface
        self.assertEqual(interface['procedures'], {})

    def test_comprehensive_interfaces(self):
        self.fs.create_file(
            "/fake/path/interfaces.f90",
            contents="""\
    module math_operations
        implicit none

        !!*
        ! Interface for numerical function evaluation
        ! Used in integration and other numerical methods
        !*!
        abstract interface
            !!*
            ! Function template for numerical operations
            ! @in x Point at which to evaluate function
            ! @return Function value at x
            !*!
            function func_interface(x)
                real, intent(in) :: x
                real :: func_interface
            end function func_interface
        end interface

        !!*
        ! Interface for dot product operations
        ! Supports both single and double precision vectors
        !*!
        interface operator(.dot.)
            !!*
            ! Computes dot product of two real vectors
            !*!
            module procedure dot_product_r

            !!*
            ! Computes dot product of two double precision vectors
            !*!
            module procedure dot_product_d
        end interface

        !!*
        ! Matrix assignment interface
        ! Handles allocation and assignment for matrices
        !*!
        interface assignment(=)
            !!*
            ! Assigns real matrix values with automatic allocation
            !*!
            module procedure assign_matrix_real

            !!*
            ! Assigns double precision matrix values with automatic allocation
            !*!
            module procedure assign_matrix_double
        end interface

        !!*
        ! Generic norm calculation interface
        ! Computes Euclidean norm for vectors and Frobenius norm for matrices
        !*!
        interface norm
            !!*
            ! Computes the Euclidean norm of a vector
            !*!
            module procedure vector_norm

            !!*
            ! Computes the Frobenius norm of a matrix
            !*!
            module procedure matrix_norm
        end interface

    contains
        real function dot_product_r(a, b)
            real, intent(in) :: a(:), b(:)
            dot_product_r = sum(a * b)
        end function dot_product_r

        double precision function dot_product_d(a, b)
            double precision, intent(in) :: a(:), b(:)
            dot_product_d = sum(a * b)
        end function dot_product_d

        subroutine assign_matrix_real(a, b)
            real, allocatable, intent(out) :: a(:,:)
            real, intent(in) :: b(:,:)
            a = b
        end subroutine assign_matrix_real

        subroutine assign_matrix_double(a, b)
            double precision, allocatable, intent(out) :: a(:,:)
            double precision, intent(in) :: b(:,:)
            a = b
        end subroutine assign_matrix_double

        function vector_norm(v) result(n)
            real, intent(in) :: v(:)
            real :: n
            n = sqrt(sum(v**2))
        end function vector_norm

        function matrix_norm(m) result(n)
            real, intent(in) :: m(:,:)
            real :: n
            n = sqrt(sum(m**2))
        end function matrix_norm

    end module math_operations
    """
            )
        result = extract_module_data([Path("/fake/path/interfaces.f90")])
        self.assertEqual(len(result), 1)
        module = result[0]
        
        # Check all interfaces exist
        self.assertEqual(len(module['interfaces']), 4)  # abstract, dot, assignment, and norm interfaces
        
        # Check abstract interface
        func_interface = module['interfaces'][0]
        self.assertEqual(
            func_interface['description'], 
            "\nInterface for numerical function evaluation\n"
            "Used in integration and other numerical methods\n\n"
        )
        self.assertIn('abstract', func_interface['attributes'])
        
        # Check explicit procedure in abstract interface
        proc = func_interface['procedures']['func_interface']
        self.assertEqual(proc['arguments'], ['x'])
        self.assertEqual(proc['in'], {
            'x': {'type': 'real', 'description': 'Point at which to evaluate function', 'dimension': ''}
        })
        self.assertEqual(proc['return'], {
            'func_interface': {'type': 'real', 'description': 'Function value at x', 'dimension': ''}
        })
        self.assertEqual(func_interface['module_procedures'], {})  # No module procedures
        
        # Check dot product operator interface
        dot_interface = module['interfaces'][1]
        self.assertEqual(dot_interface['operator_symbol'], '.dot.')
        self.assertEqual(len(dot_interface['module_procedures']), 2)
        self.assertEqual(dot_interface['procedures'], {})  # No explicit procedures
        
        dot_r = dot_interface['module_procedures']['dot_product_r']
        self.assertEqual(dot_r['name'], 'dot_product_r')
        self.assertEqual(dot_r['description'], "\nComputes dot product of two real vectors\n\n")
        
        dot_d = dot_interface['module_procedures']['dot_product_d']
        self.assertEqual(dot_d['name'], 'dot_product_d')
        self.assertEqual(dot_d['description'], "\nComputes dot product of two double precision vectors\n\n")
        
        # Check assignment interface
        assign_interface = module['interfaces'][2]
        self.assertEqual(assign_interface['operator_symbol'], '=')
        self.assertEqual(len(assign_interface['module_procedures']), 2)
        self.assertEqual(assign_interface['procedures'], {})  # No explicit procedures
        
        assign_r = assign_interface['module_procedures']['assign_matrix_real']
        self.assertEqual(assign_r['name'], 'assign_matrix_real')
        self.assertEqual(assign_r['description'], "\nAssigns real matrix values with automatic allocation\n\n")
        
        assign_d = assign_interface['module_procedures']['assign_matrix_double']
        self.assertEqual(assign_d['name'], 'assign_matrix_double')
        self.assertEqual(assign_d['description'], "\nAssigns double precision matrix values with automatic allocation\n\n")
        
        # Check norm interface
        norm_interface = module['interfaces'][3]
        self.assertEqual(len(norm_interface['module_procedures']), 2)
        self.assertEqual(norm_interface['procedures'], {})  # No explicit procedures
        
        vector_n = norm_interface['module_procedures']['vector_norm']
        self.assertEqual(vector_n['name'], 'vector_norm')
        self.assertEqual(vector_n['description'], "\nComputes the Euclidean norm of a vector\n\n")
        
        matrix_n = norm_interface['module_procedures']['matrix_norm']
        self.assertEqual(matrix_n['name'], 'matrix_norm')
        self.assertEqual(matrix_n['description'], "\nComputes the Frobenius norm of a matrix\n\n")    


if __name__ == '__main__':
    unittest.main()


# module complex_interfaces
#     implicit none
#     private

#     public :: general_math, vector_ops

#     interface general_math
#         module procedure int_func, real_func
#         procedure double_func
#     end interface

#     interface vector_ops
#         pure function dot_product(a, b) result(c)
#             real, dimension(:), intent(in) :: a, b
#             real :: c
#         end function dot_product

#         elemental subroutine scale(v, factor)
#             real, intent(inout) :: v
#             real, intent(in) :: factor
#         end subroutine scale

#         function cross_product(a, b, c) result(d)
#             real, dimension(3), intent(in) :: a, b
#             real, dimension(3), intent(out), optional :: c
#             real, dimension(3) :: d
#         end function cross_product
#     end interface

#     abstract interface
#         function func(x, callback) result(y)
#             import
#             real, intent(in) :: x
#             procedure(real_func) :: callback
#             real :: y
#         end function func
#     end interface

#     interface
#         function external_func(x) bind(C, name="c_func")
#             use, intrinsic :: iso_c_binding
#             real(c_float), value :: x
#             real(c_float) :: external_func
#         end function external_func
#     end interface

# contains
#     function int_func(x)
#         integer, intent(in) :: x
#         integer :: int_func
#         int_func = x * 2
#     end function int_func

#     function real_func(x)
#         real, intent(in) :: x
#         real :: real_func
#         real_func = x * 2.0
#     end function real_func
# end module complex_interfaces    


# module complex_interfaces
#     implicit none
#     private
#     public :: vector_ops, operator(+)

#     !!* Private interface for internal use *!
#     private interface internal_ops
#         module procedure internal_func
#     end interface

#     !!* Public interface with various procedure types *!
#     public interface vector_ops
#         pure function pure_op(x)
#             real, intent(in) :: x
#             real :: pure_op
#         end function

#         elemental function elem_op(x)
#             real, intent(in) :: x
#             real :: elem_op
#         end function

#         recursive function rec_op(x)
#             real, intent(in) :: x
#             real :: rec_op
#         end function

#         function array_op(x)
#             real, dimension(:), intent(in) :: x
#             real, dimension(size(x)) :: array_op
#         end function

#         function procedure_arg(func, x)
#             interface
#                 function func(x)
#                     real, intent(in) :: x
#                     real :: func
#                 end function
#             end interface
#             real, intent(in) :: x
#             real :: procedure_arg
#         end function
#     end interface

#     !!* Interface for derived type IO *!
#     interface write(formatted)
#         module procedure write_mytype
#     end interface

# contains
#     ! Implementation procedures...
# end module complex_interfaces