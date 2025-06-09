import unittest
from pathlib import Path
from pyfakefs.fake_filesystem_unittest import TestCase
from doc4for.f90.generate_module_tree import extract_module_data
from doc4for.models.dimension_models import ArrayBound, BoundType, Expression
from doc4for.models.common import ExpressionType
from doc4for.models.variable_models import PolymorphismType

class TestInterfaces(TestCase):
    maxDiff=None

    def setUp(self):
        self.setUpPyfakefs()

    def test_abstract_interface(self):
        self.fs.create_file(
            "/fake/path/abstract_interface.f90",
            contents="""\
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
 """
        )
        result = extract_module_data([Path("/fake/path/abstract_interface.f90")])
        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(len(module["interfaces"]), 1)
        interface = module["interfaces"][0]        
        self.assertEqual(interface["description"], "An abstract interface containing a function &lt;code&gt;func&lt;/code&gt;\n")
        self.assertEqual(interface["attributes"], ["ABSTRACT"])
        self.assertIsNone(interface["operator_symbol"])
        self.assertEqual(len(interface["procedures"]), 1)
        function = interface["procedures"]["func"]
        self.assertEqual(function["arguments"], ["x"])
        self.assertEqual(function["attributes"], [])
        self.assertIsNone(function["binding_type"])
        self.assertEqual(function["description"], "Transforms x into y somehow\n")
        self.assertEqual(function["in"], {"x": {"type": 
                                                "REAL", 
                                                "description": "", 
                                                "dimension": None, 
                                                "interface_name": None, 
                                                "enum_type": None,
                                                "attributes": [],
                                                "kind": None,
                                                "length": None,
                                                "default_value": None,
                                                "polymorphism_type": PolymorphismType.NONE
                                                }})
        self.assertEqual(function["out"], {})
        self.assertEqual(function["return"], {"type": "REAL", 
                                              "description": "", 
                                              "dimension": None, 
                                              "interface_name": None, 
                                              "enum_type": None,
                                                "attributes": [],
                                                "kind": None,
                                                "length": None,
                                                "default_value": None,
                                                "polymorphism_type": PolymorphismType.NONE
                                              })

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
        result = extract_module_data([Path("/fake/path/nested_interface.f90")])
        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(len(module["interfaces"]), 1)
        interface = module["interfaces"][0]

        # Check main interface properties
        self.assertEqual(interface["description"], "Interface for numerical integration functions\n")
        self.assertEqual(interface["attributes"], ["ABSTRACT"])
        self.assertIsNone(interface["operator_symbol"])
        # Check the integrand function
        self.assertEqual(len(interface["procedures"]), 1)
        function = interface["procedures"]["integrand"]
        self.assertEqual(function["arguments"], ["f", "a", "b"])
        self.assertEqual(function["attributes"], [])
        self.assertIsNone(function["binding_type"])
        
        # Check input parameters
        self.assertEqual(function["in"], {
            "a": {"type": "REAL",
                   "description": "Lower bound", 
                   "dimension": None,
                   "interface_name": None,
                   "enum_type": None,
                    "attributes": [],
                    "kind": None,
                    "length": None,
                    "default_value": None,
                    "polymorphism_type": PolymorphismType.NONE
                   },
            "b": {"type": "REAL", 
                  "description": "Upper bound", 
                  "dimension": None,
                  "interface_name": None,
                  "enum_type": None,
                  "attributes": [],
                  "kind": None,
                  "length": None,
                  "default_value": None,
                  "polymorphism_type": PolymorphismType.NONE
                  },
            "f": {"type": "PROCEDURE", 
                  "description": "Function to integrate", 
                  "dimension": None, 
                  "interface_name": "f",
                  "enum_type": None,
                  "attributes": [],
                  "kind": None,
                  "length": None,
                  "default_value": None,
                  "polymorphism_type": PolymorphismType.NONE
                  }
        })
        self.assertEqual(function["out"], {})
        self.assertEqual(function["return"], 
            {"type": "REAL", 
             "description": "Integral value", 
             "dimension": None,
             "interface_name": None,
             "enum_type": None,
            "attributes": [],
            "kind": None,
            "length": None,
            "default_value": None,
            "polymorphism_type": PolymorphismType.NONE
        })

        # Check the nested interface for f
        self.assertTrue("f" in function["argument_interfaces"])
        nested_interface = function["argument_interfaces"]["f"]
        self.assertEqual(nested_interface["description"], "Defines the signature for the argument f\n")

        # Check the function within the nested interface
        self.assertEqual(len(nested_interface["procedures"]), 1)
        nested_function = nested_interface["procedures"]["f"]
        self.assertEqual(nested_function["description"], "Required signature for the function to be integrated\n\n")
        self.assertEqual(nested_function["arguments"], ["x"])
        self.assertEqual(nested_function["in"], {
            "x": {"type": "REAL", 
                  "description": "Point at which to evaluate the function", 
                  "dimension": None,
                  "interface_name": None,
                  "enum_type": None,
                  "attributes": [],
                  "kind": None,
                  "length": None,
                  "default_value": None,
                  "polymorphism_type": PolymorphismType.NONE
                  }
        })
        self.assertEqual(nested_function["out"], {})
        self.assertEqual(nested_function["return"], {
            "type": "REAL", 
            "description": "Value of the function at x", 
            "dimension": None,
            "interface_name": None,
            "enum_type": None,
            "attributes": [],
            "kind": None,
            "length": None,
            "default_value": None,
            "polymorphism_type": PolymorphismType.NONE
            }
        )
        self.assertEqual(nested_interface["attributes"], [])  # not abstract
        self.assertIsNone(nested_interface["operator_symbol"])
        self.assertEqual(nested_interface["name"], "")
        self.assertEqual(nested_interface["module_procedures"], {})

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

        contains
        !!*
        ! Integrates a function using Simpson"s rule
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
        result = extract_module_data([Path("/fake/path/modern_interface.f90")])
        self.assertEqual(len(result), 1)
        module = result[0]
        
        # Check that both the interface and function exist
        self.assertEqual(len(module["interfaces"]), 1)
        interface = module["interfaces"][0]
        
        # Check integrand interface
        self.assertEqual(interface["description"], "Interface for numerical functions to integrate\n")
        self.assertEqual(interface["attributes"], ["ABSTRACT"])
        self.assertEqual(len(interface["procedures"]), 1)
        
        integrand = interface["procedures"]["integrand"]
        self.assertEqual(integrand["arguments"], ["x"])
        self.assertEqual(integrand["in"], {
            "x": {"type": "REAL", 
                  "description": "Point to evaluate", 
                  "dimension": None,
                  "interface_name": None,
                  "enum_type": None,
                  "attributes": [],
                  "kind": None,
                  "length": None,
                  "default_value": None,
                  "polymorphism_type": PolymorphismType.NONE
                  }
        })
        self.assertEqual(integrand["return"], 
            {"type": "REAL", 
                  "description": "Function value", 
                  "dimension": None,
                  "interface_name": None,
                  "enum_type": None,
                  "attributes": [],
                  "kind": None,
                  "length": None,
                  "default_value": None,
                  "polymorphism_type": PolymorphismType.NONE
        })

        # Check Simpson function and its use of the procedure interface
        simpson = module["functions"]["simpson"]
        self.assertEqual(simpson["arguments"], ["f", "a", "b"])
        self.assertEqual(simpson["in"], {
            "f": {"type": "PROCEDURE", 
                  "description": "Function to integrate", 
                  "dimension": None, 
                  "interface_name": "integrand",
                  "enum_type": None,
                  "attributes": [],
                  "kind": None,
                  "length": None,
                  "default_value": None,
                  "polymorphism_type": PolymorphismType.NONE
                  },
            "a": {"type": "REAL", 
                  "description": "Lower bound", 
                  "dimension": None,
                  "interface_name": None,
                  "enum_type": None,
                  "attributes": [],
                  "kind": None,
                  "length": None,
                  "default_value": None,
                  "polymorphism_type": PolymorphismType.NONE
                  },
            "b": {"type": "REAL", 
                  "description": "Upper bound", 
                  "dimension": None,
                  "interface_name": None,
                  "enum_type": None,
                  "attributes": [],
                  "kind": None,
                  "length": None,
                  "default_value": None,
                  "polymorphism_type": PolymorphismType.NONE
                  }
        })
        self.assertEqual(simpson["return"], {
            "type": "REAL", 
            "description": "Integral value", 
            "dimension": None,
            "interface_name": None,
            "enum_type": None,
            "attributes": [],
            "kind": None,
            "length": None,
            "default_value": None,
            "polymorphism_type": PolymorphismType.NONE
        })
        
    def test_subroutine_interface(self):
        self.fs.create_file(
            "/fake/path/subroutine_interface.f90",
            contents="""\
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
    """
        )
        result = extract_module_data([Path("/fake/path/subroutine_interface.f90")])
        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(len(module["interfaces"]), 1)
        interface = module["interfaces"][0]

        # Check main interface
        self.assertEqual(interface["description"], "Interface for data processing routines\n")
        self.assertEqual(interface["attributes"], ["ABSTRACT"])
        self.assertEqual(len(interface["procedures"]), 1)
        
        # Check process_data subroutine
        process_data = interface["procedures"]["process_data"]
        self.assertEqual(process_data["arguments"], ["processor", "data"])
        self.assertEqual(process_data["in"], {
            "processor": {"type": "PROCEDURE", 
                          "description": "Callback subroutine to process data", 
                          "dimension": None, 
                          "interface_name": "processor",
                          "enum_type": None,
                          "attributes": [],
                          "kind": None,
                          "length": None,
                          "default_value": None,
                          "polymorphism_type": PolymorphismType.NONE},
            "data": {"type": "REAL", 
                     "description": "Input/output array to process", 
                     "dimension": {"dimensions": [ArrayBound(BoundType.ASSUMED_SHAPE)]},
                     "interface_name": None,
                     "enum_type": None,
                     "attributes": [],
                     "kind": None,
                     "length": None,
                     "default_value": None,
                     "polymorphism_type": PolymorphismType.NONE}
        })
        self.assertEqual(process_data["out"], {
            "data": {"type": "REAL", 
                     "description": "Input/output array to process", 
                     "dimension": {"dimensions": [ArrayBound(BoundType.ASSUMED_SHAPE)]},
                     "interface_name": None,
                     "enum_type": None,
                    "attributes": [],
                    "kind": None,
                    "length": None,
                    "default_value": None,
                    "polymorphism_type": PolymorphismType.NONE}
        })
        self.assertNotIn("return", process_data)  # It"s a subroutine, so no return value

        # Check nested interface
        self.assertTrue("argument_interfaces" in process_data)
        self.assertEqual(len(process_data["argument_interfaces"]), 1)
        
        processor_interface = process_data["argument_interfaces"]["processor"]
        self.assertEqual(processor_interface["description"], "Required signature for the processing subroutine\n")
        
        # Check the processor subroutine specification
        processor_proc = processor_interface["procedures"]["processor"]
        self.assertEqual(processor_proc["arguments"], ["x"])
        self.assertEqual(processor_proc["in"], {
            "x": {"type": "REAL", 
                  "description": "Data array to modify", 
                  "dimension": {"dimensions": [ArrayBound(BoundType.ASSUMED_SHAPE)]},
                  "interface_name": None,
                  "enum_type": None,
                  "attributes": [],
                  "kind": None,
                  "length": None,
                  "default_value": None,
                  "polymorphism_type": PolymorphismType.NONE
            }  
        })
        self.assertEqual(processor_proc["out"], {
            "x": {"type": "REAL", 
                  "description": "Data array to modify", 
                  "dimension": {"dimensions": [ArrayBound(BoundType.ASSUMED_SHAPE)]},
                  "interface_name": None,
                  "enum_type": None,
                  "attributes": [],
                  "kind": None,
                  "length": None,
                  "default_value": None,
                  "polymorphism_type": PolymorphismType.NONE
                  }
        })
        self.assertNotIn("return", processor_proc)

    def test_multiple_function_interfaces(self):
        self.fs.create_file(
            "/fake/path/multiple_interfaces.f90",
            contents="""\
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
    """
        )
        result = extract_module_data([Path("/fake/path/multiple_interfaces.f90")])
        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(len(module["interfaces"]), 1)
        interface = module["interfaces"][0]

        # Check main interface
        self.assertEqual(interface["attributes"], ["ABSTRACT"])
        self.assertEqual(len(interface["procedures"]), 1)
        
        # Check optimize function
        optimize = interface["procedures"]["optimize"]
        self.assertEqual(optimize["arguments"], ["f", "df", "constraint", "x0"])
        self.assertEqual(optimize["in"], {
            "f": {"type": "PROCEDURE", 
                  "description": "Objective function to minimize", 
                  "dimension": None, 
                  "interface_name": "f", 
                  "enum_type": None,
                  "attributes": [],
                  "kind": None,
                  "length": None,
                  "default_value": None,
                  "polymorphism_type": PolymorphismType.NONE
                  },
            "df": {"type": "PROCEDURE", 
                   "description": "Derivative of objective function", 
                   "dimension": None, 
                   "interface_name": "df", 
                   "enum_type": None,
                  "attributes": [],
                  "kind": None,
                  "length": None,
                  "default_value": None,
                  "polymorphism_type": PolymorphismType.NONE
                   },
            "constraint": {"type": "PROCEDURE", 
                           "description": "Constraint function", 
                           "dimension": None, 
                           "interface_name": "constraint", 
                           "enum_type": None,
                  "attributes": [],
                  "kind": None,
                  "length": None,
                  "default_value": None,
                  "polymorphism_type": PolymorphismType.NONE
                           },
            "x0": {"type": "REAL", 
                   "description": "Initial guess", 
                   "dimension": None, 
                   "interface_name": None, 
                   "enum_type": None,
                  "attributes": [],
                  "kind": None,
                  "length": None,
                  "default_value": None,
                  "polymorphism_type": PolymorphismType.NONE
                   }
        })
        self.assertEqual(optimize["return"], {
            "type": "REAL", 
            "description": "Optimized value", 
            "dimension": None, 
            "interface_name": None, 
            "enum_type": None,
            "attributes": [],
            "kind": None,
            "length": None,
            "default_value": None,
            "polymorphism_type": PolymorphismType.NONE
        })

        # Check nested interfaces
        self.assertEqual(len(optimize["argument_interfaces"]), 3)
        
        # Check objective function interface
        f_interface = optimize["argument_interfaces"]["f"]
        self.assertEqual(f_interface["description"], "Objective function interface\n")
        f_proc = f_interface["procedures"]["f"]
        self.assertEqual(f_proc["arguments"], ["x"])
        self.assertEqual(f_proc["in"], {
            "x": {"type": "REAL", 
                  "description": "Point to evaluate", 
                  "dimension": None, 
                  "interface_name": None, 
                  "enum_type": None,
                  "attributes": [],
                  "kind": None,
                  "length": None,
                  "default_value": None,
                  "polymorphism_type": PolymorphismType.NONE
                  }
        })
        self.assertEqual(f_proc["return"], {
            "type": "REAL", 
            "description": "Function value", 
            "dimension": None, 
            "interface_name": None, 
            "enum_type": None,
            "attributes": [],
            "kind": None,
            "length": None,
            "default_value": None,
            "polymorphism_type": PolymorphismType.NONE
        })

        # Check derivative function interface
        df_interface = optimize["argument_interfaces"]["df"]
        self.assertEqual(df_interface["description"], "Derivative function interface\n")
        df_proc = df_interface["procedures"]["df"]
        self.assertEqual(df_proc["arguments"], ["x"])
        self.assertEqual(df_proc["in"], {
            "x": {"type": "REAL", 
                  "description": "Point to evaluate derivative", 
                  "dimension": None, 
                  "interface_name": None, 
                  "enum_type": None,
                  "attributes": [],
                  "kind": None,
                  "length": None,
                  "default_value": None,
                  "polymorphism_type": PolymorphismType.NONE
                  }
        })
        self.assertEqual(df_proc["return"], {
            "type": "REAL", 
            "description": "Derivative value", 
            "dimension": None, 
            "interface_name": None, 
            "enum_type": None,
            "attributes": [],
            "kind": None,
            "length": None,
            "default_value": None,
            "polymorphism_type": PolymorphismType.NONE
        })

        # Check constraint function interface
        constraint_interface = optimize["argument_interfaces"]["constraint"]
        self.assertEqual(constraint_interface["description"], "Constraint function interface\n")
        constraint_proc = constraint_interface["procedures"]["constraint"]
        self.assertEqual(constraint_proc["arguments"], ["x"])
        self.assertEqual(constraint_proc["in"], {
            "x": {"type": "REAL", 
                  "description": "Point to check", 
                  "dimension": None, 
                  "interface_name": None, 
                  "enum_type": None,
                  "attributes": [],
                  "kind": None,
                  "length": None,
                  "default_value": None,
                  "polymorphism_type": PolymorphismType.NONE
                  }
        })
        self.assertEqual(constraint_proc["return"], {
            "type": "LOGICAL", 
            "description": "True if constraint is satisfied", 
            "dimension": None, 
            "interface_name": None, 
            "enum_type": None,
            "attributes": [],
            "kind": None,
            "length": None,
            "default_value": None,
            "polymorphism_type": PolymorphismType.NONE
        })

    def test_interface_position_matching(self):
        self.fs.create_file(
            "/fake/path/position_matching.f90",
            contents="""\
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
    """
        )
        result = extract_module_data([Path("/fake/path/position_matching.f90")])
        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(len(module["interfaces"]), 1)
        interface = module["interfaces"][0]

        # Check main interface
        self.assertEqual(interface["description"], "Interface for optimization routines\n")
        self.assertEqual(interface["attributes"], ["ABSTRACT"])
        self.assertEqual(len(interface["procedures"]), 1)
        
        # Check minimize function
        minimize = interface["procedures"]["minimize"]
        self.assertEqual(minimize["arguments"], ["objective", "gradient", "x0"])
        self.assertEqual(minimize["in"], {
            "objective": {"type": "PROCEDURE", 
                          "description": "Function to minimize", 
                          "dimension": None, 
                          "interface_name": "func1", 
                          "enum_type": None,
                            "attributes": [],
                            "kind": None,
                            "length": None,
                            "default_value": None,
                            "polymorphism_type": PolymorphismType.NONE
                          },  
            "gradient": {"type": "PROCEDURE", 
                         "description": "Gradient function", 
                         "dimension": None, 
                         "interface_name": "grad", 
                         "enum_type": None,
                        "attributes": [],
                        "kind": None,
                        "length": None,
                        "default_value": None,
                        "polymorphism_type": PolymorphismType.NONE
                         },  
            "x0": {"type": "REAL", 
                   "description": "Initial guess", 
                   "dimension": None, 
                   "interface_name": None, 
                   "enum_type": None,
                    "attributes": [],
                    "kind": None,
                    "length": None,
                    "default_value": None,
                    "polymorphism_type": PolymorphismType.NONE
                   }
        })
        self.assertEqual(minimize["return"], {
            "type": "REAL", 
            "description": "Optimized value", 
            "dimension": None, 
            "interface_name": None, 
            "enum_type": None,
            "attributes": [],
            "kind": None,
            "length": None,
            "default_value": None,
            "polymorphism_type": PolymorphismType.NONE
        })

        # Check nested interfaces - they should be associated with arguments by position
        self.assertEqual(len(minimize["argument_interfaces"]), 2)
        
        # First interface should be associated with "objective" parameter
        objective_interface = minimize["argument_interfaces"]["objective"]
        self.assertEqual(objective_interface["description"], "Interface for objective function\n")
        self.assertEqual(objective_interface["attributes"], [])
        self.assertIsNone(objective_interface["operator_symbol"])
        
        objective_proc = objective_interface["procedures"]["func1"]  # Note different name
        self.assertEqual(objective_proc["arguments"], ["x"])
        self.assertEqual(objective_proc["in"], {
            "x": {"type": "REAL", 
                  "description": "Evaluation point", 
                  "dimension": None, 
                  "interface_name": None, 
                  "enum_type": None,
                    "attributes": [],
                    "kind": None,
                    "length": None,
                    "default_value": None,
                    "polymorphism_type": PolymorphismType.NONE
                  }
        })
        self.assertEqual(objective_proc["return"], {
            "type": "REAL", 
            "description": "Function value", 
            "dimension": None, 
            "interface_name": None, 
            "enum_type": None,
            "attributes": [],
            "kind": None,
            "length": None,
            "default_value": None,
            "polymorphism_type": PolymorphismType.NONE
        })

        # Second interface should be associated with "gradient" parameter
        gradient_interface = minimize["argument_interfaces"]["gradient"]
        self.assertEqual(gradient_interface["description"], "Interface for gradient function\n")
        self.assertEqual(gradient_interface["attributes"], [])
        self.assertIsNone(gradient_interface["operator_symbol"])
        
        gradient_proc = gradient_interface["procedures"]["grad"]  # Note different name
        self.assertEqual(gradient_proc["arguments"], ["x"])
        self.assertEqual(gradient_proc["in"], {
            "x": {"type": "REAL", 
                  "description": "Point to evaluate gradient", 
                  "dimension": None, 
                  "interface_name": None, 
                  "enum_type": None,
                "attributes": [],
                "kind": None,
                "length": None,
                "default_value": None,
                "polymorphism_type": PolymorphismType.NONE
                  }
        })
        self.assertEqual(gradient_proc["return"], {
            "type": "REAL", 
            "description": "Gradient value", 
            "dimension": None, 
            "interface_name": None, 
            "enum_type": None,
            "attributes": [],
            "kind": None,
            "length": None,
            "default_value": None,
            "polymorphism_type": PolymorphismType.NONE
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
        result = extract_module_data([Path("/fake/path/vector_ops.f90")])
        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(len(module["interfaces"]), 1)
        interface = module["interfaces"][0]

        # Check interface properties
        self.assertEqual(interface["description"], "Interface for vector operations\nContains functions for various vector manipulations\n")
        self.assertEqual(interface["attributes"], [])
        self.assertEqual(interface["name"], "vector_ops")
        self.assertIsNone(interface["operator_symbol"])
        
        # Check that we have three procedures
        self.assertEqual(len(interface["procedures"]), 3)
        
        # Check normalize function
        normalize = interface["procedures"]["normalize"]
        self.assertEqual(normalize["arguments"], ["v"])
        self.assertEqual(normalize["description"], "Normalizes a vector\n\n")
        self.assertEqual(normalize["in"], {
            "v": {"type": "REAL", 
                  "description": "Input vector", 
                  "dimension": {"dimensions": [ArrayBound(BoundType.ASSUMED_SHAPE)]},
                  "interface_name": None,
                  "enum_type": None,
                  "attributes": [],
                  "kind": None,
                  "length": None,
                  "default_value": None,
                  "polymorphism_type": PolymorphismType.NONE
                  }
        })
        self.assertEqual(normalize["out"], {})
        self.assertEqual(normalize["return"], {
            "type": "REAL", 
            "description": "Normalized vector", 
            "dimension": {"dimensions": [ArrayBound(BoundType.VARIABLE, 
                                                    Expression(expr_type=ExpressionType.LITERAL, value="1"),
                                                    Expression(expr_type=ExpressionType.LITERAL, value="SIZE(v)"))]},
            "interface_name": None,
            "enum_type": None,
                  "attributes": [],
                  "kind": None,
                  "length": None,
                  "default_value": None,
                  "polymorphism_type": PolymorphismType.NONE
        })

        # Check magnitude function
        magnitude = interface["procedures"]["magnitude"]
        self.assertEqual(magnitude["arguments"], ["v"])
        self.assertEqual(magnitude["description"], "Calculates the magnitude of a vector\n\n")
        self.assertEqual(magnitude["in"], {
            "v": {"type": "REAL", 
                  "description": "Input vector", 
                  "dimension": {"dimensions": [ArrayBound(BoundType.ASSUMED_SHAPE)]},
                  "interface_name": None,
                  "enum_type": None,
                  "attributes": [],
                  "kind": None,
                  "length": None,
                  "default_value": None,
                  "polymorphism_type": PolymorphismType.NONE
                  }
        })
        self.assertEqual(magnitude["out"], {})
        self.assertEqual(magnitude["return"], {
            "type": "REAL", 
            "description": "Scalar magnitude", 
            "dimension": None,
            "interface_name": None,
            "enum_type": None,
                "attributes": [],
                "kind": None,
                "length": None,
                "default_value": None,
                "polymorphism_type": PolymorphismType.NONE
        })

        # Check scale subroutine
        scale = interface["procedures"]["scale"]
        self.assertEqual(scale["arguments"], ["v", "factor"])
        self.assertEqual(scale["description"], "Scales a vector by a factor\n\n")
        self.assertEqual(scale["in"], {
            "factor": {"type": "REAL",
                       "description": "Scale factor to apply", 
                       "dimension": None,
                       "interface_name": None,
                       "enum_type": None,
                  "attributes": [],
                  "kind": None,
                  "length": None,
                  "default_value": None,
                  "polymorphism_type": PolymorphismType.NONE
                       },
            "v": {"type": "REAL", 
                  "description": "Vector to scale", 
                  "dimension": {"dimensions": [ArrayBound(BoundType.ASSUMED_SHAPE)]},
                  "interface_name": None,
                  "enum_type": None,
                  "attributes": [],
                  "kind": None,
                  "length": None,
                  "default_value": None,
                  "polymorphism_type": PolymorphismType.NONE
                  }
        })
        self.assertEqual(scale["out"], {
            "v": {
            "type": "REAL", 
            "description": "Vector to scale", 
            "dimension": {"dimensions": [ArrayBound(BoundType.ASSUMED_SHAPE)]},
            "interface_name": None,
            "enum_type": None,
                  "attributes": [],
                  "kind": None,
                  "length": None,
                  "default_value": None,
                  "polymorphism_type": PolymorphismType.NONE
            }
        })
        self.assertNotIn("return", scale)  # It"s a subroutine, so no return value

    def test_named_interface_module_procedure(self):
        self.fs.create_file(
            "/fake/path/module_procedures.f90",
            contents="""\
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
"""
    )
        result = extract_module_data([Path("/fake/path/module_procedures.f90")])
        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(len(module["interfaces"]), 1)
        interface = module["interfaces"][0]

        self.assertEqual(interface["description"], "Interface for matrix operations\nProvides operations for matrix manipulation\n")
        self.assertEqual(interface["attributes"], [])
        self.assertEqual(interface["name"], "matrix_ops")
        self.assertIsNone(interface["operator_symbol"])
        
        self.assertEqual(len(interface["module_procedures"]), 3)
        self.assertEqual(len(interface["procedures"]), 1)

        noop = interface["module_procedures"]["noop"]
        self.assertEqual(noop["description"], "")

        transpose = interface["module_procedures"]["transpose_matrix"]
        self.assertEqual(transpose["description"], "")

        scale = interface["module_procedures"]["scale_matrix"]
        self.assertEqual(scale["description"], "Scales a matrix\n")

        add = interface["procedures"]["add_matrices"]
        self.assertEqual(add["arguments"], ["a", "b"])
        self.assertEqual(add["description"], "Adds two matrices\n\n")
        self.assertEqual(add["in"], {
            "a": {"type": "REAL", 
                  "description": 
                  "First matrix", 
                  "dimension": {"dimensions": [ArrayBound(BoundType.ASSUMED_SHAPE), ArrayBound(BoundType.ASSUMED_SHAPE)]},
                  "interface_name": None,
                  "enum_type": None,
                  "attributes": [],
                  "kind": None,
                  "length": None,
                  "default_value": None,
                  "polymorphism_type": PolymorphismType.NONE
                  },
            "b": {"type": "REAL", 
                  "description": "Second matrix", 
                  "dimension": {"dimensions": [ArrayBound(BoundType.ASSUMED_SHAPE), ArrayBound(BoundType.ASSUMED_SHAPE)]},
                  "interface_name": None,
                  "enum_type": None,
                  "attributes": [],
                  "kind": None,
                  "length": None,
                  "default_value": None,
                  "polymorphism_type": PolymorphismType.NONE
                  }
        })
        self.assertEqual(add["out"], {})
        self.assertEqual(add["return"], {
            "type": "REAL", 
            "description": "Sum of matrices", 
            "dimension": {"dimensions": [ArrayBound(BoundType.VARIABLE,
                                                    Expression(ExpressionType.LITERAL, value="1"),
                                                    Expression(ExpressionType.LITERAL, value="SIZE(a, 1)")),
                                        ArrayBound(BoundType.VARIABLE,
                                                   Expression(ExpressionType.LITERAL, value="1"),
                                                   Expression(ExpressionType.LITERAL, value="SIZE(a, 2)"))]},
            "interface_name": None,
            "enum_type": None,
            "attributes": [],
            "kind": None,
            "length": None,
            "default_value": None,
            "polymorphism_type": PolymorphismType.NONE
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
        result = extract_module_data([Path("/fake/path/operator_interface.f90")])
        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(len(module["interfaces"]), 1)
        interface = module["interfaces"][0]

        # Check interface properties
        self.assertEqual(interface["description"], "Interface for vector addition\n")
        self.assertEqual(interface["attributes"], [])
        self.assertEqual(interface["operator_symbol"], "+")

        # Check that we have two procedures
        self.assertEqual(len(interface["procedures"]), 2)

        # Check add_vectors function
        add_vectors = interface["procedures"]["add_vectors"]
        self.assertEqual(add_vectors["arguments"], ["a", "b"])
        self.assertEqual(add_vectors["description"], "Adds two vectors of equal size\n\n")
        self.assertEqual(add_vectors["in"], {
            "a": {"type": "REAL", 
                  "description": "First vector", 
                  "dimension": {"dimensions": [ArrayBound(BoundType.ASSUMED_SHAPE)]},
                  "interface_name": None,
                  "enum_type": None,
                  "attributes": [],
                  "kind": None,
                  "length": None,
                  "default_value": None,
                  "polymorphism_type": PolymorphismType.NONE
                  },
            "b": {"type": "REAL", 
                  "description": "Second vector", 
                  "dimension": {"dimensions": [ArrayBound(BoundType.ASSUMED_SHAPE)]},
                  "interface_name": None,
                  "enum_type": None,
                  "attributes": [],
                  "kind": None,
                  "length": None,
                  "default_value": None,
                  "polymorphism_type": PolymorphismType.NONE
            }
        })
        self.assertEqual(add_vectors["out"], {})
        self.assertEqual(add_vectors["return"], {
            "type": "REAL", 
            "description": "Sum of vectors", 
            "dimension": {"dimensions": [ArrayBound(BoundType.VARIABLE,
                                                    Expression(expr_type=ExpressionType.LITERAL, value="1"),
                                                    Expression(expr_type=ExpressionType.LITERAL, value='SIZE(a)'))]},
            "interface_name": None,
            "enum_type": None,
            "attributes": [],
            "kind": None,
            "length": None,
            "default_value": None,
            "polymorphism_type": PolymorphismType.NONE                            
        })

        # Check add_vector_scalar function
        add_vector_scalar = interface["procedures"]["add_vector_scalar"]
        self.assertEqual(add_vector_scalar["arguments"], ["a", "b"])
        self.assertEqual(add_vector_scalar["description"], "Adds a scalar to each element of a vector\n\n")
        self.assertEqual(add_vector_scalar["in"], {
            "a": {"type": "REAL", 
                  "description": "Input vector", 
                  "dimension": {"dimensions": [ArrayBound(BoundType.ASSUMED_SHAPE)]},
                  "interface_name": None,
                  "enum_type": None,
                  "attributes": [],
                  "kind": None,
                  "length": None,
                  "default_value": None,
                  "polymorphism_type": PolymorphismType.NONE
                  },
            "b": {"type": "REAL", 
                  "description": "Scalar value", 
                  "dimension": None,
                  "interface_name": None,
                  "enum_type": None,
                  "attributes": [],
                  "kind": None,
                  "length": None,
                  "default_value": None,
                  "polymorphism_type": PolymorphismType.NONE
                  }
        })
        self.assertEqual(add_vector_scalar["out"], {})
        self.assertEqual(add_vector_scalar["return"], {
            "type": "REAL", 
            "description": "Scaled vector", 
            "dimension": {"dimensions": [ArrayBound(BoundType.VARIABLE,
                                                    Expression(expr_type=ExpressionType.LITERAL, value="1"),
                                                    Expression(expr_type=ExpressionType.LITERAL, value="SIZE(a)"))]},
            "interface_name": None,
            "enum_type": None,
            "attributes": [],
            "kind": None,
            "length": None,
            "default_value": None,
            "polymorphism_type": PolymorphismType.NONE
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
        self.assertEqual(len(module["interfaces"]), 1)
        
        # Check interface properties
        interface = module["interfaces"][0]
        self.assertEqual(
            interface["description"], 
            "Interface for vector addition operations\n"
            "Provides overloaded + operator for vector-vector and vector-scalar addition\n"
        )
        self.assertEqual(interface["attributes"], [])
        self.assertEqual(interface["operator_symbol"], "+")
        
        # Check module procedures are correctly recorded
        self.assertEqual(len(interface["module_procedures"]), 2)
        
        # Check add_vectors module procedure
        add_vectors_proc = interface["module_procedures"]["add_vectors"]
        self.assertEqual(add_vectors_proc["name"], "add_vectors")
        self.assertEqual(
            add_vectors_proc["description"],
            "Adds two vectors of equal size\n"
        )
        
        add_scalar_proc = interface["module_procedures"]["add_vector_scalar"]
        self.assertEqual(add_scalar_proc["name"], "add_vector_scalar")
        self.assertEqual(
            add_scalar_proc["description"],
            "Adds a scalar to each element of a vector\n"
        )
        
        # Check that procedures dictionary is empty (as these are module procedures)
        self.assertEqual(len(interface["procedures"]), 0)
        self.assertEqual(len(module["functions"]), 2)

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
        self.assertEqual(len(module["interfaces"]), 1)
        
        # Check interface properties
        interface = module["interfaces"][0]
        self.assertEqual(
            interface["description"], 
            "Custom assignment interface for vector types\n"
            "Allows direct assignment between allocatable and non-allocatable vectors\n"
        )
        self.assertEqual(interface["attributes"], [])
        self.assertEqual(interface["operator_symbol"], "=")
        
        # Check module procedures are correctly recorded
        self.assertEqual(len(interface["module_procedures"]), 1)
        
        # Check assign_vector module procedure
        assign_vector_mp = interface["module_procedures"]["assign_vector"]
        self.assertEqual(assign_vector_mp["name"], "assign_vector")
        self.assertEqual(
            assign_vector_mp["description"],
            "Assigns values from one vector to another, handling allocation\n"
        )
        
        # No explicit procedures in this interface
        self.assertEqual(interface["procedures"], {})
        self.assertEqual(len(module["subroutines"]), 1)

    def test_user_defined_operator_interface(self):
        self.fs.create_file(
            "/fake/path/user_operator.f90",
            contents="""\
    module test_mod
        implicit none

        type :: vector3d
            real :: x, y, z
        end type vector3d

        !!*
        ! User-defined cross product operator for 3D vectors
        ! Allows syntax like: c = a .cross. b
        !*!
        interface operator(.cross.)
            !!*
            ! Computes the cross product of two 3D vectors
            ! @in a First vector
            ! @in b Second vector 
            ! @return Cross product vector
            !*!
            function cross_product(a, b) result(c)
                import :: vector3d
                type(vector3d), intent(in) :: a, b
                type(vector3d) :: c
            end function cross_product
        end interface

        !!*
        ! User-defined dot product operator for 3D vectors
        ! Allows syntax like: scalar = a .dot. b
        !*!
        interface operator(.dot.)
            !!*
            ! Computes the dot product of two 3D vectors
            !*!
            module procedure dot_product_3d
        end interface

    contains
        function cross_product(a, b) result(c)
            type(vector3d), intent(in) :: a, b
            type(vector3d) :: c
            c%x = a%y * b%z - a%z * b%y
            c%y = a%z * b%x - a%x * b%z
            c%z = a%x * b%y - a%y * b%x
        end function cross_product

        function dot_product_3d(a, b) result(scalar)
            type(vector3d), intent(in) :: a, b
            real :: scalar
            scalar = a%x * b%x + a%y * b%y + a%z * b%z
        end function dot_product_3d
    end module test_mod
    """
        )
        result = extract_module_data([Path("/fake/path/user_operator.f90")])
        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(len(module["interfaces"]), 2)
        
        # Check .cross. operator interface
        cross_interface = None
        dot_interface = None
        for interface in module["interfaces"]:
            if interface.get("operator_symbol") == ".CROSS.":
                cross_interface = interface
            elif interface.get("operator_symbol") == ".DOT.":
                dot_interface = interface
        
        self.assertIsNotNone(cross_interface, "Should find .cross. operator interface")
        self.assertIsNotNone(dot_interface, "Should find .dot. operator interface")
        
        # Check .cross. interface properties
        self.assertEqual(
            cross_interface["description"], 
            "User-defined cross product operator for 3D vectors\n"
            "Allows syntax like: c = a .cross. b\n"
        )
        self.assertEqual(cross_interface["attributes"], [])
        self.assertEqual(cross_interface["operator_symbol"], ".CROSS.")
        self.assertEqual(cross_interface["name"], "")
        self.assertIsNone(cross_interface.get("generic_spec"))  # Should not have generic spec
        
        # Check explicit procedure
        self.assertEqual(len(cross_interface["procedures"]), 1)
        self.assertEqual(len(cross_interface["module_procedures"]), 0)
        
        cross_func = cross_interface["procedures"]["cross_product"]
        self.assertEqual(cross_func["arguments"], ["a", "b"])
        self.assertEqual(
            cross_func["description"],
            "Computes the cross product of two 3D vectors\n\n"
        )
        self.assertEqual(list(cross_func["in"].keys()), ["a", "b"])
        self.assertEqual(cross_func["in"]["a"]["type"], "vector3d")
        self.assertEqual(cross_func["in"]["a"]["description"], "First vector")
        self.assertEqual(cross_func["in"]["b"]["type"], "vector3d")
        self.assertEqual(cross_func["in"]["b"]["description"], "Second vector")
        self.assertEqual(cross_func["out"], {})
        self.assertIsNotNone(cross_func["return"])
        self.assertEqual(cross_func["return"]["type"], "vector3d")
        self.assertEqual(cross_func["return"]["description"], "Cross product vector")
        self.assertEqual(cross_func["attributes"], [])
        self.assertIsNone(cross_func["binding_type"])
        self.assertEqual(cross_func["argument_interfaces"], {})
        
        # Check .dot. interface properties
        self.assertEqual(
            dot_interface["description"],
            "User-defined dot product operator for 3D vectors\n"
            "Allows syntax like: scalar = a .dot. b\n"
        )
        self.assertEqual(dot_interface["attributes"], [])
        self.assertEqual(dot_interface["operator_symbol"], ".DOT.")
        self.assertEqual(dot_interface["name"], "")
        
        # Check module procedure
        self.assertEqual(len(dot_interface["procedures"]), 0)
        self.assertEqual(len(dot_interface["module_procedures"]), 1)
        
        dot_mp = dot_interface["module_procedures"]["dot_product_3d"]
        self.assertEqual(dot_mp["name"], "dot_product_3d")
        self.assertEqual(
            dot_mp["description"],
            "Computes the dot product of two 3D vectors\n"
        )
        
        # Check the actual function implementations exist and match expectations
        self.assertEqual(len(module["functions"]), 2)
        self.assertIn("cross_product", module["functions"])
        self.assertIn("dot_product_3d", module["functions"])
        
        # Verify function implementations match their interface declarations
        cross_impl = module["functions"]["cross_product"]
        self.assertEqual(cross_impl["arguments"], ["a", "b"])
        self.assertEqual(cross_impl["in"]["a"]["type"], "vector3d") 
        self.assertEqual(cross_impl["in"]["b"]["type"], "vector3d")
        self.assertEqual(cross_impl["return"]["type"], "vector3d")
        
        dot_impl = module["functions"]["dot_product_3d"]
        self.assertEqual(dot_impl["arguments"], ["a", "b"])
        self.assertEqual(dot_impl["return"]["type"], "REAL")

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
        self.assertEqual(len(module["interfaces"]), 4)  # abstract, dot, assignment, and norm interfaces
        
        # Validate we have all expected procedure implementations
        self.assertEqual(len(module["functions"]), 4)
        self.assertEqual(len(module["subroutines"]), 2)
        
        # Check abstract interface
        func_interface = module["interfaces"][0]
        self.assertEqual(
            func_interface["description"], 
            "Interface for numerical function evaluation\n"
            "Used in integration and other numerical methods\n"
        )
        self.assertEqual(func_interface["attributes"], ["ABSTRACT"])
        self.assertEqual(func_interface["name"], "")
        self.assertIsNone(func_interface["operator_symbol"])
        
        # Check explicit procedure in abstract interface
        self.assertEqual(len(func_interface["procedures"]), 1)
        self.assertEqual(len(func_interface["module_procedures"]), 0)  # No module procedures
        
        proc = func_interface["procedures"]["func_interface"]
        self.assertEqual(proc["arguments"], ["x"])
        self.assertEqual(proc["description"], "Function template for numerical operations\n\n")
        self.assertEqual(proc["in"], {
            "x": {"type": "REAL", 
                  "description": "Point at which to evaluate function", 
                  "dimension": None, 
                  "interface_name": None, 
                  "enum_type": None,
                    "attributes": [],
                    "kind": None,
                    "length": None,
                    "default_value": None,
                    "polymorphism_type": PolymorphismType.NONE
                  }
        })
        self.assertEqual(proc["out"], {})
        self.assertEqual(proc["return"], {
            "type": "REAL", 
            "description": "Function value at x", 
            "dimension": None, 
            "interface_name": None, 
            "enum_type": None,
            "attributes": [],
            "kind": None,
            "length": None,
            "default_value": None,
            "polymorphism_type": PolymorphismType.NONE
        })
        self.assertEqual(proc["attributes"], [])
        self.assertEqual(proc["argument_interfaces"], {})
        
        # Check dot product operator interface
        dot_interface = module["interfaces"][1]
        self.assertEqual(dot_interface["operator_symbol"], ".DOT.")
        self.assertEqual(dot_interface["name"], "")
        self.assertEqual(dot_interface["attributes"], [])
        self.assertEqual(len(dot_interface["module_procedures"]), 2)
        self.assertEqual(len(dot_interface["procedures"]), 0)  # No explicit procedures
        self.assertEqual(
            dot_interface["description"],
            "Interface for dot product operations\n"
            "Supports both single and double precision vectors\n"
        )
        
        dot_r = dot_interface["module_procedures"]["dot_product_r"]
        self.assertEqual(dot_r["name"], "dot_product_r")
        self.assertEqual(dot_r["description"], "Computes dot product of two real vectors\n")
        
        dot_d = dot_interface["module_procedures"]["dot_product_d"]
        self.assertEqual(dot_d["name"], "dot_product_d")
        self.assertEqual(dot_d["description"], "Computes dot product of two double precision vectors\n")
        
        # Verify these functions actually exist in module
        self.assertIn("dot_product_r", module["functions"])
        self.assertIn("dot_product_d", module["functions"])
        
        # Check assignment interface
        assign_interface = module["interfaces"][2]
        self.assertEqual(assign_interface["operator_symbol"], "=")
        self.assertEqual(assign_interface["name"], "")
        self.assertEqual(assign_interface["attributes"], [])
        self.assertEqual(len(assign_interface["module_procedures"]), 2)
        self.assertEqual(len(assign_interface["procedures"]), 0)  # No explicit procedures
        self.assertEqual(
            assign_interface["description"],
            "Matrix assignment interface\n"
            "Handles allocation and assignment for matrices\n"
        )
        
        assign_r = assign_interface["module_procedures"]["assign_matrix_real"]
        self.assertEqual(assign_r["name"], "assign_matrix_real")
        self.assertEqual(assign_r["description"], "Assigns real matrix values with automatic allocation\n")
        
        assign_d = assign_interface["module_procedures"]["assign_matrix_double"]
        self.assertEqual(assign_d["name"], "assign_matrix_double")
        self.assertEqual(assign_d["description"], "Assigns double precision matrix values with automatic allocation\n")
        
        # Verify these subroutines actually exist in module
        self.assertIn("assign_matrix_real", module["subroutines"])
        self.assertIn("assign_matrix_double", module["subroutines"])
        
        # Check norm interface (generic, no operator)
        norm_interface = module["interfaces"][3]
        self.assertEqual(norm_interface["name"], "norm")
        self.assertIsNone(norm_interface["operator_symbol"])
        self.assertEqual(norm_interface["attributes"], [])
        self.assertEqual(len(norm_interface["module_procedures"]), 2)
        self.assertEqual(len(norm_interface["procedures"]), 0)  # No explicit procedures
        self.assertEqual(
            norm_interface["description"],
            "Generic norm calculation interface\n"
            "Computes Euclidean norm for vectors and Frobenius norm for matrices\n"
        )
        
        vector_n = norm_interface["module_procedures"]["vector_norm"]
        self.assertEqual(vector_n["name"], "vector_norm")
        self.assertEqual(vector_n["description"], "Computes the Euclidean norm of a vector\n")
        
        matrix_n = norm_interface["module_procedures"]["matrix_norm"]
        self.assertEqual(matrix_n["name"], "matrix_norm")
        self.assertEqual(matrix_n["description"], "Computes the Frobenius norm of a matrix\n")
        
        # Verify these functions actually exist in module
        self.assertIn("vector_norm", module["functions"])
        self.assertIn("matrix_norm", module["functions"])
        
        # Cross-check: Verify function implementations have expected structure
        vector_norm_impl = module["functions"]["vector_norm"]
        self.assertEqual(vector_norm_impl["arguments"], ["v"])
        self.assertEqual(vector_norm_impl["return"]["type"], "REAL")
        
        matrix_norm_impl = module["functions"]["matrix_norm"]
        self.assertEqual(matrix_norm_impl["arguments"], ["m"])
        self.assertEqual(matrix_norm_impl["return"]["type"], "REAL")

    def test_cross_module_interface_resolution(self):
        # Create the basic math module
        self.fs.create_file(
            "/fake/path/math_basic.f90",
            contents="""\
    module math_basic
        implicit none
        
        !!*
        ! Basic numerical integration interface
        ! Simple two-parameter integration
        !*!
        abstract interface
            !!*
            ! Basic integration function signature
            ! @in f Function to integrate
            ! @in a Lower bound
            ! @in b Upper bound
            ! @return Integral value
            !*!
            function integrate(f, a, b) result(val)
                real, intent(in) :: a, b
                real :: val
                interface
                    function f(x)
                        real, intent(in) :: x
                        real :: f
                    end function
                end interface
            end function integrate
        end interface
        
    end module math_basic
    """
        )
        
        # Create the advanced math module
        self.fs.create_file(
            "/fake/path/math_advanced.f90", 
            contents="""\
    module math_advanced
        implicit none
        
        !!*
        ! Advanced numerical integration interface
        ! Includes method selection and tolerance control
        !*!
        abstract interface
            !!*
            ! Advanced integration function signature
            ! @in f Function to integrate
            ! @in a Lower bound  
            ! @in b Upper bound
            ! @in method Integration method identifier
            ! @in tolerance Convergence tolerance
            ! @return Integral value
            !*!
            function integrate(f, a, b, method, tolerance) result(val)
                real, intent(in) :: a, b, tolerance
                integer, intent(in) :: method
                real :: val
                interface
                    function f(x)
                        real, intent(in) :: x
                        real :: f
                    end function
                end interface  
            end function integrate
        end interface
        
    end module math_advanced
    """
        )
        
        # Create the integration module that uses both
        self.fs.create_file(
            "/fake/path/integration_composite.f90",
            contents="""\
    module integration_composite
        use math_basic, only: basic_integrate => integrate
        use math_advanced  ! brings in everything including integrate
        implicit none
        
        !!*
        ! Local integration interface for simple functions
        ! Single parameter, no bounds needed
        !*!
        abstract interface
            !!*
            ! Local simple integration interface
            ! @in x Evaluation point
            ! @return Function value
            !*!
            function local_integrate(x) result(y)
                real, intent(in) :: x
                real :: y
            end function local_integrate
        end interface
        
    contains
        
        !!*
        ! Composite integration function using different interface sources
        ! @in simple_func Uses local interface
        ! @in basic_func Uses renamed import from math_basic
        ! @in advanced_func Uses direct import from math_advanced
        ! @in x Input value
        ! @return Combined integration result
        !*!
        function composite_integration(simple_func, basic_func, advanced_func, x) result(result_val)
            procedure(local_integrate) :: simple_func     ! local interface
            procedure(basic_integrate) :: basic_func      ! from math_basic via rename
            procedure(integrate) :: advanced_func         ! from math_advanced
            real, intent(in) :: x
            real :: result_val
            
            result_val = simple_func(x)
        end function composite_integration
        
    end module integration_composite
    """
        )
        
        # Extract all modules
        all_files = [
            Path("/fake/path/math_basic.f90"),
            Path("/fake/path/math_advanced.f90"), 
            Path("/fake/path/integration_composite.f90")
        ]
        result = extract_module_data(all_files)
        self.assertEqual(len(result), 3)
        
        # Find each module
        math_basic = next(m for m in result if m["module_name"] == "math_basic")
        math_advanced = next(m for m in result if m["module_name"] == "math_advanced") 
        integration_composite = next(m for m in result if m["module_name"] == "integration_composite")
        
        # Verify basic structure is parsed correctly
        self.assertEqual(len(math_basic["interfaces"]), 1)
        self.assertEqual(len(math_advanced["interfaces"]), 1) 
        self.assertEqual(len(integration_composite["interfaces"]), 1)
        self.assertEqual(len(integration_composite["functions"]), 1)
        
        # Check that imported modules are tracked
        self.assertEqual(len(integration_composite["imports"]), 2)
        
        # Verify import information
        basic_import = next(imp for imp in integration_composite["imports"] if imp["module"] == "math_basic")
        advanced_import = next(imp for imp in integration_composite["imports"] if imp["module"] == "math_advanced")
        
        self.assertEqual(basic_import["type"], "only")
        self.assertEqual(basic_import["items"], [{"local_name": "basic_integrate", "original_name": "integrate"}])
        
        self.assertEqual(advanced_import["type"], "all")
        self.assertEqual(advanced_import["items"], [])  # Empty for 'use module' without only
        
        # Check that interfaces have different signatures (same name, different args)
        basic_integrate_proc = math_basic["interfaces"][0]["procedures"]["integrate"]
        advanced_integrate_proc = math_advanced["interfaces"][0]["procedures"]["integrate"]
        
        self.assertEqual(len(basic_integrate_proc["arguments"]), 3)  # f, a, b
        self.assertEqual(len(advanced_integrate_proc["arguments"]), 5)  # f, a, b, method, tolerance
        
        # Check the composite function structure
        composite_func = integration_composite["functions"]["composite_integration"]
        self.assertEqual(composite_func["arguments"], ["simple_func", "basic_func", "advanced_func", "x"])
        
        # Check that procedure arguments have correct interface names after resolution
        simple_func_arg = composite_func["in"]["simple_func"]
        basic_func_arg = composite_func["in"]["basic_func"] 
        advanced_func_arg = composite_func["in"]["advanced_func"]
        
        self.assertEqual(simple_func_arg["type"], "PROCEDURE")
        self.assertEqual(basic_func_arg["type"], "PROCEDURE")
        self.assertEqual(advanced_func_arg["type"], "PROCEDURE")
        
        # Check resolved interface names
        self.assertEqual(simple_func_arg["interface_name"], "local_integrate")  # Local interface
        self.assertEqual(basic_func_arg["interface_name"], "basic_integrate")   # Renamed import
        self.assertEqual(advanced_func_arg["interface_name"], "integrate")      # Direct import
        
        # Check argument descriptions
        self.assertEqual(simple_func_arg["description"], "Uses local interface")
        self.assertEqual(basic_func_arg["description"], "Uses renamed import from math_basic")
        self.assertEqual(advanced_func_arg["description"], "Uses direct import from math_advanced")
        
        # Verify that argument_interfaces contains resolved interface descriptions
        self.assertEqual(len(composite_func["argument_interfaces"]), 3)
        self.assertIn("local_integrate", composite_func["argument_interfaces"])
        self.assertIn("basic_integrate", composite_func["argument_interfaces"])
        self.assertIn("integrate", composite_func["argument_interfaces"])
        
        # Check local interface resolution
        local_interface = composite_func["argument_interfaces"]["local_integrate"]
        self.assertEqual(local_interface["description"], "\nLocal integration interface for simple functions\nSingle parameter, no bounds needed\n\n")
        self.assertEqual(local_interface["source_module"], "integration_composite")
        self.assertEqual(len(local_interface["procedures"]), 1)
        
        local_proc = local_interface["procedures"]["local_integrate"]
        self.assertEqual(local_proc["arguments"], ["x"])
        self.assertEqual(local_proc["description"], "\nLocal simple integration interface\n\n")
        
        # Check cross-module interface resolution for basic_integrate
        basic_interface = composite_func["argument_interfaces"]["basic_integrate"]
        self.assertEqual(basic_interface["description"], "\nBasic numerical integration interface\nSimple two-parameter integration\n\n")
        self.assertEqual(basic_interface["source_module"], "math_basic")
        self.assertEqual(basic_interface["original_name"], "integrate")  # Track original name
        self.assertEqual(len(basic_interface["procedures"]), 1)
        
        basic_proc = basic_interface["procedures"]["integrate"]  # Original procedure name
        self.assertEqual(basic_proc["arguments"], ["f", "a", "b"])
        self.assertEqual(basic_proc["description"], "\nBasic integration function signature\n\n")
        
        # Check cross-module interface resolution for integrate (from math_advanced)
        advanced_interface = composite_func["argument_interfaces"]["integrate"]
        self.assertEqual(advanced_interface["description"], "\nAdvanced numerical integration interface\nIncludes method selection and tolerance control\n\n")
        self.assertEqual(advanced_interface["source_module"], "math_advanced")
        self.assertNotIn("original_name", advanced_interface)  # No rename
        self.assertEqual(len(advanced_interface["procedures"]), 1)
        
        advanced_proc = advanced_interface["procedures"]["integrate"]
        self.assertEqual(advanced_proc["arguments"], ["f", "a", "b", "method", "tolerance"])
        self.assertEqual(advanced_proc["description"], "\nAdvanced integration function signature\n\n")
        
        # Verify cross-references work both ways - check source modules contain the interfaces
        basic_source_interface = math_basic["interfaces"][0]
        self.assertEqual(basic_source_interface["procedures"]["integrate"]["arguments"], ["f", "a", "b"])
        
        advanced_source_interface = math_advanced["interfaces"][0] 
        self.assertEqual(advanced_source_interface["procedures"]["integrate"]["arguments"], ["f", "a", "b", "method", "tolerance"])


if __name__ == "__main__":
    unittest.main()

