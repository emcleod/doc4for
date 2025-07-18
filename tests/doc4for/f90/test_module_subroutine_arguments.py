import unittest
from pathlib import Path
from pyfakefs.fake_filesystem_unittest import TestCase
from doc4for.f90.generate_module_tree import extract_module_data
from doc4for.models.common import Expression, ExpressionType, BindingType, BindingTypeEnum
from doc4for.models.dimension_models import ArrayBound, BoundType
from doc4for.models.variable_models import PolymorphismType
#TODO assumed size and assumed rank arrays

class TestSubroutineArguments(TestCase):
    maxDiff=None
    def setUp(self):
        self.setUpPyfakefs()

    def test_subroutine_with_scalar_args_1(self):
        self.fs.create_file(
            "/fake/path/scalar_args.f90",
            contents="""\
    module scalar_args_subroutines
    contains
    !!*
    ! A subroutine with scalar arguments
    !*!
    subroutine add_numbers(x, y, res)
        real, intent(in) :: x, y
        real, intent(out) :: res
        res = x + y
    end subroutine add_numbers
    end module scalar_args_subroutines
                            """,
        )
        result = extract_module_data([Path("/fake/path/scalar_args.f90")])

        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(module["module_name"], "scalar_args_subroutines")
        self.assertEqual(len(module["subroutines"]), 1)
        self.assertIn("add_numbers", module["subroutines"])
        subroutine = module["subroutines"]["add_numbers"]
        inputs = subroutine["in"]
        outputs = subroutine["out"]
        self.assertEqual(len(inputs), 2)
        self.assertEqual(inputs["x"], {"type": "REAL", 
                                       "description": "", 
                                       "dimension": None, 
                                       "interface_name": None, 
                                       "enum_type": None,
                                       "kind": None,
                                       "length": None,
                                       "attributes": [],
                                       "default_value": None,
                                       "polymorphism_type": PolymorphismType.NONE,
                                       "type_params": None})
        self.assertEqual(inputs["y"], {"type": "REAL", 
                                       "description": "", 
                                       "dimension": None, 
                                       "interface_name": None, 
                                       "enum_type": None,
                                       "kind": None,
                                       "length": None,
                                       "attributes": [],
                                       "default_value": None,
                                       "polymorphism_type": PolymorphismType.NONE,
                                       "type_params": None})
        self.assertEqual(len(outputs), 1)
        self.assertEqual(outputs["res"], {"type": "REAL", 
                                          "description": "", 
                                          "dimension": None, 
                                          "interface_name": None, 
                                          "enum_type": None,
                                          "kind": None,
                                          "length": None,
                                          "attributes": [],
                                          "default_value": None,
                                          "polymorphism_type": PolymorphismType.NONE,
                                          "type_params": None})

    def test_subroutine_with_scalar_args_2(self):
        self.fs.create_file(
            "/fake/path/scalar_args.f90",
            contents="""\
    module scalar_args_subroutines
    contains
    !!*
    ! A subroutine with scalar arguments
    !*!
    subroutine add_numbers(x, y, res)
        real, intent(in) :: x
        real, intent(in) :: y
        real, intent(out) :: res
        res = x + y
    end subroutine add_numbers
    end module scalar_args_subroutines
                            """,
        )
        result = extract_module_data([Path("/fake/path/scalar_args.f90")])

        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(module["module_name"], "scalar_args_subroutines")
        self.assertEqual(len(module["subroutines"]), 1)
        self.assertIn("add_numbers", module["subroutines"])
        subroutine = module["subroutines"]["add_numbers"]
        inputs = subroutine["in"]
        outputs = subroutine["out"]
        self.assertEqual(len(inputs), 2)
        self.assertEqual(inputs["x"], {"type": "REAL", 
                                       "description": "", 
                                       "dimension": None, 
                                       "interface_name": None, 
                                       "enum_type": None,
                                       "kind": None,
                                       "length": None,
                                       "attributes": [],
                                       "default_value": None,
                                       "polymorphism_type": PolymorphismType.NONE,
                                       "type_params": None})
        self.assertEqual(inputs["y"], {"type": "REAL", 
                                       "description": "", 
                                       "dimension": None, 
                                       "interface_name": None, 
                                       "enum_type": None,
                                       "kind": None,
                                       "length": None,
                                       "attributes": [],
                                       "default_value": None,
                                       "polymorphism_type": PolymorphismType.NONE,
                                       "type_params": None})
        self.assertEqual(len(outputs), 1)
        self.assertEqual(outputs["res"], {"type": "REAL", 
                                          "description": "", 
                                          "dimension": None, 
                                          "interface_name": None, 
                                          "enum_type": None,
                                          "kind": None,
                                          "length": None,
                                          "attributes": [],
                                          "default_value": None,
                                          "polymorphism_type": PolymorphismType.NONE,
                                          "type_params": None})

    def test_subroutine_with_kind_length_default_value(self):
        self.fs.create_file(
            "/fake/path/scalar_args.f90",
            contents="""\
    module scalar_args_subroutines
    contains
    !!*
    ! A subroutine with scalar arguments
    !*!
    subroutine add_numbers(x, y, res)
        real(kind=real64), intent(in) :: x
        character(len=20), intent(in) :: y
        real, intent(out) :: res = 35
        res = x + y
    end subroutine add_numbers
    end module scalar_args_subroutines
                            """,
        )
        result = extract_module_data([Path("/fake/path/scalar_args.f90")])

        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(module["module_name"], "scalar_args_subroutines")
        self.assertEqual(len(module["subroutines"]), 1)
        self.assertIn("add_numbers", module["subroutines"])
        subroutine = module["subroutines"]["add_numbers"]
        inputs = subroutine["in"]
        outputs = subroutine["out"]
        self.assertEqual(len(inputs), 2)
        self.assertEqual(inputs["x"], {"type": "REAL", 
                                       "description": "", 
                                       "dimension": None, 
                                       "interface_name": None, 
                                       "enum_type": None,
                                       "kind": "real64",
                                       "length": None,
                                       "attributes": [],
                                       "default_value": None,
                                       "polymorphism_type": PolymorphismType.NONE,
                                       "type_params": None})
        self.assertEqual(inputs["y"], {"type": "CHARACTER", 
                                       "description": "", 
                                       "dimension": None, 
                                       "interface_name": None, 
                                       "enum_type": None,
                                       "kind": None,
                                       "length": "20",
                                       "attributes": [],
                                       "default_value": None,
                                       "polymorphism_type": PolymorphismType.NONE,
                                       "type_params": None})
        self.assertEqual(len(outputs), 1)
        self.assertEqual(outputs["res"], {"type": "REAL", 
                                          "description": "", 
                                          "dimension": None, 
                                          "interface_name": None, 
                                          "enum_type": None,
                                          "kind": None,
                                          "length": None,
                                          "attributes": [],
                                          "default_value": "35",
                                          "polymorphism_type": PolymorphismType.NONE,
                                          "type_params": None})

    def test_subroutine_with_array_args(self):
        self.fs.create_file(
            "/fake/path/array_args.f90",
            contents="""\
    module array_args_subroutines
    contains
    !!*
    ! A subroutine with array arguments
    !*!
    subroutine sum_array(arr, res)
        real, intent(in) :: arr(:)
        real, intent(out) :: res
        res = sum(arr)
    end subroutine sum_array
    end module array_args_subroutines
                            """,
        )
        result = extract_module_data([Path("/fake/path/array_args.f90")])

        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(module["module_name"], "array_args_subroutines")
        self.assertEqual(len(module["subroutines"]), 1)
        self.assertIn("sum_array", module["subroutines"])
        subroutine = module["subroutines"]["sum_array"]
        inputs = subroutine["in"]
        outputs = subroutine["out"]
        self.assertEqual(len(inputs), 1)
        self.assertEqual(inputs["arr"], {
            "type": "REAL", 
            "description": "", 
            "dimension": {"dimensions": [ArrayBound(BoundType.ASSUMED_SHAPE)]},
            "interface_name": None, 
            "enum_type": None,
            "kind": None,
            "length": None,
            "attributes": [],
            "default_value": None,
            "polymorphism_type": PolymorphismType.NONE,
            "type_params": None})
        self.assertEqual(len(outputs), 1)
        self.assertEqual(outputs["res"], {"type": "REAL", 
                                          "description": "", 
                                          "dimension": None, 
                                          "interface_name": None, 
                                          "enum_type": None,
                                          "kind": None,
                                          "length": None,
                                          "attributes": [],
                                          "default_value": None,
                                          "polymorphism_type": PolymorphismType.NONE,
                                          "type_params": None})

    def test_subroutine_with_inout_args(self):
        self.fs.create_file(
            "/fake/path/inout_args.f90",
            contents="""\
    module inout_args_subroutines
    contains
    !!*
    ! A subroutine with inout arguments
    !*!
    subroutine increment(x)
        real, intent(inout) :: x
        x = x + 1.0
    end subroutine increment
    end module inout_args_subroutines
                            """,
        )
        result = extract_module_data([Path("/fake/path/inout_args.f90")])

        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(module["module_name"], "inout_args_subroutines")
        self.assertEqual(len(module["subroutines"]), 1)
        self.assertIn("increment", module["subroutines"])
        subroutine = module["subroutines"]["increment"]
        inputs = subroutine["in"]
        outputs = subroutine["out"]
        self.assertEqual(len(inputs), 1)
        self.assertEqual(inputs["x"], {"type": "REAL", 
                                       "description": "", 
                                       "dimension": None, 
                                       "interface_name": None, 
                                       "enum_type": None,                                       
                                       "kind": None,
                                       "length": None,
                                       "attributes": [],
                                       "default_value": None,
                                       "polymorphism_type": PolymorphismType.NONE,
                                       "type_params": None})
        self.assertEqual(len(outputs), 1)
        self.assertEqual(outputs["x"], {"type": "REAL", 
                                        "description": "", 
                                        "dimension": None, 
                                        "interface_name": None, 
                                        "enum_type": None,
                                        "kind": None,
                                        "length": None,
                                        "attributes": [],
                                        "default_value": None,
                                        "polymorphism_type": PolymorphismType.NONE,
                                        "type_params": None})

    def test_subroutine_with_mixed_args(self):
        self.fs.create_file(
            "/fake/path/mixed_args.f90",
            contents="""\
    module mixed_args_subroutines
    contains
    !!*
    ! A subroutine with mixed scalar and array arguments
    !*!
    subroutine multiply_scalar_array(scalar, arr, res)
        real, intent(in) :: scalar
        real, intent(in) :: arr(:)
        real, intent(out), allocatable :: res(:)
        res = scalar * arr
    end subroutine multiply_scalar_array
    end module mixed_args_subroutines
                            """,
        )
        result = extract_module_data([Path("/fake/path/mixed_args.f90")])

        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(module["module_name"], "mixed_args_subroutines")
        self.assertEqual(len(module["subroutines"]), 1)
        self.assertIn("multiply_scalar_array", module["subroutines"])
        subroutine = module["subroutines"]["multiply_scalar_array"]
        inputs = subroutine["in"]
        outputs = subroutine["out"]
        self.assertEqual(len(inputs), 2)
        self.assertEqual(inputs["scalar"], {"type": "REAL", 
                                            "description": "", 
                                            "dimension": None, 
                                            "interface_name": None, 
                                            "enum_type": None,
                                            "kind": None,
                                            "length": None,
                                            "attributes": [],
                                            "default_value": None,
                                            "polymorphism_type": PolymorphismType.NONE,
                                            "type_params": None
                                            })
        self.assertEqual(inputs["arr"], {
            "type": "REAL", 
            "description": "", 
            "dimension": {"dimensions": [ArrayBound(BoundType.ASSUMED_SHAPE, lower=None, upper=None)]},
            "interface_name": None,
            "enum_type": None,
            "kind": None,
            "length": None,
            "attributes": [],
            "default_value": None,
            "polymorphism_type": PolymorphismType.NONE,
            "type_params": None})
        self.assertEqual(len(outputs), 1)
        self.assertEqual(outputs["res"], {
            "type": "REAL", 
            "description": "", 
            "dimension": {"dimensions": [ArrayBound(BoundType.ASSUMED_SHAPE, lower=None, upper=None)]},
            "interface_name": None,
            "enum_type": None,
            "kind": None,
            "length": None,
            "attributes": ["ALLOCATABLE"],
            "default_value": None,
            "polymorphism_type": PolymorphismType.NONE,
            "type_params": None})

    def test_subroutine_with_polymorphic_args(self):
        self.fs.create_file(
            "/fake/path/inout_args.f90",
            contents="""\
module shape_module
    implicit none
    
    !!* Base type *!
    type :: shape
        real :: area
    contains
        procedure :: calculate_area
    end type shape
    
    !!* Derived type *!
    type, extends(shape) :: circle
        real :: radius
    contains
        procedure :: calculate_area => circle_area
    end type circle
    
    type, extends(shape) :: rectangle
        real :: width, height
    contains
        procedure :: calculate_area => rectangle_area
    end type rectangle

contains
    
    !!* 
    ! Procedure with polymorphic argument (CLASS) 
    ! @inout s The shape
    !*!
    subroutine process_shape_polymorphic(s)
        class(shape), intent(inout) :: s  ! Polymorphic - can accept shape or any extended type
        call s%calculate_area()
    end subroutine
    
    !!*
    ! Procedure with non-polymorphic argument
    ! @inout s The shape
    !*!
    subroutine process_shape_monomorphic(s)
        type(shape), intent(inout) :: s   ! Non-polymorphic - only accepts exact shape type
        call s%calculate_area()
    end subroutine
    
    !!*
    ! Procedure with intrinsic type argument
    ! @in x The real value 
    !*!
    subroutine process_real_value(x)
        real, intent(in) :: x             ! Intrinsic type
        print *, "Value:", x
    end subroutine

    !!*
    ! @inout this The shape
    !*!
    subroutine calculate_area(this)
        class(shape), intent(inout) :: this
        this%area = 0.0
    end subroutine
    
    !!*
    ! @inout this
    !*!
    subroutine circle_area(this)
        class(circle), intent(inout) :: this
        this%area = 3.14159 * this%radius**2
    end subroutine
    
    subroutine rectangle_area(this)
        class(rectangle), intent(inout) :: this
        this%area = this%width * this%height
    end subroutine
end module shape_module
                            """,
        )
        result = extract_module_data([Path("/fake/path/inout_args.f90")])

        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(module["module_name"], "shape_module")
        self.assertEqual(len(module["types"]), 3)
        self.assertEqual(len(module["subroutines"]), 6)
        self.assertIn("process_shape_polymorphic", module["subroutines"])
        subroutine = module["subroutines"]["process_shape_polymorphic"]
        self.assertEqual(subroutine["description"], "Procedure with polymorphic argument (CLASS)\n\n")
        expected = {"type": "shape",
                    "description": "The shape",
                    "dimension": None,
                    "interface_name": None,
                    "enum_type": None,
                    "kind": None,
                    "length": None,
                    "attributes": [],
                    "default_value": None,
                    "polymorphism_type": PolymorphismType.LIMITED,
                    "type_params": None
                    }
        self.assertEqual(subroutine["arguments"], ["s"])
        self.assertEqual(len(subroutine["in"]), 1)
        self.assertEqual(subroutine["in"]["s"], expected)
        self.assertEqual(subroutine["out"]["s"], expected)
        subroutine = module["subroutines"]["process_shape_monomorphic"]
        self.assertEqual(subroutine["description"], "Procedure with non-polymorphic argument\n\n")
        expected = {"type": "shape",
                      "description": "The shape",
                      "dimension": None,
                      "interface_name": None,
                      "enum_type": None,
                      "kind": None,
                      "length": None,
                      "attributes": [],
                      "default_value": None,
                      "polymorphism_type": PolymorphismType.NONE,
                      "type_params": None
                      }
        self.assertEqual(subroutine["arguments"], ["s"])
        self.assertEqual(len(subroutine["in"]), 1)
        self.assertEqual(subroutine["in"]["s"], expected)
        self.assertEqual(subroutine["out"]["s"], expected)
        subroutine = module["subroutines"]["process_real_value"]
        self.assertEqual(subroutine["description"], "Procedure with intrinsic type argument\n\n")
        expected = {"type": "REAL",
                    "description": "The real value",
                    "dimension": None,
                    "interface_name": None,
                    "enum_type": None,
                    "kind": None,
                    "length": None,
                    "attributes": [],
                    "default_value": None,
                    "polymorphism_type": PolymorphismType.NONE,
                    "type_params": None
                    }
        self.assertEqual(subroutine["arguments"], ["x"])
        self.assertEqual(len(subroutine["in"]), 1)
        self.assertEqual(subroutine["in"]["x"], expected)
        self.assertFalse(subroutine["out"])
        subroutine = module["subroutines"]["calculate_area"]
        self.assertEqual(subroutine["description"], "\n\n")
        expected = {"type": "shape",
                    "description": "The shape",
                    "dimension": None,
                    "interface_name": None,
                    "enum_type": None,
                    "kind": None,
                    "length": None,
                    "attributes": [],
                    "default_value": None,
                    "polymorphism_type": PolymorphismType.LIMITED,
                    "type_params": None
                    }
        self.assertEqual(subroutine["arguments"], ["this"])
        self.assertEqual(len(subroutine["in"]), 1)
        self.assertEqual(subroutine["in"]["this"], expected)
        self.assertEqual(subroutine["out"]["this"], expected)
        subroutine = module["subroutines"]["circle_area"]
        self.assertEqual(subroutine["description"], "\n\n")
        expected = {"type": "circle",
                    "description": "",
                    "dimension": None,
                    "interface_name": None,
                    "enum_type": None,
                    "kind": None,
                    "length": None,
                    "attributes": [],
                    "default_value": None,
                    "polymorphism_type": PolymorphismType.LIMITED,
                    "type_params": None
                    }
        self.assertEqual(subroutine["arguments"], ["this"])
        self.assertEqual(len(subroutine["in"]), 1)
        self.assertEqual(subroutine["in"]["this"], expected)
        self.assertEqual(subroutine["out"]["this"], expected)
        subroutine = module["subroutines"]["rectangle_area"]
        self.assertEqual(subroutine["description"], "")
        expected = {"type": "rectangle",
                    "description": "",
                    "dimension": None,
                    "interface_name": None,
                    "enum_type": None,
                    "kind": None,
                    "length": None,
                    "attributes": [],
                    "default_value": None,
                    "polymorphism_type": PolymorphismType.LIMITED,
                    "type_params": None
                    }
        self.assertEqual(subroutine["arguments"], ["this"])
        self.assertEqual(len(subroutine["in"]), 1)
        self.assertEqual(subroutine["in"]["this"], expected)
        self.assertEqual(subroutine["out"]["this"], expected)

    def test_subroutine_with_c_bindings(self):
        self.fs.create_file(
            "/fake/path/c_interface.f90",
            contents="""\
    module c_interface_module
        use iso_c_binding
        implicit none
        
        interface
            ! C function that takes various argument types with C binding
            subroutine process_data_c(n, data, callback) bind(c, name="process_data")
                import :: c_int, c_double, c_funptr
                integer(c_int), value, intent(in) :: n
                real(c_double), intent(inout) :: data(*)
                type(c_funptr), value, intent(in) :: callback
            end subroutine process_data_c
            
            ! Another C function with different binding name
            function calculate_sum_c(arr, size) bind(c, name="calc_sum") result(sum_val)
                import :: c_int, c_double
                integer(c_int), value, intent(in) :: size
                real(c_double), intent(in) :: arr(size)
                real(c_double) :: sum_val
            end function calculate_sum_c
        end interface

    contains
        
        ! Fortran procedure that calls C functions
        subroutine fortran_wrapper()
            integer(c_int) :: n = 10
            real(c_double) :: my_data(10) = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0]
            type(c_funptr) :: my_callback
            real(c_double) :: result
            
            ! Call C function
            call process_data_c(n, my_data, my_callback)
            result = calculate_sum_c(my_data, n)
        end subroutine fortran_wrapper
        
    end module c_interface_module
                            """,
        )
        result = extract_module_data([Path("/fake/path/c_interface.f90")])

        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(module["module_name"], "c_interface_module")
        self.assertEqual(len(module["interfaces"]), 1)
        self.assertEqual(len(module["subroutines"]), 1)
        self.assertIn("fortran_wrapper", module["subroutines"])

        interface = module["interfaces"][0]
        # Test process_data_c subroutine
        subroutine = interface["procedures"]["process_data_c"]
        self.assertEqual(subroutine["binding_type"], {"type": BindingTypeEnum.BIND_C, "name": "process_data"})
        self.assertEqual(subroutine["arguments"], ["n", "data", "callback"])
        self.assertEqual(len(subroutine["in"]), 3)

        # Check 'n' parameter
        expected_n = {"type": "INTEGER",
                      "description": "",
                      "dimension": None,
                      "interface_name": None,
                      "enum_type": None,
                      "kind": "c_int",
                      "length": None,
                      "attributes": ["VALUE"],
                      "default_value": None,
                      "polymorphism_type": PolymorphismType.NONE,
                      "type_params": None
                      }
        self.assertEqual(subroutine["in"]["n"], expected_n)

        # Check 'data' parameter
        expected_data = {"type": "REAL",
                        "description": "",
                        "dimension": {"dimensions": [ArrayBound(BoundType.ASSUMED_SIZE)]},
                        "interface_name": None,
                        "enum_type": None,
                        "kind": "c_double",
                        "length": None,
                        "attributes": [],
                        "default_value": None,
                        "polymorphism_type": PolymorphismType.NONE,
                        "type_params": None
                        }
        self.assertEqual(subroutine["in"]["data"], expected_data)
        self.assertEqual(subroutine["out"]["data"], expected_data)

        # Check 'callback' parameter
        expected_callback = {"type": "c_funptr",
                            "description": "",
                            "dimension": None,
                            "interface_name": None,
                            "enum_type": None,
                            "kind": None,
                            "length": None,
                            "attributes": ["VALUE"],
                            "default_value": None,
                            "polymorphism_type": PolymorphismType.NONE,
                            "type_params": None
                            }
        self.assertEqual(subroutine["in"]["callback"], expected_callback)

        # Test calculate_sum_c function
        function = interface["procedures"]["calculate_sum_c"]
        self.assertEqual(function["binding_type"], {"type": BindingTypeEnum.BIND_C, "name": "calc_sum"})
        self.assertEqual(function["arguments"], ["arr", "size"])
        self.assertEqual(len(function["in"]), 2)

        # Check 'size' parameter
        expected_size = {"type": "INTEGER",
                        "description": "",
                        "dimension": None,
                        "interface_name": None,
                        "enum_type": None,
                        "kind": "c_int",
                        "length": None,
                        "attributes": ["VALUE"],
                        "default_value": None,
                        "polymorphism_type": PolymorphismType.NONE,
                        "type_params": None
                        }
        self.assertEqual(function["in"]["size"], expected_size)

        # Check 'arr' parameter
        expected_arr = {"type": "REAL",
                        "description": "",
                        "dimension": {"dimensions": [ArrayBound(BoundType.VARIABLE, 
                                                                Expression(ExpressionType.LITERAL, "1"),
                                                                Expression(ExpressionType.VARIABLE, "size"))]},
                        "interface_name": None,
                        "enum_type": None,
                        "kind": "c_double",
                        "length": None,
                        "attributes": [],
                        "default_value": None,
                        "polymorphism_type": PolymorphismType.NONE,
                        "type_params": None
                        }
        self.assertEqual(function["in"]["arr"], expected_arr)

        # Test fortran_wrapper subroutine
        wrapper = module["subroutines"]["fortran_wrapper"]
        self.assertEqual(wrapper["arguments"], [])
        self.assertEqual(len(wrapper["in"]), 0)
        self.assertEqual(len(wrapper["out"]), 0)

if __name__ == "__main__":
    unittest.main()