import unittest
from pathlib import Path
from pyfakefs.fake_filesystem_unittest import TestCase
from doc4for.f90.generate_module_tree import extract_module_data
from doc4for.models.common import BindingTypeEnum

class TestFunctionBindings(TestCase):

    def setUp(self):
        self.setUpPyfakefs()

    def test_function_with_and_without_binding(self):
        self.fs.create_file(
            "/fake/path/c_binding.f90",
            contents="""\
    module c_interface_mod
        use iso_c_binding
        implicit none

        !!* Interface for C interoperable function *!
        interface
            !!* 
            ! Calculate square of a number (C-interoperable)
            ! @in x Value to square
            ! @return Square of input value
            !*!            
            function square_it(x) bind(c, name="c_square") result(y)
                use iso_c_binding
                real(c_double), intent(in) :: x
                real(c_double) :: y
            end function square_it
            !!* 
            ! Do nothing
            ! @in x Input value
            ! @return The input value
            !*!
            function no_op(x) result(y)
                real, intent(in) :: x
                real :: y
            end function no_op
        end interface

        contains
        
        !!* 
        ! Computes the cube of a value with C binding
        ! @in x Input value
        ! @return Cubed value
        !*!
        function cube_it(x) bind(c) result(y)
            use iso_c_binding
            real(c_double), intent(in) :: x
            real(c_double) :: y
            y = x * x * x
        end function cube_it
        
        !!* 
        ! Computes the square root of a value without any binding
        ! @in x Input value
        ! @return Square root
        !*!
        function square_root_it(x) result(y)
            real, intent(in) :: x
            real :: y
            y = sqrt(x)
        end function square_root_it

    end module c_interface_mod
    """
            )
        result = extract_module_data([Path("/fake/path/c_binding.f90")])
        module = result[0]
        
        # Check that binding type is associated with the procedure, not the interface

        # interface = module["interfaces"][0]
        # self.assertNotIn("binding_type", interface)  # Interface shouldn"t have binding type
        
        # Check binding type of a procedure within the interface
        # function = interface["procedures"]["square_it"]
        # self.assertIn("binding_type", function)
        # self.assertEqual(function["binding_type"]["type"], BindingTypeEnum.BIND_C)
        # self.assertEqual(function["binding_type"]["name"], "c_square")
        
        # # Check binding type of a procedure within the interface
        # function = interface["procedures"]["no_op"]
        # self.assertIn("binding_type", function)
        # self.assertIsNone(function["binding_type"])

        # Check binding type of the procedure within the module
        function = module["functions"]["cube_it"]
        self.assertEqual(function["description"], "\nComputes the cube of a value with C binding\n\n")
        self.assertIn("binding_type", function)
        self.assertEqual(function["binding_type"]["type"], BindingTypeEnum.BIND_C)
        self.assertIsNone(function["binding_type"]["name"])
        self.assertEqual(function["in"]["x"]["description"], "Input value")
        self.assertEqual(function["in"]["x"]["type"], "REAL")
        self.assertEqual(function["return"]["description"], "Cubed value")

        # Check binding type of the procedure within the module
        function = module["functions"]["square_root_it"]
        self.assertIn("binding_type", function)
        self.assertIsNone(function["binding_type"])

    def test_binding_capitalization_and_spacing_variations(self):
        self.fs.create_file(
            "/fake/path/binding_variations.f90",
            contents="""\
    module binding_variations
        use iso_c_binding
        implicit none
        
        contains
        
        !!* Test uppercase BIND *!
        function test_uppercase(x) BIND(C) result(y)
            real, intent(in) :: x
            real :: y
        end function

        !!* Test mixed case Bind *!
        function test_mixed_case(x) Bind(C) result(y)
            real, intent(in) :: x
            real :: y
        end function
        
        !!* Test spacing variations *!
        function test_tight_spacing(x) bind(c,name="tight") result(y)
            real, intent(in) :: x
            real :: y
        end function
        
        !!* Test extra spacing *!
        function test_loose_spacing(x) bind  (  c  ,  name  =  "loose"  ) result(y)
            real, intent(in) :: x
            real :: y
        end function
    end module
    """
        )
        result = extract_module_data([Path("/fake/path/binding_variations.f90")])
        module = result[0]
        
        # Test capitalization variations
        uppercase = module["functions"]["test_uppercase"]
        self.assertIn("binding_type", uppercase)
        self.assertEqual(uppercase["binding_type"]["type"], BindingTypeEnum.BIND_C)
        
        mixed_case = module["functions"]["test_mixed_case"]
        self.assertIn("binding_type", mixed_case)
        self.assertEqual(mixed_case["binding_type"]["type"], BindingTypeEnum.BIND_C)
        
        # Test spacing variations
        tight = module["functions"]["test_tight_spacing"]
        self.assertIn("binding_type", tight)
        self.assertEqual(tight["binding_type"]["type"], BindingTypeEnum.BIND_C)
        self.assertEqual(tight["binding_type"]["name"], "tight")
        
        loose = module["functions"]["test_loose_spacing"]
        self.assertIn("binding_type", loose)
        self.assertEqual(loose["binding_type"]["type"], BindingTypeEnum.BIND_C)
        self.assertEqual(loose["binding_type"]["name"], "loose")

    def test_binding_name_variations(self):
        self.fs.create_file(
            "/fake/path/binding_names.f90",
            contents="""\
    module binding_names_mod
        use iso_c_binding
        implicit none

        contains
        
        !!* Single quotes, lowercase *!
        function func1(x) bind(c, name="c_func1") result(y)
            use iso_c_binding
            real(c_double), intent(in) :: x
            real(c_double) :: y
        end function func1
        
        !!* Double quotes, mixed case *!
        function func2(x) bind(c, name="CFunc2") result(y)
            use iso_c_binding
            real(c_double), intent(in) :: x
            real(c_double) :: y
        end function func2
        
        !!* Extra whitespace, uppercase NAME *!
        function func3(x) bind(c, NAME = "c_func3") result(y)
            use iso_c_binding
            real(c_double), intent(in) :: x
            real(c_double) :: y
        end function func3
        
    end module binding_names_mod
    """
        )
        result = extract_module_data([Path("/fake/path/binding_names.f90")])
        module = result[0]
        
        expected_names = {
            "func1": "c_func1",
            "func2": "CFunc2",
            "func3": "c_func3",
        }
        
        for func_name, expected_c_name in expected_names.items():
            function = module["functions"][func_name]
            self.assertIn("binding_type", function)
            self.assertEqual(function["binding_type"]["type"], BindingTypeEnum.BIND_C)
            self.assertEqual(function["binding_type"]["name"], expected_c_name)

    def test_binding_in_subroutines(self):
        self.fs.create_file(
            "/fake/path/binding_subroutines.f90",
            contents="""\
    module binding_subroutines_mod
        use iso_c_binding
        implicit none

        contains
        
        !!* Basic subroutine with binding *!
        subroutine sub1(x) bind(c)
            use iso_c_binding
            real(c_double), intent(inout) :: x
        end subroutine sub1
        
        !!* Subroutine with custom name *!
        subroutine sub2(x, y) bind(c, name="c_modify")
            use iso_c_binding
            real(c_double), intent(in) :: x
            real(c_double), intent(out) :: y
        end subroutine sub2
        
        !!* Subroutine without binding (for contrast) *!
        subroutine sub3(x)
            real, intent(inout) :: x
        end subroutine sub3
    end module binding_subroutines_mod
    """
        )
        result = extract_module_data([Path("/fake/path/binding_subroutines.f90")])
        module = result[0]
        
        # Check sub1 with basic binding
        sub1 = module["subroutines"]["sub1"]
        self.assertIn("binding_type", sub1)
        self.assertEqual(sub1["binding_type"]["type"], BindingTypeEnum.BIND_C)
        self.assertIsNone(sub1["binding_type"]["name"])
        
        # Check sub2 with custom name
        sub2 = module["subroutines"]["sub2"]
        self.assertIn("binding_type", sub2)
        self.assertEqual(sub2["binding_type"]["type"], BindingTypeEnum.BIND_C)
        self.assertEqual(sub2["binding_type"]["name"], "c_modify")
        
        # Check sub3 without binding
        sub3 = module["subroutines"]["sub3"]
        self.assertIn("binding_type", sub3)
        self.assertIsNone(sub3["binding_type"])


    def test_complex_binding_names(self):
        self.fs.create_file(
            "/fake/path/complex_binding_names.f90",
            contents="""\
    module complex_names
        use iso_c_binding
        implicit none
        
        contains
        
        !!* Test double quotes *!
        function test_double_quotes(x) bind(c, name="double_quoted_name") result(y)
            real, intent(in) :: x
            real :: y
        end function
        
        !!* Test with underscores and numbers *!
        function test_complex_name(x) bind(c, name="c_func_123_complex") result(y)
            real, intent(in) :: x
            real :: y
        end function
        
        !!* Test with special characters *!
        subroutine test_special_chars() bind(c, name="__c_special$chars")
            ! Empty subroutine
        end subroutine
    end module
    """
        )
        result = extract_module_data([Path("/fake/path/complex_binding_names.f90")])
        module = result[0]
        
        # Test double quotes
        double_quotes = module["functions"]["test_double_quotes"]
        self.assertIn("binding_type", double_quotes)
        self.assertEqual(double_quotes["binding_type"]["name"], "double_quoted_name")
        
        # Test complex name with underscores and numbers
        complex_name = module["functions"]["test_complex_name"]
        self.assertIn("binding_type", complex_name)
        self.assertEqual(complex_name["binding_type"]["name"], "c_func_123_complex")
        
        # Test special characters
        special_chars = module["subroutines"]["test_special_chars"]
        self.assertIn("binding_type", special_chars)
        self.assertEqual(special_chars["binding_type"]["name"], "__c_special$chars")

    def test_default_binding_type(self):
        self.fs.create_file(
            "/fake/path/default_binding.f90",
            contents="""\
    module default_binding
        implicit none
        
        contains
        
        !!* Function with default binding *!
        function regular_function(x) result(y)
            real, intent(in) :: x
            real :: y
            y = x
        end function
    end module
    """
        )
        result = extract_module_data([Path("/fake/path/default_binding.f90")])
        module = result[0]
        
        # Test default binding
        regular = module["functions"]["regular_function"]
        self.assertIn("binding_type", regular)
        self.assertIsNone(regular["binding_type"])

    def test_complex_binding_scenarios(self):
        self.fs.create_file(
            "/fake/path/complex_bindings.f90",
            contents="""\
    module module1
        use iso_c_binding
        implicit none

        interface
            !!* Interface with mixed binding types *!
            function func_with_binding(x) bind(c, name="c_compute") result(y)
                use iso_c_binding
                real(c_double), intent(in) :: x
                real(c_double) :: y
            end function func_with_binding
            
            function func_without_binding(x) result(y)
                real, intent(in) :: x
                real :: y
            end function func_without_binding
        end interface

    end module module1

    module module2
        use iso_c_binding
        implicit none

        !!* Abstract interface with binding *!
        abstract interface
            subroutine callback_type(x, status) bind(c)
                use iso_c_binding
                real(c_double), intent(inout) :: x
                integer(c_int), intent(out) :: status
            end subroutine callback_type
        end interface

    end module module2
    """
        )
        result = extract_module_data([Path("/fake/path/complex_bindings.f90")])
        self.assertEqual(len(result), 2)
        
        # Module 1 checks
        module1 = next(m for m in result if m["module_name"] == "module1")
        interface = module1["interfaces"][0]
        
        func_with_binding = interface["procedures"]["func_with_binding"]
        self.assertIn("binding_type", func_with_binding)
        self.assertEqual(func_with_binding["binding_type"]["type"], BindingTypeEnum.BIND_C)
        self.assertEqual(func_with_binding["binding_type"]["name"], "c_compute")
        
        func_without_binding = interface["procedures"]["func_without_binding"]
        self.assertIn("binding_type", func_without_binding)
        self.assertEqual(func_without_binding["binding_type"]["type"], BindingTypeEnum.DEFAULT)
        self.assertIsNone(func_without_binding["binding_type"]["name"])
        
        # Module 2 checks
        module2 = next(m for m in result if m["module_name"] == "module2")
        abstract_interface = module2["interfaces"][0]
        
        callback = abstract_interface["procedures"]["callback_type"]
        self.assertIn("binding_type", callback)
        self.assertEqual(callback["binding_type"]["type"], BindingTypeEnum.BIND_C)
        self.assertIsNone(callback["binding_type"]["name"])      

if __name__ == "__main__":
    unittest.main()


