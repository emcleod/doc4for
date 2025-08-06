import unittest
from pathlib import Path
from pyfakefs.fake_filesystem_unittest import TestCase
from doc4for.f90.generate_module_tree import extract_module_data

class TestGenericInterfacesAndOperators(TestCase):
    def setUp(self):
        self.setUpPyfakefs()

    def test_multiple_generic_interfaces(self):
        self.fs.create_file(
            "/fake/path/generic_interfaces.f90",
            contents="""\
module generic_interfaces_mod
    implicit none
    private
    public :: matrix_type

    !!* A matrix type with multiple generic interfaces *!
    type :: matrix_type
        private
        real, allocatable :: data(:,:)
        integer :: rows = 0
        integer :: cols = 0
    contains
        !!* Generic interface for initialization *!
        generic :: init => init_scalar, init_array, init_from_matrix
        procedure, private :: init_scalar
        procedure, private :: init_array
        procedure, private :: init_from_matrix
        
        !!* Multiple operator overloads *!
        generic :: operator(+) => add_matrix_matrix, add_matrix_scalar
        generic :: operator(-) => subtract_matrix_matrix, negate_matrix
        generic :: operator(*) => multiply_matrix_matrix, multiply_matrix_scalar
        generic :: operator(/) => divide_matrix_scalar
        generic :: operator(**) => power_matrix_scalar
        
        !!* Comparison operators *!
        generic :: operator(==) => equal_matrix_matrix
        generic :: operator(/=) => not_equal_matrix_matrix
        
        !!* Assignment overloading *!
        generic :: assignment(=) => assign_from_scalar, assign_from_array
        
        !!* User-defined operators *!
        generic :: operator(.dot.) => dot_product_matrix
        generic :: operator(.cross.) => cross_product_matrix
        generic :: operator(.transpose.) => transpose_matrix
        
        !!* Read/write interfaces *!
        generic :: read(formatted) => read_formatted
        generic :: write(formatted) => write_formatted
        generic :: read(unformatted) => read_unformatted
        generic :: write(unformatted) => write_unformatted
        
        !!* Multiple specific procedures per generic *!
        generic :: transform => transform_function, transform_subroutine, transform_inplace
        
        ! All the specific procedures
        procedure, private :: add_matrix_matrix
        procedure, private :: add_matrix_scalar
        procedure, private :: subtract_matrix_matrix
        procedure, private :: negate_matrix
        procedure, private :: multiply_matrix_matrix
        procedure, private :: multiply_matrix_scalar
        procedure, private :: divide_matrix_scalar
        procedure, private :: power_matrix_scalar
        procedure, private :: equal_matrix_matrix
        procedure, private :: not_equal_matrix_matrix
        procedure, private :: assign_from_scalar
        procedure, private :: assign_from_array
        procedure, private :: dot_product_matrix
        procedure, private :: cross_product_matrix
        procedure, private :: transpose_matrix
        procedure, private :: read_formatted
        procedure, private :: write_formatted
        procedure, private :: read_unformatted
        procedure, private :: write_unformatted
        procedure, private :: transform_function
        procedure, private :: transform_subroutine
        procedure, private :: transform_inplace
    end type matrix_type

end module generic_interfaces_mod
""",
        )
        result = extract_module_data([Path("/fake/path/generic_interfaces.f90")])
        module = result[0]
        
        matrix_type = module["types"]["matrix_type"]
        generics = matrix_type["generic_interfaces"]
        
        # Test initialization generic
        self.assertIn("init", generics)
        init_generic = generics["init"]
        self.assertEqual(init_generic["generic_spec"], "init")
        self.assertCountEqual(init_generic["specific_procedures"], 
                             ["init_scalar", "init_array", "init_from_matrix"])
        self.assertIn("PUBLIC", init_generic["attributes"])
        
        # Test arithmetic operators
        self.assertIn("operator(+)", generics)
        self.assertCountEqual(generics["operator(+)"]["specific_procedures"],
                             ["add_matrix_matrix", "add_matrix_scalar"])
        
        self.assertIn("operator(-)", generics)
        self.assertCountEqual(generics["operator(-)"]["specific_procedures"],
                             ["subtract_matrix_matrix", "negate_matrix"])
        
        self.assertIn("operator(**)", generics)
        self.assertCountEqual(generics["operator(**)"]["specific_procedures"],
                             ["power_matrix_scalar"])
        
        # Test comparison operators
        self.assertIn("operator(==)", generics)
        self.assertIn("operator(/=)", generics)
        
        # Test assignment
        self.assertIn("assignment(=)", generics)
        self.assertCountEqual(generics["assignment(=)"]["specific_procedures"],
                             ["assign_from_scalar", "assign_from_array"])
        
        # Test user-defined operators
        self.assertIn("operator(.dot.)", generics)
        self.assertIn("operator(.cross.)", generics)
        self.assertIn("operator(.transpose.)", generics)
        
        # Test I/O interfaces
        self.assertIn("read(formatted)", generics)
        self.assertIn("write(formatted)", generics)
        self.assertIn("read(unformatted)", generics)
        self.assertIn("write(unformatted)", generics)
        
        # Test multiple procedures per generic
        self.assertIn("transform", generics)
        self.assertCountEqual(generics["transform"]["specific_procedures"],
                             ["transform_function", "transform_subroutine", "transform_inplace"])
        
        # Verify all specific procedures are private
        for proc_name in matrix_type["procedures"]:
            if not proc_name.startswith("operator") and proc_name not in ["init", "transform", "assignment", "read", "write"]:
                self.assertIn("PRIVATE", matrix_type["procedures"][proc_name]["attributes"])

    def test_generic_interface_with_mixed_access(self):
        self.fs.create_file(
            "/fake/path/mixed_access_generics.f90",
            contents="""\
module mixed_access_generics_mod
    implicit none

    type :: data_handler
        real :: value
    contains
        ! Public generic with private specifics
        generic, public :: process => process_real, process_integer, process_logical
        procedure, private :: process_real
        procedure, private :: process_integer  
        procedure, private :: process_logical
        
        ! Private generic with mixed specific access (unusual but legal)
        generic, private :: internal_op => public_impl, private_impl
        procedure, public :: public_impl
        procedure, private :: private_impl
        
        ! Public generic with public specifics
        generic, public :: convert => to_string, to_integer, to_real
        procedure, public :: to_string
        procedure, public :: to_integer
        procedure, public :: to_real
        
        ! Generic without explicit access (inherits default)
        generic :: operator(.custom.) => custom_op1, custom_op2
        procedure :: custom_op1
        procedure :: custom_op2
    end type data_handler

contains
    ! Implementation stubs would go here
    
end module mixed_access_generics_mod
""",
        )
        result = extract_module_data([Path("/fake/path/mixed_access_generics.f90")])
        module = result[0]
        
        handler_type = module["types"]["data_handler"]
        generics = handler_type["generic_interfaces"]
        procedures = handler_type["procedures"]
        
        # Test public generic with private specifics
        self.assertIn("PUBLIC", generics["process"]["attributes"])
        self.assertIn("PRIVATE", procedures["process_real"]["attributes"])
        self.assertIn("PRIVATE", procedures["process_integer"]["attributes"])
        self.assertIn("PRIVATE", procedures["process_logical"]["attributes"])
        
        # Test private generic with mixed specific access
        self.assertIn("PRIVATE", generics["internal_op"]["attributes"])
        self.assertIn("PUBLIC", procedures["public_impl"]["attributes"])
        self.assertIn("PRIVATE", procedures["private_impl"]["attributes"])
        
        # Test public generic with public specifics
        self.assertIn("PUBLIC", generics["convert"]["attributes"])
        self.assertIn("PUBLIC", procedures["to_string"]["attributes"])
        self.assertIn("PUBLIC", procedures["to_integer"]["attributes"])
        self.assertIn("PUBLIC", procedures["to_real"]["attributes"])
        
        # Test generic without explicit access
        self.assertIn("PUBLIC", generics["operator(.custom.)"]["attributes"])  # Default
        self.assertIn("PUBLIC", procedures["custom_op1"]["attributes"])
        self.assertIn("PUBLIC", procedures["custom_op2"]["attributes"])

    def test_supported_generic_interfaces(self):
        """Test generic interfaces that fparser can parse"""
        self.fs.create_file(
            "/fake/path/supported_generics.f90",
            contents="""\
    !!*
    ! Module demonstrating generic interfaces
    ! Shows operator overloading and named generic procedures
    !*!
    module supported_generics_mod
        implicit none
        
        !!*
        ! Mathematical type with overloaded operators
        ! Demonstrates comprehensive operator overloading capabilities
        !*!
        type :: math_type
            !!* The numeric value stored *!
            real :: value
        contains
            ! Standard operators
            !!* Addition operator for scalar and type *!
            generic :: operator(+) => add_scalar, add_type
            !!* Subtraction operator for scalar and type *!
            generic :: operator(-) => subtract_scalar, subtract_type
            !!* Multiplication operator for scalar and type *!
            generic :: operator(*) => multiply_scalar, multiply_type
            !!* Division operator for scalar and type *!
            generic :: operator(/) => divide_scalar, divide_type
            !!* Power operator for scalar exponent *!
            generic :: operator(**) => power_scalar
            
            ! Comparison operators
            !!* Equality comparison *!
            generic :: operator(==) => equal_type
            !!* Inequality comparison *!
            generic :: operator(/=) => not_equal_type
            !!* Less than comparison *!
            generic :: operator(<) => less_than_type
            !!* Less than or equal comparison *!
            generic :: operator(<=) => less_equal_type
            !!* Greater than comparison *!
            generic :: operator(>) => greater_than_type
            !!* Greater than or equal comparison *!
            generic :: operator(>=) => greater_equal_type
            
            ! User-defined operators
            !!* Dot product operator *!
            generic :: operator(.dot.) => dot_product_type
            !!* Cross product operator *!
            generic :: operator(.cross.) => cross_product_type
            
            ! Named generics
            !!* Named addition interface *!
            generic :: add => add_scalar, add_type
            !!* Named scaling interface *!
            generic :: scale => multiply_scalar
            
            ! All the specific procedures - each needs its own statement
            !!* Add scalar to type *!
            procedure, private :: add_scalar
            !!* Add two types *!
            procedure, private :: add_type
            !!* Subtract scalar from type *!
            procedure, private :: subtract_scalar
            !!* Subtract two types *!
            procedure, private :: subtract_type
            !!* Multiply type by scalar *!
            procedure, private :: multiply_scalar
            !!* Multiply two types *!
            procedure, private :: multiply_type
            !!* Divide type by scalar *!
            procedure, private :: divide_scalar
            !!* Divide two types *!
            procedure, private :: divide_type
            !!* Raise type to scalar power *!
            procedure, private :: power_scalar
            !!* Test equality of two types *!
            procedure, private :: equal_type
            !!* Test inequality of two types *!
            procedure, private :: not_equal_type
            !!* Test if first type is less than second *!
            procedure, private :: less_than_type
            !!* Test if first type is less than or equal to second *!
            procedure, private :: less_equal_type
            !!* Test if first type is greater than second *!
            procedure, private :: greater_than_type
            !!* Test if first type is greater than or equal to second *!
            procedure, private :: greater_equal_type
            !!* Compute dot product of two types *!
            procedure, private :: dot_product_type
            !!* Compute cross product of two types *!
            procedure, private :: cross_product_type
        end type math_type
        
    end module supported_generics_mod
    """,
        )
        result = extract_module_data([Path("/fake/path/supported_generics.f90")])
        module = result[0]
        
        # Check module description
        self.assertEqual(module["module_description"],
                        "Module demonstrating generic interfaces\n"
                        "Shows operator overloading and named generic procedures\n")
        
        math_type = module["types"]["math_type"]
        
        # Check type description
        self.assertEqual(math_type["description"],
                        "Mathematical type with overloaded operators\n"
                        "Demonstrates comprehensive operator overloading capabilities\n")
        
        # Check value component
        self.assertEqual(math_type["data_components"]["value"]["description"],
                        "The numeric value stored\n")
        
        generics = math_type["generic_interfaces"]
        
        # Test arithmetic operators with descriptions
        self.assertIn("operator(+)", generics)
        self.assertCountEqual(generics["operator(+)"]["specific_procedures"], ["add_scalar", "add_type"])
        self.assertEqual(generics["operator(+)"]["description"],
                        "Addition operator for scalar and type\n")
        
        self.assertIn("operator(**)", generics)
        self.assertCountEqual(generics["operator(**)"]["specific_procedures"], ["power_scalar"])
        self.assertEqual(generics["operator(**)"]["description"],
                        "Power operator for scalar exponent\n")
        
        # Test subtraction
        self.assertIn("operator(-)", generics)
        self.assertEqual(generics["operator(-)"]["description"],
                        "Subtraction operator for scalar and type\n")
        
        # Test multiplication
        self.assertIn("operator(*)", generics)
        self.assertEqual(generics["operator(*)"]["description"],
                        "Multiplication operator for scalar and type\n")
        
        # Test division
        self.assertIn("operator(/)", generics)
        self.assertEqual(generics["operator(/)"]["description"],
                        "Division operator for scalar and type\n")
        
        # Test comparison operators with descriptions
        self.assertIn("operator(==)", generics)
        self.assertEqual(generics["operator(==)"]["description"],
                        "Equality comparison\n")
        
        self.assertIn("operator(/=)", generics)
        self.assertEqual(generics["operator(/=)"]["description"],
                        "Inequality comparison\n")
        
        self.assertIn("operator(<)", generics)
        self.assertEqual(generics["operator(<)"]["description"],
                        "Less than comparison\n")
        
        self.assertIn("operator(<=)", generics)
        self.assertEqual(generics["operator(<=)"]["description"],
                        "Less than or equal comparison\n")
        
        self.assertIn("operator(>)", generics)
        self.assertEqual(generics["operator(>)"]["description"],
                        "Greater than comparison\n")
        
        self.assertIn("operator(>=)", generics)
        self.assertEqual(generics["operator(>=)"]["description"],
                        "Greater than or equal comparison\n")
        
        # Test user-defined operators with descriptions
        self.assertIn("operator(.dot.)", generics)
        self.assertEqual(generics["operator(.dot.)"]["description"],
                        "Dot product operator\n")
        
        self.assertIn("operator(.cross.)", generics)
        self.assertEqual(generics["operator(.cross.)"]["description"],
                        "Cross product operator\n")
        
        # Test named generics with descriptions
        self.assertIn("add", generics)
        self.assertCountEqual(generics["add"]["specific_procedures"], ["add_scalar", "add_type"])
        self.assertEqual(generics["add"]["description"],
                        "Named addition interface\n")
        
        self.assertIn("scale", generics)
        self.assertCountEqual(generics["scale"]["specific_procedures"], ["multiply_scalar"])
        self.assertEqual(generics["scale"]["description"],
                        "Named scaling interface\n")
        
        # Test specific procedure descriptions
        procedures = math_type["procedures"]
        
        self.assertEqual(procedures["add_scalar"]["description"],
                        "Add scalar to type\n")
        self.assertEqual(procedures["add_type"]["description"],
                        "Add two types\n")
        self.assertEqual(procedures["subtract_scalar"]["description"],
                        "Subtract scalar from type\n")
        self.assertEqual(procedures["multiply_scalar"]["description"],
                        "Multiply type by scalar\n")
        self.assertEqual(procedures["power_scalar"]["description"],
                        "Raise type to scalar power\n")
        self.assertEqual(procedures["equal_type"]["description"],
                        "Test equality of two types\n")
        self.assertEqual(procedures["dot_product_type"]["description"],
                        "Compute dot product of two types\n")
        self.assertEqual(procedures["cross_product_type"]["description"],
                        "Compute cross product of two types\n")
        
        # Verify all procedures are private
        for proc_name, proc_info in procedures.items():
            self.assertIn("PRIVATE", proc_info["attributes"],
                        f"Procedure {proc_name} should be private")
        

if __name__ == "__main__":
    unittest.main()
