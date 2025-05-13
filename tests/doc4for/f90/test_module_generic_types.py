import unittest
from pathlib import Path
from pyfakefs.fake_filesystem_unittest import TestCase
from doc4for.f90.generate_module_tree import extract_module_data

class TestGenericTypes(TestCase):
    def setUp(self):
        self.setUpPyfakefs()

    def test_type_with_generic_procedure(self):
        self.fs.create_file(
            "/fake/path/types.f90",
            contents="""\
module vector_ops
    implicit none
    private
    public :: vector

    type :: vector
        real :: x, y, z
    contains
        procedure, public :: add_vector
        procedure, public :: add_scalar
        generic, public :: operator(+) => add_vector, add_scalar
    end type vector  
end module vector_ops
""",
        )
        result = extract_module_data([Path("/fake/path/types.f90")])
        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(module["module_name"], "vector_ops")
        types = module["types"]
        self.assertEqual(len(types), 1)
        type = types["vector"]
        self.assertEqual(type["type_name"], "vector")
        self.assertEqual(len(type["procedures"]), 2)
        self.assertEqual(len(type["generic_interfaces"]), 1)

        add_vector = type["procedures"]["add_vector"]
        self.assertEqual(add_vector["name"], "add_vector")
        self.assertEqual(add_vector["attributes"], ["PUBLIC"])
        self.assertFalse(add_vector["is_final"])

        add_scalar = type["procedures"]["add_scalar"]
        self.assertEqual(add_scalar["name"], "add_scalar")
        self.assertEqual(add_scalar["attributes"], ["PUBLIC"])
        self.assertFalse(add_scalar["is_final"])

        plus_operator = type["generic_interfaces"]["operator(+)"]
        self.assertEqual(plus_operator["generic_spec"], "operator(+)")
        self.assertEqual(plus_operator["description"], "")
        self.assertCountEqual(plus_operator["specific_procedures"], ["add_vector", "add_scalar"])

    def test_type_with_multiple_generic_procedures(self):
        self.fs.create_file(
            "/fake/path/types.f90",
            contents="""\
module vector_ops
    implicit none
    private
    public :: vector

    type :: vector
        real :: x, y, z
    contains
        procedure, public :: add_vector
        procedure, public :: add_scalar
        generic, public :: operator(+) => add_vector, add_scalar
        procedure, public :: print_vector
        generic, public :: write(formatted) => print_vector
    end type vector  
end module vector_ops
""",
        )
        result = extract_module_data([Path("/fake/path/types.f90")])
        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(module["module_name"], "vector_ops")
        types = module["types"]
        self.assertEqual(len(types), 1)
        type = types["vector"]
        self.assertEqual(type["type_name"], "vector")
        self.assertEqual(len(type["procedures"]), 3)

        add_vector = type["procedures"]["add_vector"]
        self.assertEqual(add_vector["name"], "add_vector")
        self.assertEqual(add_vector["attributes"], ["PUBLIC"])
        self.assertFalse(add_vector["is_final"])

        add_scalar = type["procedures"]["add_scalar"]
        self.assertEqual(add_scalar["name"], "add_scalar")
        self.assertEqual(add_scalar["attributes"], ["PUBLIC"])
        self.assertFalse(add_scalar["is_final"])

        print_vector = type["procedures"]["print_vector"]
        self.assertEqual(print_vector["name"], "print_vector")
        self.assertEqual(print_vector["attributes"], ["PUBLIC"])
        self.assertFalse(print_vector["is_final"])
        self.assertEqual(len(type["generic_interfaces"]), 2)

        plus_operator = type["generic_interfaces"]["operator(+)"]
        self.assertEqual(plus_operator["generic_spec"], "operator(+)")
        self.assertEqual(plus_operator["description"], "")
        self.assertCountEqual(plus_operator["specific_procedures"], ["add_vector", "add_scalar"])

        write_operator = type["generic_interfaces"]["write(formatted)"]
        self.assertEqual(write_operator["generic_spec"], "write(formatted)")
        self.assertEqual(write_operator["description"], "")
        self.assertCountEqual(write_operator["specific_procedures"], ["print_vector"])

    def test_type_with_multiple_generic_procedures_and_comments(self):
        self.fs.create_file(
            "/fake/path/types.f90",
            contents="""\
module vector_ops
    implicit none
    private
    public :: vector

    type :: vector
        real :: x, y, z
    contains
        !!* Adds two vectors *!
        procedure, public :: add_vector
        !!* Adds two scalars *!
        procedure, public :: add_scalar
        !!* Overloads the plus operator to handle vectors and scalars *!
        generic, public :: operator(+) => add_vector, add_scalar
        !!* Prints a vector *!
        procedure, public :: print_vector
        !!* Overloads the write() operator *!
        generic, public :: write(formatted) => print_vector
    end type vector  
end module vector_ops
""",
        )
        result = extract_module_data([Path("/fake/path/types.f90")])
        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(module["module_name"], "vector_ops")
        types = module["types"]
        self.assertEqual(len(types), 1)
        type = types["vector"]
        self.assertEqual(type["type_name"], "vector")
        self.assertEqual(len(type["procedures"]), 3)
        self.assertEqual(len(type["generic_interfaces"]), 2)

        add_vector = type["procedures"]["add_vector"]
        self.assertEqual(add_vector["name"], "add_vector")
        self.assertEqual(add_vector["attributes"], ["PUBLIC"])
        self.assertEqual(add_vector["description"], "Adds two vectors\n")
        self.assertFalse(add_vector["is_final"])

        add_scalar = type["procedures"]["add_scalar"]
        self.assertEqual(add_scalar["name"], "add_scalar")
        self.assertEqual(add_scalar["attributes"], ["PUBLIC"])
        self.assertEqual(add_scalar["description"], "Adds two scalars\n")
        self.assertFalse(add_scalar["is_final"])

        print_vector = type["procedures"]["print_vector"]
        self.assertEqual(print_vector["name"], "print_vector")
        self.assertEqual(print_vector["attributes"], ["PUBLIC"])
        self.assertEqual(print_vector["description"], "Prints a vector\n")
        self.assertFalse(print_vector["is_final"])
        
        plus_operator = type["generic_interfaces"]["operator(+)"]
        self.assertEqual(plus_operator["generic_spec"], "operator(+)")
        self.assertEqual(plus_operator["description"], "Overloads the plus operator to handle vectors and scalars\n")
        self.assertCountEqual(plus_operator["specific_procedures"], ["add_vector", "add_scalar"])
        
        write_operator = type["generic_interfaces"]["write(formatted)"]
        self.assertEqual(write_operator["generic_spec"], "write(formatted)")
        self.assertEqual(write_operator["description"], "Overloads the write() operator\n")
        self.assertCountEqual(write_operator["specific_procedures"], ["print_vector"])

    def test_type_with_generic_assignment(self):
        self.fs.create_file(
            "/fake/path/types.f90",
            contents="""\
    module vector_ops
        implicit none
        private
        public :: vector

        type :: vector
            real :: x, y, z
        contains
            procedure, public :: assign_vector
            procedure, public :: assign_array
            generic, public :: assignment(=) => assign_vector, assign_array
        end type vector  
    end module vector_ops
    """,
        )
        result = extract_module_data([Path("/fake/path/types.f90")])
        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(module["module_name"], "vector_ops")
        types = module["types"]
        self.assertEqual(len(types), 1)
        type = types["vector"]
        self.assertEqual(type["type_name"], "vector")
        self.assertEqual(len(type["procedures"]), 2)
        self.assertEqual(len(type["generic_interfaces"]), 1)

        assign_vector = type["procedures"]["assign_vector"]
        self.assertEqual(assign_vector["name"], "assign_vector")
        self.assertEqual(assign_vector["attributes"], ["PUBLIC"])
        self.assertFalse(assign_vector["is_final"])
        
        assign_array = type["procedures"]["assign_array"]
        self.assertEqual(assign_array["name"], "assign_array")
        self.assertEqual(assign_array["attributes"], ["PUBLIC"])
        self.assertFalse(assign_array["is_final"])
        
        assignment_operator = type["generic_interfaces"]["assignment(=)"]
        self.assertEqual(assignment_operator["generic_spec"], "assignment(=)")
        self.assertEqual(assignment_operator["description"], "")
        self.assertCountEqual(assignment_operator["specific_procedures"], ["assign_vector", "assign_array"])

    def test_type_with_multiple_generic_statements_same_spec(self):
        self.fs.create_file(
            "/fake/path/types.f90",
            contents="""\
    module vector_ops
        implicit none
        private
        public :: vector

        type :: vector
            real :: x, y, z
        contains
            !!* Adds two vectors *!
            procedure, public :: add_vector
            !!* Adds two scalars *!
            procedure, public :: add_scalar
            !!* Overloads the + operator for vectors *!
            generic, public :: operator(+) => add_vector
            !!* Overloads the + operator for scalars *!
            generic, public :: operator(+) => add_scalar
        end type vector  
    end module vector_ops
    """,
        )
        result = extract_module_data([Path("/fake/path/types.f90")])
        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(module["module_name"], "vector_ops")
        types = module["types"]
        self.assertEqual(len(types), 1)
        type = types["vector"]
        self.assertEqual(type["type_name"], "vector")
        self.assertEqual(len(type["procedures"]), 2)
        self.assertEqual(len(type["generic_interfaces"]), 1)

        add_vector = type["procedures"]["add_vector"]
        self.assertEqual(add_vector["name"], "add_vector")
        self.assertEqual(add_vector["description"], "Adds two vectors\n")
        self.assertEqual(add_vector["attributes"], ["PUBLIC"])
        self.assertFalse(add_vector["is_final"])

        add_scalar = type["procedures"]["add_scalar"]
        self.assertEqual(add_scalar["name"], "add_scalar")
        self.assertEqual(add_scalar["description"], "Adds two scalars\n")
        self.assertEqual(add_scalar["attributes"], ["PUBLIC"])
        self.assertFalse(add_scalar["is_final"])

        plus_operator = type["generic_interfaces"]["operator(+)"]
        self.assertEqual(plus_operator["generic_spec"], "operator(+)")
        self.assertEqual(plus_operator["description"], "Overloads the + operator for vectors\nOverloads the + operator for scalars\n")
        self.assertCountEqual(plus_operator["specific_procedures"], ["add_vector", "add_scalar"])

    def test_type_with_private_generic(self):
        self.fs.create_file(
            "/fake/path/types.f90",
            contents="""\
    module vector_ops
        implicit none
        private
        public :: vector

        type :: vector
            real :: x, y, z
        contains
            procedure, private :: add_vector_private
            generic, private :: operator(+) => add_vector_private
        end type vector  
    end module vector_ops
    """,
        )
        result = extract_module_data([Path("/fake/path/types.f90")])
        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(module["module_name"], "vector_ops")
        types = module["types"]
        self.assertEqual(len(types), 1)
        type = types["vector"]
        self.assertEqual(type["type_name"], "vector")
        self.assertEqual(len(type["procedures"]), 1)
        self.assertEqual(len(type["generic_interfaces"]), 1)

        add_vector_private = type["procedures"]["add_vector_private"]
        self.assertEqual(add_vector_private["name"], "add_vector_private")
        self.assertEqual(add_vector_private["attributes"], ["PRIVATE"])
        self.assertFalse(add_vector_private["is_final"])
        
        plus_operator = type["generic_interfaces"]["operator(+)"]
        self.assertEqual(plus_operator["generic_spec"], "operator(+)")
        self.assertEqual(plus_operator["description"], "")
        self.assertEqual(plus_operator["attributes"], ["PRIVATE"])
        self.assertCountEqual(plus_operator["specific_procedures"], ["add_vector_private"])

    def test_type_with_generic_spec_with_spaces(self):
        self.fs.create_file(
            "/fake/path/types.f90",
            contents="""\
    module vector_ops
        implicit none
        private
        public :: vector

        type :: vector
            real :: x, y, z
        contains
            procedure, public :: add_vector
            generic, public :: operator ( + ) => add_vector
        end type vector  
    end module vector_ops
    """,
        )
        result = extract_module_data([Path("/fake/path/types.f90")])
        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(module["module_name"], "vector_ops")
        types = module["types"]
        self.assertEqual(len(types), 1)
        type = types["vector"]
        self.assertEqual(type["type_name"], "vector")
        self.assertEqual(len(type["procedures"]), 1)
        self.assertEqual(len(type["generic_interfaces"]), 1)

        add_vector = type["procedures"]["add_vector"]
        self.assertEqual(add_vector["name"], "add_vector")
        self.assertEqual(add_vector["attributes"], ["PUBLIC"])
        self.assertFalse(add_vector["is_final"])
        
        plus_operator = type["generic_interfaces"]["operator ( + )"]
        self.assertIn(plus_operator["generic_spec"], "operator ( + )")
        self.assertEqual(plus_operator["description"], "")
        self.assertCountEqual(plus_operator["specific_procedures"], ["add_vector"])

    def test_type_with_generic_user_defined_operator(self):
        self.fs.create_file(
            "/fake/path/types.f90",
            contents="""\
    module vector_ops
        implicit none
        private
        public :: vector

        type :: vector
            real :: x, y, z
        contains
            procedure, public :: cross_product
            generic, public :: operator(.cross.) => cross_product
        end type vector  
    end module vector_ops
    """,
        )
        result = extract_module_data([Path("/fake/path/types.f90")])
        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(module["module_name"], "vector_ops")
        types = module["types"]
        self.assertEqual(len(types), 1)
        type = types["vector"]
        self.assertEqual(type["type_name"], "vector")
        self.assertEqual(len(type["procedures"]), 1)
        self.assertEqual(len(type["generic_interfaces"]), 1)

        cross_product = type["procedures"]["cross_product"]
        self.assertEqual(cross_product["name"], "cross_product")
        self.assertEqual(cross_product["attributes"], ["PUBLIC"])
        self.assertFalse(cross_product["is_final"])

        cross_operator = type["generic_interfaces"]["operator(.cross.)"]
        self.assertEqual(cross_operator["generic_spec"], "operator(.cross.)")
        self.assertEqual(cross_operator["description"], "")
        self.assertCountEqual(cross_operator["specific_procedures"], ["cross_product"])

if __name__ == "__main__":
    unittest.main()
