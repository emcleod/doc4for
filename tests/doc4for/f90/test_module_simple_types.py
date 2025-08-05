import unittest
from pathlib import Path
from pyfakefs.fake_filesystem_unittest import TestCase
from doc4for.f90.generate_module_tree import extract_module_data
from doc4for.models.procedure_models import PassType

class TestTypes(TestCase):
    def setUp(self):
        self.setUpPyfakefs()

    def test_single_procedures_no_comments(self):
        self.fs.create_file(
            "/fake/path/types.f90",
            contents="""\
    module types
        implicit none
        private
        public :: simple_type
        type :: simple_type
        contains
            procedure, public :: init
        end type simple_type    
    end module types
""",
        )
        result = extract_module_data([Path("/fake/path/types.f90")])
        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(module["module_name"], "types")
        types = module["types"]
        self.assertEqual(len(types), 1)
        type = types["simple_type"]
        self.assertEqual(type["type_name"], "simple_type")
        self.assertEqual(type["attributes"], ["PUBLIC"])
        procedures = type["procedures"]["init"]
        self.assertEqual(procedures["name"], "init")
        self.assertEqual(procedures["description"], "")
        self.assertEqual(procedures["attributes"], ["PUBLIC"])
        self.assertFalse(procedures["is_final"])
        self.assertEqual(procedures["bound_to"], None)
        self.assertEqual(type["data_components"], {})
        self.assertEqual(type["description"], "")
        self.assertIsNone(type["extends"])
        self.assertEqual(type["generic_interfaces"], {})

    def test_single_procedure_with_comments(self):
        self.fs.create_file(
            "/fake/path/types.f90",
            contents="""\
    module types
        implicit none
        private
        public :: simple_type
        type :: simple_type
        contains
            !!*
            ! A procedure that initialises the type
            !*!
            procedure, public :: init
        end type simple_type    
    end module types
""",
        )
        result = extract_module_data([Path("/fake/path/types.f90")])
        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(module["module_name"], "types")
        types = module["types"]
        self.assertEqual(len(types), 1)
        type = types["simple_type"]
        self.assertEqual(type["type_name"], "simple_type")
        self.assertEqual(type["attributes"], ["PUBLIC"])
        procedures = type["procedures"]["init"]
        self.assertEqual(procedures["name"], "init")
        self.assertEqual(procedures["description"], "A procedure that initialises the type\n")
        self.assertEqual(procedures["attributes"], ["PUBLIC"])
        self.assertFalse(procedures["is_final"])
        self.assertEqual(procedures["bound_to"], None)
        self.assertEqual(type["data_components"], {})
        self.assertEqual(type["description"], "")
        self.assertIsNone(type["extends"])
        self.assertEqual(type["generic_interfaces"], {})

    def test_multiple_procedures_no_comments(self):
        self.fs.create_file(
            "/fake/path/types.f90",
            contents="""\
    module types
        implicit none
        private
        public :: simple_type
        type :: simple_type
        contains
            procedure, public :: init
            procedure, public :: add
            procedure, public :: multiply
        end type simple_type    
    end module types
""",
        )
        result = extract_module_data([Path("/fake/path/types.f90")])
        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(module["module_name"], "types")
        types = module["types"]
        self.assertEqual(len(types), 1)
        type = types["simple_type"]
        self.assertEqual(type["type_name"], "simple_type")
        self.assertEqual(type["attributes"], ["PUBLIC"])
        procedures = type["procedures"]
        self.assertEqual(len(procedures), 3)

        self.assertEqual(procedures["init"]["name"], "init")
        self.assertEqual(procedures["init"]["description"], "")
        self.assertEqual(procedures["init"]["attributes"], ["PUBLIC"])
        self.assertFalse(procedures["init"]["is_final"])
        self.assertEqual(procedures["init"]["bound_to"], None)

        self.assertEqual(procedures["add"]["name"], "add")
        self.assertEqual(procedures["add"]["description"], "")
        self.assertEqual(procedures["add"]["attributes"], ["PUBLIC"])
        self.assertFalse(procedures["add"]["is_final"])
        self.assertEqual(procedures["add"]["bound_to"], None)

        self.assertEqual(procedures["multiply"]["name"], "multiply")
        self.assertEqual(procedures["multiply"]["description"], "")
        self.assertEqual(procedures["multiply"]["attributes"], ["PUBLIC"])
        self.assertFalse(procedures["multiply"]["is_final"])
        self.assertEqual(procedures["multiply"]["bound_to"], None)

        self.assertEqual(type["data_components"], {})
        self.assertEqual(type["description"], "")
        self.assertIsNone(type["extends"])
        self.assertEqual(type["generic_interfaces"], {})

    def test_multiple_procedures_with_comments(self):
        self.fs.create_file(
            "/fake/path/types.f90",
            contents="""\
    module types
        implicit none
        private
        public :: simple_type
        type :: simple_type
        contains
            ! Initializer 
            procedure, public :: init
            !!* Adds two numbers *!
            procedure, public :: add
            !!*
            ! Multiplies two numbers
            !*!
            procedure, public :: multiply
        end type simple_type    
    end module types
""",
        )
        result = extract_module_data([Path("/fake/path/types.f90")])
        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(module["module_name"], "types")
        types = module["types"]
        self.assertEqual(len(types), 1)
        type = types["simple_type"]
        self.assertEqual(type["type_name"], "simple_type")
        self.assertEqual(type["attributes"], ["PUBLIC"])
        procedures = type["procedures"]
        self.assertEqual(len(procedures), 3)

        self.assertEqual(procedures["init"]["name"], "init")    
        self.assertEqual(procedures["init"]["description"], "")
        self.assertEqual(procedures["init"]["attributes"], ["PUBLIC"])
        self.assertFalse(procedures["init"]["is_final"])
        self.assertEqual(procedures["init"]["bound_to"], None)

        self.assertEqual(procedures["add"]["name"], "add")
        self.assertEqual(procedures["add"]["description"], "Adds two numbers\n")
        self.assertEqual(procedures["add"]["attributes"], ["PUBLIC"])
        self.assertFalse(procedures["add"]["is_final"])
        self.assertEqual(procedures["add"]["bound_to"], None)

        self.assertEqual(procedures["multiply"]["name"], "multiply")
        self.assertEqual(procedures["multiply"]["description"], "Multiplies two numbers\n")
        self.assertEqual(procedures["multiply"]["attributes"], ["PUBLIC"])
        self.assertFalse(procedures["multiply"]["is_final"])
        self.assertEqual(procedures["multiply"]["bound_to"], None)

        self.assertEqual(type["data_components"], {})
        self.assertEqual(type["description"], "")
        self.assertIsNone(type["extends"])
        self.assertEqual(type["generic_interfaces"], {})

    def test_multiple_procedures_with_attributes(self):
        self.fs.create_file(
            "/fake/path/types.f90",
            contents="""\
    module types
        implicit none
        private
        public :: simple_type
        type, abstract :: simple_type
        contains
            procedure, pass, public :: init
            procedure, public, deferred :: add
            procedure, public, nopass, deferred :: multiply
            procedure, pass(obj) :: custom_pass
            procedure, private :: hidden_method
        end type simple_type    
    end module types
""",
        )
        result = extract_module_data([Path("/fake/path/types.f90")])
        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(module["module_name"], "types")
        types = module["types"]
        self.assertEqual(len(types), 1)
        type = types["simple_type"]
        self.assertEqual(type["type_name"], "simple_type")
        self.assertCountEqual(type["attributes"], ["PUBLIC", "ABSTRACT"])
        init = type["procedures"]["init"]
        add = type["procedures"]["add"]
        multiply = type["procedures"]["multiply"]
        custom_pass = type["procedures"]["custom_pass"]
        hidden_method = type["procedures"]["hidden_method"]

        self.assertEqual(init["name"], "init")
        self.assertEqual(init["description"], "")
        self.assertEqual(init["attributes"], ["PUBLIC"])
        self.assertEqual(init["pass_type"], PassType.DEFAULT)
        self.assertIsNone(init["pass_name"])
        
        self.assertEqual(add["name"], "add")
        self.assertEqual(add["description"], "")
        self.assertCountEqual(add["attributes"], ["PUBLIC", "DEFERRED"])
        self.assertEqual(add["pass_type"], PassType.DEFAULT)
        self.assertIsNone(add["pass_name"])

        self.assertEqual(multiply["name"], "multiply")
        self.assertEqual(multiply["description"], "")
        self.assertCountEqual(multiply["attributes"], ["PUBLIC", "DEFERRED"])
        self.assertEqual(multiply["pass_type"], PassType.NONE)
        self.assertIsNone(multiply["pass_name"])

        self.assertEqual(custom_pass["name"], "custom_pass")
        self.assertEqual(custom_pass["description"], "")
        self.assertCountEqual(custom_pass["attributes"], ["PUBLIC"])
        self.assertEqual(custom_pass["pass_type"], PassType.NAMED)
        self.assertEqual(custom_pass["pass_name"], "obj")

        self.assertEqual(hidden_method["name"], "hidden_method")
        self.assertEqual(hidden_method["description"], "")
        self.assertCountEqual(hidden_method["attributes"], ["PRIVATE"])
        self.assertEqual(hidden_method["pass_type"], PassType.DEFAULT)
        self.assertIsNone(hidden_method["pass_name"])

        self.assertFalse(multiply["is_final"])
        self.assertEqual(type["data_components"], {})
        self.assertEqual(type["description"], "")
        self.assertIsNone(type["extends"])
        self.assertEqual(type["generic_interfaces"], {})

# =>
    def test_multiple_procedures_with_attributes_and_comments(self):
        self.fs.create_file(
            "/fake/path/types.f90",
            contents="""\
    module types
        implicit none
        private
        public :: simple_type
        !!*
        ! Base declaration for a simple type.
        !*!
        type, abstract :: simple_type
        contains
            !!* 
            ! Initialises the type
            !*!
            procedure, pass, public :: init
            !!*
            ! Adds two types together. How this is done depends on the implementation
            ! in the concrete type.
            !*!
            procedure, public, deferred :: add
            !!*
            ! Adds two types together. How this is done depends on the implementation
            ! in the concrete type. Note that procedure does not have access to data
            ! in the type.
            !*!
            procedure, public, nopass, deferred :: multiply
            !!*
            ! The procedure argument that accesses this type is called 'obj'
            !*!
            procedure, pass(obj) :: custom_pass
            !!*
            ! A private method
            !*!
            procedure, private :: hidden_method
        end type simple_type    
    end module types
""",
        )
        result = extract_module_data([Path("/fake/path/types.f90")])
        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(module["module_name"], "types")
        types = module["types"]
        self.assertEqual(len(types), 1)
        type = types["simple_type"]
        self.assertEqual(type["type_name"], "simple_type")
        self.assertCountEqual(type["attributes"], ["PUBLIC", "ABSTRACT"])
        init = type["procedures"]["init"]
        add = type["procedures"]["add"]
        multiply = type["procedures"]["multiply"]
        custom_pass = type["procedures"]["custom_pass"]
        hidden_method = type["procedures"]["hidden_method"]

        self.assertEqual(init["name"], "init")
        self.assertEqual(init["description"], "Initialises the type\n")
        self.assertEqual(init["attributes"], ["PUBLIC"])
        self.assertEqual(init["pass_type"], PassType.DEFAULT)
        self.assertIsNone(init["pass_name"])

        self.assertEqual(add["name"], "add")
        self.assertEqual(add["description"], "Adds two types together. How this is done depends on the implementation\nin the concrete type.\n")
        self.assertCountEqual(add["attributes"], ["PUBLIC", "DEFERRED"])
        self.assertEqual(add["pass_type"], PassType.DEFAULT)
        self.assertIsNone(add["pass_name"])

        self.assertEqual(multiply["name"], "multiply")
        self.assertEqual(multiply["description"], "Adds two types together. How this is done depends on the implementation\n\
in the concrete type. Note that procedure does not have access to data\nin the type.\n")
        self.assertCountEqual(multiply["attributes"], ["PUBLIC", "DEFERRED"])
        self.assertEqual(multiply["pass_type"], PassType.NONE)
        self.assertIsNone(multiply["pass_name"])

        self.assertEqual(custom_pass["name"], "custom_pass")
        self.assertEqual(custom_pass["description"], "The procedure argument that accesses this type is called &#x27;obj&#x27;\n")
        self.assertCountEqual(custom_pass["attributes"], ["PUBLIC"])
        self.assertEqual(custom_pass["pass_type"], PassType.NAMED)
        self.assertEqual(custom_pass["pass_name"], "obj")

        self.assertEqual(hidden_method["name"], "hidden_method")
        self.assertEqual(hidden_method["description"], "A private method\n")
        self.assertCountEqual(hidden_method["attributes"], ["PRIVATE"])
        self.assertEqual(hidden_method["pass_type"], PassType.DEFAULT)
        self.assertIsNone(hidden_method["pass_name"])

        self.assertFalse(multiply["is_final"])
        self.assertEqual(type["data_components"], {})
        self.assertEqual(type["description"], "Base declaration for a simple type.\n")
        self.assertIsNone(type["extends"])
        self.assertEqual(type["generic_interfaces"], {})

    def test_renamed_procedure_no_comments(self):
        self.fs.create_file(
            "/fake/path/types.f90",
            contents="""\
    module types
        implicit none
        import test
        private
        public :: simple_type
        type :: simple_type
        contains
            procedure, public :: add => plus
            procedure, public :: subtract
        end type simple_type    
    end module types
""",
        )
        result = extract_module_data([Path("/fake/path/types.f90")])
        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(module["module_name"], "types")
        types = module["types"]
        self.assertEqual(len(types), 1)
        type = types["simple_type"]
        self.assertEqual(type["type_name"], "simple_type")
        self.assertEqual(type["attributes"], ["PUBLIC"])

        add = type["procedures"]["add"]
        self.assertEqual(add["name"], "add")
        self.assertEqual(add["description"], "")
        self.assertEqual(add["attributes"], ["PUBLIC"])
        self.assertEqual(add["bound_to"], None)
        self.assertEqual(add["pass_type"], PassType.DEFAULT)
        self.assertIsNone(add["pass_name"])
        self.assertEqual(add["implementation"], "plus")

        subtract = type["procedures"]["subtract"]
        self.assertEqual(subtract["name"], "subtract")
        self.assertEqual(subtract["description"], "")
        self.assertEqual(subtract["attributes"], ["PUBLIC"])
        self.assertEqual(subtract["bound_to"], None)
        self.assertEqual(subtract["pass_type"], PassType.DEFAULT)
        self.assertIsNone(subtract["pass_name"])
        self.assertIsNone(subtract["implementation"])

        self.assertEqual(type["data_components"], {})
        self.assertEqual(type["description"], "")
        self.assertIsNone(type["extends"])
        self.assertEqual(type["generic_interfaces"], {})

    def test_type_with_final_procedure_and_comment(self):
        self.fs.create_file(
            "/fake/path/types.f90",
            contents="""\
    module types
        implicit none
        private
        public :: simple_type
        type, abstract :: simple_type
        contains
            !!* Initializes the type *!
            procedure, public :: init
            !!* Cleans up the type *!
            final :: cleanup, other_cleanup
        end type simple_type    
    end module types
""",
        )
        result = extract_module_data([Path("/fake/path/types.f90")])
        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(module["module_name"], "types")
        types = module["types"]
        self.assertEqual(len(types), 1)
        type = types["simple_type"]
        self.assertEqual(type["type_name"], "simple_type")
        self.assertCountEqual(type["attributes"], ["PUBLIC", "ABSTRACT"])

        init = type["procedures"]["init"]
        self.assertEqual(init["name"], "init")
        self.assertEqual(init["description"], "Initializes the type\n")
        self.assertEqual(init["attributes"], ["PUBLIC"])
        self.assertFalse(init["is_final"])
        
        cleanup = type["procedures"]["cleanup"]
        self.assertEqual(cleanup["name"], "cleanup")
        self.assertEqual(cleanup["description"], "Cleans up the type\n")
        self.assertEqual(cleanup["attributes"], ["PUBLIC"])
        self.assertTrue(cleanup["is_final"])
        
        other_cleanup = type["procedures"]["other_cleanup"]
        self.assertEqual(other_cleanup["name"], "other_cleanup")
        self.assertEqual(other_cleanup["description"], "Cleans up the type\n")
        self.assertEqual(other_cleanup["attributes"], ["PUBLIC"])
        self.assertTrue(other_cleanup["is_final"])

        self.assertEqual(type["data_components"], {})
        self.assertEqual(type["description"], "")
        self.assertIsNone(type["extends"])
        self.assertEqual(type["generic_interfaces"], {})

if __name__ == "__main__":
    unittest.main()
