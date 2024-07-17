import unittest
from pathlib import Path
from pyfakefs.fake_filesystem_unittest import TestCase
from doc4for.f90.generate_module_tree import extract_module_data

class TestTypes(TestCase):
    def setUp(self):
        self.setUpPyfakefs()

    def test_single_procedures_no_comments(self):
        self.fs.create_file(
            '/fake/path/types.f90',
            contents='''\
    module types
        implicit none
        private
        public :: simple_type
        type :: simple_type
        contains
            procedure, public :: init
        end type simple_type    
    end module types
''',
        )
        result = extract_module_data([Path('/fake/path/types.f90')])
        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(module['module_name'], 'types')
        types = module['types']
        self.assertEqual(len(types), 1)
        type = types['simple_type']
        self.assertEqual(type['type_name'], 'simple_type')
        self.assertEqual(type['attributes'], ['public'])
        procedures = type['procedures']
        self.assertEqual(procedures['init']['name'], 'init')
        self.assertEqual(procedures['init']['description'], '')
        self.assertEqual(type['data_components'], {})
        self.assertEqual(type['description'], '')
        self.assertIsNone(type['extends'])
        self.assertEqual(type['generic_interfaces'], {})

    def test_single_procedure_with_comments(self):
        self.fs.create_file(
            '/fake/path/types.f90',
            contents='''\
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
''',
        )
        result = extract_module_data([Path('/fake/path/types.f90')])
        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(module['module_name'], 'types')
        types = module['types']
        self.assertEqual(len(types), 1)
        type = types['simple_type']
        self.assertEqual(type['type_name'], 'simple_type')
        self.assertEqual(type['attributes'], ['public'])
        procedures = type['procedures']
        self.assertEqual(procedures['init']['name'], 'init')
        self.assertEqual(procedures['init']['description'], '\nA procedure that initialises the type\n\n')
        self.assertEqual(type['data_components'], {})
        self.assertEqual(type['description'], '')
        self.assertIsNone(type['extends'])
        self.assertEqual(type['generic_interfaces'], {})

    def test_multiple_procedures_no_comments(self):
        self.fs.create_file(
            '/fake/path/types.f90',
            contents='''\
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
''',
        )
        result = extract_module_data([Path('/fake/path/types.f90')])
        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(module['module_name'], 'types')
        types = module['types']
        self.assertEqual(len(types), 1)
        type = types['simple_type']
        self.assertEqual(type['type_name'], 'simple_type')
        self.assertEqual(type['attributes'], ['public'])
        procedures = type['procedures']
        self.assertEqual(len(procedures), 3)
        self.assertEqual(procedures['init']['name'], 'init')
        self.assertEqual(procedures['init']['description'], '')
        self.assertEqual(procedures['add']['name'], 'add')
        self.assertEqual(procedures['add']['description'], '')
        self.assertEqual(procedures['multiply']['name'], 'multiply')
        self.assertEqual(procedures['multiply']['description'], '')
        self.assertEqual(type['data_components'], {})
        self.assertEqual(type['description'], '')
        self.assertIsNone(type['extends'])
        self.assertEqual(type['generic_interfaces'], {})

    def test_multiple_procedures_with_comments(self):
        self.fs.create_file(
            '/fake/path/types.f90',
            contents='''\
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
''',
        )
        result = extract_module_data([Path('/fake/path/types.f90')])
        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(module['module_name'], 'types')
        types = module['types']
        self.assertEqual(len(types), 1)
        type = types['simple_type']
        self.assertEqual(type['type_name'], 'simple_type')
        self.assertEqual(type['attributes'], ['public'])
        procedures = type['procedures']
        self.assertEqual(len(procedures), 3)
        self.assertEqual(procedures['init']['name'], 'init')
        self.assertEqual(procedures['init']['description'], '')
        self.assertEqual(procedures['add']['name'], 'add')
        self.assertEqual(procedures['add']['description'], 'Adds two numbers\n')
        self.assertEqual(procedures['multiply']['name'], 'multiply')
        self.assertEqual(procedures['multiply']['description'], '\nMultiplies two numbers\n\n')
        self.assertEqual(type['data_components'], {})
        self.assertEqual(type['description'], '')
        self.assertIsNone(type['extends'])
        self.assertEqual(type['generic_interfaces'], {})

    def test_multiple_procedures_with_attributes(self):
        self.fs.create_file(
            '/fake/path/types.f90',
            contents='''\
    module types
        implicit none
        private
        public :: simple_type
        type, abstract :: simple_type
        contains
            procedure, public :: init
            procedure, public, deferred :: add
            procedure, public, nopass, deferred :: multiply
        end type simple_type    
    end module types
''',
        )
        result = extract_module_data([Path('/fake/path/types.f90')])
        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(module['module_name'], 'types')
        types = module['types']
        self.assertEqual(len(types), 1)
        type = types['simple_type']
        self.assertEqual(type['type_name'], 'simple_type')
        self.assertCountEqual(type['attributes'], ['public', 'abstract'])
        init = type['procedures']['init']
        add = type['procedures']['add']
        multiply = type['procedures']['multiply']
        self.assertEqual(init['name'], 'init')
        self.assertEqual(init['description'], '')
        self.assertEqual(init['attributes'], ['public'])
        self.assertEqual(add['name'], 'add')
        self.assertEqual(add['description'], '')
        self.assertCountEqual(add['attributes'], ['public', 'deferred'])
        self.assertEqual(multiply['name'], 'multiply')
        self.assertEqual(multiply['description'], '')
        self.assertCountEqual(multiply['attributes'], ['public', 'deferred', 'nopass'])
        self.assertFalse(multiply['is_final'])
        self.assertEqual(type['data_components'], {})
        self.assertEqual(type['description'], '')
        self.assertIsNone(type['extends'])
        self.assertEqual(type['generic_interfaces'], {})

    def test_multiple_procedures_with_attributes_and_comments(self):
        self.fs.create_file(
            '/fake/path/types.f90',
            contents='''\
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
            procedure, public :: init
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
        end type simple_type    
    end module types
''',
        )
        result = extract_module_data([Path('/fake/path/types.f90')])
        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(module['module_name'], 'types')
        types = module['types']
        self.assertEqual(len(types), 1)
        type = types['simple_type']
        self.assertEqual(type['type_name'], 'simple_type')
        self.assertCountEqual(type['attributes'], ['public', 'abstract'])
        init = type['procedures']['init']
        add = type['procedures']['add']
        multiply = type['procedures']['multiply']
        self.assertEqual(init['name'], 'init')
        self.assertEqual(init['description'], '\nInitialises the type\n\n')
        self.assertEqual(init['attributes'], ['public'])
        self.assertEqual(add['name'], 'add')
        self.assertEqual(add['description'], '\nAdds two types together. How this is done depends on the implementation\nin the concrete type.\n\n')
        self.assertCountEqual(add['attributes'], ['public', 'deferred'])
        self.assertEqual(multiply['name'], 'multiply')
        self.assertEqual(multiply['description'], '\nAdds two types together. How this is done depends on the implementation\nin the concrete type. Note that procedure does not have access to data\nin the type.\n\n')
        self.assertCountEqual(multiply['attributes'], ['public', 'deferred', 'nopass'])
        self.assertFalse(multiply['is_final'])
        self.assertEqual(type['data_components'], {})
        self.assertEqual(type['description'], '\nBase declaration for a simple type.\n\n')
        self.assertIsNone(type['extends'])
        self.assertEqual(type['generic_interfaces'], {})

    def test_type_with_final_procedure(self):
        self.fs.create_file(
            '/fake/path/types.f90',
            contents='''\
    module types
        implicit none
        private
        public :: simple_type
        type, abstract :: simple_type
        contains
            procedure, public :: init
            final :: cleanup
        end type simple_type    
    end module types
''',
        )
        result = extract_module_data([Path('/fake/path/types.f90')])
        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(module['module_name'], 'types')
        types = module['types']
        self.assertEqual(len(types), 1)
        type = types['simple_type']
        self.assertEqual(type['type_name'], 'simple_type')
        self.assertCountEqual(type['attributes'], ['public', 'abstract'])
        init = type['procedures']['init']
        cleanup = type['procedures']['cleanup']
        self.assertEqual(init['name'], 'init')
        self.assertEqual(init['description'], '')
        self.assertEqual(init['attributes'], ['public'])
        self.assertFalse(init['is_final'])
        self.assertEqual(cleanup['name'], 'cleanup')
        self.assertEqual(cleanup['description'], '')
        self.assertEqual(cleanup['attributes'], ['final'])
        self.assertTrue(cleanup['is_final'])
        self.assertEqual(type['data_components'], {})
        self.assertEqual(type['description'], '')
        self.assertIsNone(type['extends'])
        self.assertEqual(type['generic_interfaces'], {})

    def test_type_with_final_procedure_and_comment(self):
        self.fs.create_file(
            '/fake/path/types.f90',
            contents='''\
    module types
        implicit none
        private
        public :: simple_type
        type, abstract :: simple_type
        contains
            !!* Initializes the type *!
            procedure, public :: init
            !!* Cleans up the type *!
            final :: cleanup
        end type simple_type    
    end module types
''',
        )
        result = extract_module_data([Path('/fake/path/types.f90')])
        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(module['module_name'], 'types')
        types = module['types']
        self.assertEqual(len(types), 1)
        type = types['simple_type']
        self.assertEqual(type['type_name'], 'simple_type')
        self.assertCountEqual(type['attributes'], ['public', 'abstract'])
        init = type['procedures']['init']
        cleanup = type['procedures']['cleanup']
        self.assertEqual(init['name'], 'init')
        self.assertEqual(init['description'], 'Initializes the type\n')
        self.assertEqual(init['attributes'], ['public'])
        self.assertFalse(init['is_final'])
        self.assertEqual(cleanup['name'], 'cleanup')
        self.assertEqual(cleanup['description'], 'Cleans up the type\n')
        self.assertEqual(cleanup['attributes'], ['final'])
        self.assertTrue(cleanup['is_final'])
        self.assertEqual(type['data_components'], {})
        self.assertEqual(type['description'], '')
        self.assertIsNone(type['extends'])
        self.assertEqual(type['generic_interfaces'], {})

if __name__ == '__main__':
    unittest.main()
