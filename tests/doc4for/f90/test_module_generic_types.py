import unittest
from pathlib import Path
from pyfakefs.fake_filesystem_unittest import TestCase
from doc4for.f90.generate_module_tree import extract_module_data

class TestGenericTypes(TestCase):
    def setUp(self):
        self.setUpPyfakefs()

    def test_type_with_generic_procedure(self):
        self.fs.create_file(
            '/fake/path/types.f90',
            contents='''\
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
''',
        )
        result = extract_module_data([Path('/fake/path/types.f90')])
        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(module['module_name'], 'vector_ops')
        types = module['types']
        self.assertEqual(len(types), 1)
        type = types['vector']
        self.assertEqual(type['type_name'], 'vector')
        self.assertEqual(len(type['procedures']), 2)
        add_vector = type['procedures']['add_vector']
        self.assertEqual(add_vector['name'], 'add_vector')
        self.assertEqual(add_vector['attributes'], ['public'])
        self.assertFalse(add_vector['is_final'])
        add_scalar = type['procedures']['add_scalar']
        self.assertEqual(add_scalar['name'], 'add_scalar')
        self.assertEqual(add_scalar['attributes'], ['public'])
        self.assertFalse(add_scalar['is_final'])
        self.assertEqual(len(type['generic_interfaces']), 1)
        plus_operator = type['generic_interfaces']['operator(+)']
        self.assertEqual(plus_operator['generic_spec'], 'operator(+)')
        self.assertEqual(plus_operator['description'], '')
        self.assertCountEqual(plus_operator['specific_procedures'], ['add_vector', 'add_scalar'])

    def test_type_with_multiple_generic_procedures(self):
        self.fs.create_file(
            '/fake/path/types.f90',
            contents='''\
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
''',
        )
        result = extract_module_data([Path('/fake/path/types.f90')])
        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(module['module_name'], 'vector_ops')
        types = module['types']
        self.assertEqual(len(types), 1)
        type = types['vector']
        self.assertEqual(type['type_name'], 'vector')
        self.assertEqual(len(type['procedures']), 3)
        add_vector = type['procedures']['add_vector']
        self.assertEqual(add_vector['name'], 'add_vector')
        self.assertEqual(add_vector['attributes'], ['public'])
        self.assertFalse(add_vector['is_final'])
        add_scalar = type['procedures']['add_scalar']
        self.assertEqual(add_scalar['name'], 'add_scalar')
        self.assertEqual(add_scalar['attributes'], ['public'])
        self.assertFalse(add_scalar['is_final'])
        print_vector = type['procedures']['print_vector']
        self.assertEqual(print_vector['name'], 'print_vector')
        self.assertEqual(print_vector['attributes'], ['public'])
        self.assertFalse(print_vector['is_final'])
        self.assertEqual(len(type['generic_interfaces']), 2)
        plus_operator = type['generic_interfaces']['operator(+)']
        self.assertEqual(plus_operator['generic_spec'], 'operator(+)')
        self.assertEqual(plus_operator['description'], '')
        self.assertCountEqual(plus_operator['specific_procedures'], ['add_vector', 'add_scalar'])
        write_operator = type['generic_interfaces']['write(formatted)']
        self.assertEqual(write_operator['generic_spec'], 'write(formatted)')
        self.assertEqual(write_operator['description'], '')
        self.assertCountEqual(write_operator['specific_procedures'], ['print_vector'])

    def test_type_with_multiple_generic_procedures_and_comments(self):
        self.fs.create_file(
            '/fake/path/types.f90',
            contents='''\
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
''',
        )
        result = extract_module_data([Path('/fake/path/types.f90')])
        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(module['module_name'], 'vector_ops')
        types = module['types']
        self.assertEqual(len(types), 1)
        type = types['vector']
        self.assertEqual(type['type_name'], 'vector')
        self.assertEqual(len(type['procedures']), 3)
        add_vector = type['procedures']['add_vector']
        self.assertEqual(add_vector['name'], 'add_vector')
        self.assertEqual(add_vector['attributes'], ['public'])
        self.assertEqual(add_vector['description'], 'Adds two vectors\n')
        self.assertFalse(add_vector['is_final'])
        add_scalar = type['procedures']['add_scalar']
        self.assertEqual(add_scalar['name'], 'add_scalar')
        self.assertEqual(add_scalar['attributes'], ['public'])
        self.assertEqual(add_scalar['description'], 'Adds two scalars\n')
        self.assertFalse(add_scalar['is_final'])
        print_vector = type['procedures']['print_vector']
        self.assertEqual(print_vector['name'], 'print_vector')
        self.assertEqual(print_vector['attributes'], ['public'])
        self.assertEqual(print_vector['description'], 'Prints a vector\n')
        self.assertFalse(print_vector['is_final'])
        self.assertEqual(len(type['generic_interfaces']), 2)
        plus_operator = type['generic_interfaces']['operator(+)']
        self.assertEqual(plus_operator['generic_spec'], 'operator(+)')
        self.assertEqual(plus_operator['description'], 'Overloads the plus operator to handle vectors and scalars\n')
        self.assertCountEqual(plus_operator['specific_procedures'], ['add_vector', 'add_scalar'])
        write_operator = type['generic_interfaces']['write(formatted)']
        self.assertEqual(write_operator['generic_spec'], 'write(formatted)')
        self.assertEqual(write_operator['description'], 'Overloads the write() operator\n')
        self.assertCountEqual(write_operator['specific_procedures'], ['print_vector'])

if __name__ == '__main__':
    unittest.main()
