import unittest
from pathlib import Path
from pyfakefs.fake_filesystem_unittest import TestCase
from doc4for.f90.generate_type_tree import generate_inheritance_tree  

class TestInheritanceTreePopulation(TestCase):
    def setUp(self):
        self.setUpPyfakefs()

    def test_populate_inheritance_tree_unrelated_types_in_modules(self):
        self.fs.create_file(
            '/fake/path/unrelated_types.f90',
            contents='''\
module unrelated_types
    implicit none

    type :: type_a
        integer :: a_value
    end type type_a

    type :: type_b
        real :: b_value
    end type type_b

    type :: type_c
        character(len=10) :: c_value
    end type type_c

end module unrelated_types
            ''',
        )

        result = generate_inheritance_tree([Path('/fake/path/unrelated_types.f90')])
        self.assertIn('type_a', result)
        self.assertIn('type_b', result)
        self.assertIn('type_c', result)
        self.assertIsNone(result['type_a']['parent'])
        self.assertEqual(result['type_a']['children'], [])
        self.assertIsNone(result['type_b']['parent'])
        self.assertEqual(result['type_b']['children'], [])
        self.assertIsNone(result['type_c']['parent'])
        self.assertEqual(result['type_c']['children'], [])
        self.assertEqual(len(result), 3)

    def test_populate_inheritance_tree_unrelated_types_multiple_files(self):
        self.fs.create_file(
            '/fake/path/type_a.f90',
            contents='''\
module type_a_module
    implicit none

    type :: type_a
        integer :: a_value
    end type type_a

end module type_a_module
            ''',
        )

        self.fs.create_file(
            '/fake/path/type_b.f90',
            contents='''\
module type_b_module
    implicit none

    type :: type_b
        real :: b_value
    end type type_b

end module type_b_module
            ''',
        )

        self.fs.create_file(
            '/fake/path/type_c.f90',
            contents='''\
module type_c_module
    implicit none

    type :: type_c
        character(len=10) :: c_value
    end type type_c

end module type_c_module
            ''',
        )

        # Create a list of Paths for all three files
        f90_files = [
            Path('/fake/path/type_a.f90'),
            Path('/fake/path/type_b.f90'),
            Path('/fake/path/type_c.f90')
        ]

        result = generate_inheritance_tree(f90_files)

        # Check if all types are in the tree
        self.assertIn('type_a', result)
        self.assertIn('type_b', result)
        self.assertIn('type_c', result)

        # Check type_a
        self.assertIsNone(result['type_a']['parent'])
        self.assertEqual(result['type_a']['children'], [])

        # Check type_b
        self.assertIsNone(result['type_b']['parent'])
        self.assertEqual(result['type_b']['children'], [])

        # Check type_c
        self.assertIsNone(result['type_c']['parent'])
        self.assertEqual(result['type_c']['children'], [])

        # Check that there are only these three types in the result
        self.assertEqual(len(result), 3)

    def test_populate_inheritance_tree_parent_child(self):
        self.fs.create_file(
            '/fake/path/parent_child_types.f90',
            contents='''\
module parent_child_types
    implicit none

    type :: parent_type
        integer :: parent_value
    end type parent_type

    type, extends(parent_type) :: child_type
        real :: child_value
    end type child_type

end module parent_child_types
            ''',
        )

        result = generate_inheritance_tree([Path('/fake/path/parent_child_types.f90')])

        # Check if both types are in the tree
        self.assertIn('parent_type', result)
        self.assertIn('child_type', result)

        # Check parent_type
        self.assertIsNone(result['parent_type']['parent'])
        self.assertEqual(result['parent_type']['children'], ['child_type'])

        # Check child_type
        self.assertEqual(result['child_type']['parent'], 'parent_type')
        self.assertEqual(result['child_type']['children'], [])

        # Check that there are only these two types in the result
        self.assertEqual(len(result), 2)

    def test_populate_inheritance_tree_parent_child_different_files(self):
        # Create file for parent type
        self.fs.create_file(
            '/fake/path/parent_type.f90',
            contents='''\
module parent_module
    implicit none

    type :: parent_type
        integer :: parent_value
    end type parent_type

end module parent_module
            ''',
        )

        # Create file for child type
        self.fs.create_file(
            '/fake/path/child_type.f90',
            contents='''\
module child_module
    use parent_module, only: parent_type
    implicit none

    type, extends(parent_type) :: child_type
        real :: child_value
    end type child_type

end module child_module
            ''',
        )

        # Create a list of Paths for both files
        f90_files = [
            Path('/fake/path/parent_type.f90'),
            Path('/fake/path/child_type.f90')
        ]

        result = generate_inheritance_tree(f90_files)

        # Check if both types are in the tree
        self.assertIn('parent_type', result)
        self.assertIn('child_type', result)

        # Check parent_type
        self.assertIsNone(result['parent_type']['parent'])
        self.assertEqual(result['parent_type']['children'], ['child_type'])

        # Check child_type
        self.assertEqual(result['child_type']['parent'], 'parent_type')
        self.assertEqual(result['child_type']['children'], [])

        # Check that there are only these two types in the result
        self.assertEqual(len(result), 2)

    def test_populate_inheritance_tree_complex_inheritance_1(self):
        # Create file for base types
        self.fs.create_file(
            '/fake/path/base_types.f90',
            contents='''\
module base_types
    implicit none

    type :: grandparent_type
        integer :: grandparent_value
    end type grandparent_type

    type, extends(grandparent_type) :: parent_type_a
        real :: parent_a_value
    end type parent_type_a

    type, extends(grandparent_type) :: parent_type_b
        character(len=10) :: parent_b_value
    end type parent_type_b

end module base_types
            ''',
        )

        # Create file for child types
        self.fs.create_file(
            '/fake/path/child_types.f90',
            contents='''\
module child_types
    use base_types
    implicit none

    type, extends(parent_type_a) :: child_type_a1
        logical :: child_a1_value
    end type child_type_a1

    type, extends(parent_type_a) :: child_type_a2
        complex :: child_a2_value
    end type child_type_a2

    type, extends(parent_type_b) :: child_type_b1
        integer, dimension(3) :: child_b1_value
    end type child_type_b1

end module child_types
            ''',
        )

        # Create file for grandchild type
        self.fs.create_file(
            '/fake/path/grandchild_type.f90',
            contents='''\
module grandchild_types
    use child_types
    implicit none

    type, extends(child_type_a1) :: grandchild_type
        real, dimension(2,2) :: grandchild_value
    end type grandchild_type

end module grandchild_types
            ''',
        )

        f90_files = [
            Path('/fake/path/base_types.f90'),
            Path('/fake/path/child_types.f90'),
            Path('/fake/path/grandchild_type.f90')
        ]

        result = generate_inheritance_tree(f90_files)

        # Check if all types are in the tree
        expected_types = ['grandparent_type', 'parent_type_a', 'parent_type_b', 
                          'child_type_a1', 'child_type_a2', 'child_type_b1', 
                          'grandchild_type']
        for type_name in expected_types:
            self.assertIn(type_name, result)

        # Check grandparent_type
        self.assertIsNone(result['grandparent_type']['parent'])
        self.assertCountEqual(result['grandparent_type']['children'], ['parent_type_a', 'parent_type_b'])

        # Check parent_type_a
        self.assertEqual(result['parent_type_a']['parent'], 'grandparent_type')
        self.assertCountEqual(result['parent_type_a']['children'], ['child_type_a1', 'child_type_a2'])

        # Check parent_type_b
        self.assertEqual(result['parent_type_b']['parent'], 'grandparent_type')
        self.assertCountEqual(result['parent_type_b']['children'], ['child_type_b1'])

        # Check child_type_a1
        self.assertEqual(result['child_type_a1']['parent'], 'parent_type_a')
        self.assertCountEqual(result['child_type_a1']['children'], ['grandchild_type'])

        # Check child_type_a2
        self.assertEqual(result['child_type_a2']['parent'], 'parent_type_a')
        self.assertEqual(result['child_type_a2']['children'], [])

        # Check child_type_b1
        self.assertEqual(result['child_type_b1']['parent'], 'parent_type_b')
        self.assertEqual(result['child_type_b1']['children'], [])

        # Check grandchild_type
        self.assertEqual(result['grandchild_type']['parent'], 'child_type_a1')
        self.assertEqual(result['grandchild_type']['children'], [])

        # Check that there are only these types in the result
        self.assertEqual(len(result), len(expected_types))

    def test_populate_inheritance_tree_complex_inheritance_2(self):
        # Create file for grandparent type
        self.fs.create_file(
            '/fake/path/grandparent_type.f90',
            contents='''\
module grandparent_module
    implicit none

    type :: grandparent_type
        integer :: grandparent_value
    end type grandparent_type

end module grandparent_module
            ''',
        )

        # Create file for parent type
        self.fs.create_file(
            '/fake/path/parent_type.f90',
            contents='''\
module parent_module
    use grandparent_module, only: grandparent_type
    implicit none

    type, extends(grandparent_type) :: parent_type
        real :: parent_value
    end type parent_type

end module parent_module
            ''',
        )

        # Create file for child type 1
        self.fs.create_file(
            '/fake/path/child1_type.f90',
            contents='''\
module child1_module
    use parent_module, only: parent_type
    implicit none

    type, extends(parent_type) :: child1_type
        character(len=10) :: child1_value
    end type child1_type

end module child1_module
            ''',
        )

        # Create file for child type 2
        self.fs.create_file(
            '/fake/path/child2_type.f90',
            contents='''\
module child2_module
    use parent_module, only: parent_type
    implicit none

    type, extends(parent_type) :: child2_type
        logical :: child2_value
    end type child2_type

end module child2_module
            ''',
        )

        # Create a list of Paths for all files
        f90_files = [
            Path('/fake/path/grandparent_type.f90'),
            Path('/fake/path/parent_type.f90'),
            Path('/fake/path/child1_type.f90'),
            Path('/fake/path/child2_type.f90')
        ]

        result = generate_inheritance_tree(f90_files)

        # Check if all types are in the tree
        self.assertIn('grandparent_type', result)
        self.assertIn('parent_type', result)
        self.assertIn('child1_type', result)
        self.assertIn('child2_type', result)

        # Check grandparent_type
        self.assertIsNone(result['grandparent_type']['parent'])
        self.assertEqual(result['grandparent_type']['children'], ['parent_type'])

        # Check parent_type
        self.assertEqual(result['parent_type']['parent'], 'grandparent_type')
        self.assertEqual(set(result['parent_type']['children']), {'child1_type', 'child2_type'})

        # Check child1_type
        self.assertEqual(result['child1_type']['parent'], 'parent_type')
        self.assertEqual(result['child1_type']['children'], [])

        # Check child2_type
        self.assertEqual(result['child2_type']['parent'], 'parent_type')
        self.assertEqual(result['child2_type']['children'], [])

        # Check that there are only these four types in the result
        self.assertEqual(len(result), 4)

if __name__ == '__main__':
    unittest.main()