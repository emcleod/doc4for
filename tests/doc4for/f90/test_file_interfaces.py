import unittest
from pathlib import Path
from pyfakefs.fake_filesystem_unittest import TestCase
from doc4for.f90.generate_file_tree import extract_file_data

class TestInterfaceExtraction(TestCase):
    def setUp(self):
        self.setUpPyfakefs()

#TODO
#     def test_interface_names_extracted(self):
#         self.fs.create_file(
#             '/fake/path/interfaces.f90',
#             contents='''\
# implicit none

# interface interface1
#     subroutine sub1(a, b, c)
#         real, intent(in) :: a, b
#         real, intent(out) :: c
#     end subroutine sub1
# end interface interface1

# interface interface2
#     function func1(x) result(y)
#         real, intent(in) :: x
#         real :: y
#     end function func1
# end interface interface2
#     ''')
#         result = extract_file_data([Path('/fake/path/interfaces.f90')])
#         self.assertEqual(len(result), 1)
#         file_data = result[0]
#         self.assertEqual(file_data['file_name'], '/fake/path/interfaces.f90')
#         self.assertEqual(len(file_data['interfaces']), 2)
#         self.assertIn('interface1', file_data['interfaces'])
#         self.assertIn('interface2', file_data['interfaces'])

if __name__ == '__main__':
    unittest.main()