import unittest
from pathlib import Path
from pyfakefs.fake_filesystem_unittest import TestCase
from doc4for.f90.generate_module_tree import extract_module_data

class TestDataComponents(TestCase):
    def setUp(self):
        self.setUpPyfakefs()

    def test_public_data_components_no_comments(self):
        self.fs.create_file(
            "/fake/path/types.f90",
            contents="""\
 module simple_module
   implicit none
   private

   public :: simple_type

   type :: simple_type
     real, public :: x
     integer, public :: y
   contains
     procedure, public :: init
     procedure, public :: get_x
     procedure, public :: get_y
   end type simple_type
end module simple_module
 """,
        )
        result = extract_module_data([Path("/fake/path/types.f90")])
        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(module["module_name"], "simple_module")
        types = module["types"]
        self.assertEqual(len(types), 1)
        type = types["simple_type"]
        self.assertEqual(type["type_name"], "simple_type")
        self.assertEqual(type["attributes"], ["PUBLIC"])
        procedures = type["procedures"]
        self.assertEqual(procedures["init"]["name"], "init")
        self.assertEqual(procedures["init"]["description"], "")
        data_components = type["data_components"]
        self.assertEqual(len(data_components), 2)
        self.assertEqual(data_components["x"]["name"], "x")
        self.assertEqual(data_components["x"]["type"], "REAL")
        self.assertEqual(data_components["x"]["description"], "")
        self.assertEqual(data_components["x"]["dimension"], None)
        self.assertEqual(data_components["x"]["initial_value"], None)
        self.assertEqual(data_components["x"]["attributes"], ["PUBLIC"])
        self.assertEqual(data_components["y"]["name"], "y")
        self.assertEqual(data_components["y"]["type"], "INTEGER")
        self.assertEqual(data_components["y"]["description"], "")
        self.assertEqual(data_components["y"]["dimension"], None)
        self.assertEqual(data_components["y"]["initial_value"], None)
        self.assertEqual(data_components["y"]["attributes"], ["PUBLIC"])
        self.assertEqual(type["description"], "")
        self.assertIsNone(type["extends"])
        self.assertEqual(type["generic_interfaces"], {})

#     def test_public_data_components_with_comments(self):
#         self.fs.create_file(
#             "/fake/path/types.f90",
#             contents="""\
#  module simple_module
#    implicit none
#    private

#    public :: simple_type

#    type :: simple_type
#      !!* declares x *!
#      real, public :: x
#      !!* declares y *!
#      integer, public :: y
#    contains
#      procedure, public :: init
#      procedure, public :: get_x
#      procedure, public :: get_y
#    end type simple_type
# end module simple_module
#  """,
#         )
#         result = extract_module_data([Path("/fake/path/types.f90")])
#         self.assertEqual(len(result), 1)
#         module = result[0]
#         self.assertEqual(module["module_name"], "simple_module")
#         types = module["types"]
#         self.assertEqual(len(types), 1)
#         type = types["simple_type"]
#         self.assertEqual(type["type_name"], "simple_type")
#         self.assertEqual(type["attributes"], ["PUBLIC"])
#         procedures = type["procedures"]
#         self.assertEqual(procedures["init"]["name"], "init")
#         self.assertEqual(procedures["init"]["description"], "")
#         data_components = type["data_components"]
#         self.assertEqual(len(data_components), 2)
#         self.assertEqual(data_components["x"]["name"], "x")
#         self.assertEqual(data_components["x"]["type"], "REAL")
#         self.assertEqual(data_components["x"]["description"], "declares x\n")
#         self.assertEqual(data_components["x"]["dimension"], None)
#         self.assertEqual(data_components["x"]["initial_value"], None)
#         self.assertEqual(data_components["x"]["attributes"], ["PUBLIC"])
#         self.assertEqual(data_components["y"]["name"], "y")
#         self.assertEqual(data_components["y"]["type"], "INTEGER")
#         self.assertEqual(data_components["y"]["description"], "declares y\n")
#         self.assertEqual(data_components["y"]["dimension"], None)
#         self.assertEqual(data_components["y"]["initial_value"], None)
#         self.assertEqual(data_components["y"]["attributes"], ["PUBLIC"])
#         self.assertEqual(type["description"], "")
#         self.assertIsNone(type["extends"])
#         self.assertEqual(type["generic_interfaces"], {})

#     def test_initialised_public_data_components_no_comments(self):
#         self.fs.create_file(
#             "/fake/path/types.f90",
#             contents="""\
#  module simple_module
#    implicit none
#    private

#    public :: simple_type

#    type :: simple_type
#      real, public :: x = 10.0
#      integer, public :: y
#    contains
#      procedure, public :: init
#      procedure, public :: get_x
#      procedure, public :: get_y
#    end type simple_type
# end module simple_module
#  """,
#         )
#         result = extract_module_data([Path("/fake/path/types.f90")])
#         self.assertEqual(len(result), 1)
#         module = result[0]
#         self.assertEqual(module["module_name"], "simple_module")
#         types = module["types"]
#         self.assertEqual(len(types), 1)
#         type = types["simple_type"]
#         self.assertEqual(type["type_name"], "simple_type")
#         self.assertEqual(type["attributes"], ["public"])
#         procedures = type["procedures"]
#         self.assertEqual(procedures["init"]["name"], "init")
#         self.assertEqual(procedures["init"]["description"], "")
#         data_components = type["data_components"]
#         self.assertEqual(len(data_components), 2)
#         self.assertEqual(data_components["x"]["name"], "x")
#         self.assertEqual(data_components["x"]["type"], "real")
#         self.assertEqual(data_components["x"]["description"], "")
#         self.assertEqual(data_components["x"]["dimension"], None)
#         self.assertEqual(data_components["x"]["initial_value"], "10.0")
#         self.assertEqual(data_components["x"]["attributes"], ["public"])
#         self.assertEqual(data_components["y"]["name"], "y")
#         self.assertEqual(data_components["y"]["type"], "integer")
#         self.assertEqual(data_components["y"]["description"], "")
#         self.assertEqual(data_components["y"]["dimension"], None)
#         self.assertEqual(data_components["y"]["initial_value"], None)
#         self.assertEqual(data_components["y"]["attributes"], ["public"])
#         self.assertEqual(type["description"], "")
#         self.assertIsNone(type["extends"])
#         self.assertEqual(type["generic_interfaces"], {})

#     def test_initialised_public_data_components_with_comments(self):
#         self.fs.create_file(
#             "/fake/path/types.f90",
#             contents="""\
#  module simple_module
#    implicit none
#    private

#    public :: simple_type

#    type :: simple_type
#      !!* x is 10 *!
#      real, public :: x = 10.0
#      !!* y is not initialised *!
#      integer, public :: y
#    contains
#      procedure, public :: init
#      procedure, public :: get_x
#      procedure, public :: get_y
#    end type simple_type
# end module simple_module
#  """,
#         )
#         result = extract_module_data([Path("/fake/path/types.f90")])
#         self.assertEqual(len(result), 1)
#         module = result[0]
#         self.assertEqual(module["module_name"], "simple_module")
#         types = module["types"]
#         self.assertEqual(len(types), 1)
#         type = types["simple_type"]
#         self.assertEqual(type["type_name"], "simple_type")
#         self.assertEqual(type["attributes"], ["PUBLIC"])
#         procedures = type["procedures"]
#         self.assertEqual(procedures["init"]["name"], "init")
#         self.assertEqual(procedures["init"]["description"], "")
#         data_components = type["data_components"]
#         self.assertEqual(len(data_components), 2)
#         self.assertEqual(data_components["x"]["name"], "x")
#         self.assertEqual(data_components["x"]["type"], "REAL")
#         self.assertEqual(data_components["x"]["description"], "x is 10\n")
#         self.assertEqual(data_components["x"]["dimension"], None)
#         self.assertEqual(data_components["x"]["initial_value"], "10.0")
#         self.assertEqual(data_components["x"]["attributes"], ["PUBLIC"])
#         self.assertEqual(data_components["y"]["name"], "y")
#         self.assertEqual(data_components["y"]["type"], "INTEGER")
#         self.assertEqual(data_components["y"]["description"], "y is not initialised\n")
#         self.assertEqual(data_components["y"]["dimension"], None)
#         self.assertEqual(data_components["y"]["initial_value"], None)
#         self.assertEqual(data_components["y"]["attributes"], ["PUBLIC"])
#         self.assertEqual(type["description"], "")
#         self.assertIsNone(type["extends"])
#         self.assertEqual(type["generic_interfaces"], {})

#     def test_public_and_private_data_components_no_comments(self):
#         self.fs.create_file(
#             "/fake/path/types.f90",
#             contents="""\
#  module simple_module
#    implicit none
#    private

#    public :: simple_type

#    type :: simple_type
#      real, public :: x
#      integer, private :: y
#    contains
#      procedure, public :: init
#      procedure, public :: get_x
#      procedure, public :: get_y
#    end type simple_type
# end module simple_module
#  """,
#         )
#         result = extract_module_data([Path("/fake/path/types.f90")])
#         self.assertEqual(len(result), 1)
#         module = result[0]
#         self.assertEqual(module["module_name"], "simple_module")
#         types = module["types"]
#         self.assertEqual(len(types), 1)
#         type = types["simple_type"]
#         self.assertEqual(type["type_name"], "simple_type")
#         self.assertEqual(type["attributes"], ["PUBLIC"])
#         procedures = type["procedures"]
#         self.assertEqual(procedures["init"]["name"], "init")
#         self.assertEqual(procedures["init"]["description"], "")
#         data_components = type["data_components"]
#         self.assertEqual(len(data_components), 2)
#         self.assertEqual(len(data_components), 2)
#         self.assertEqual(data_components["x"]["name"], "x")
#         self.assertEqual(data_components["x"]["type"], "REAL")
#         self.assertEqual(data_components["x"]["description"], "")
#         self.assertEqual(data_components["x"]["dimension"], None)
#         self.assertEqual(data_components["x"]["initial_value"], None)
#         self.assertEqual(data_components["x"]["attributes"], ["PUBLIC"])
#         self.assertEqual(data_components["y"]["name"], "y")
#         self.assertEqual(data_components["y"]["type"], "INTEGER")
#         self.assertEqual(data_components["y"]["description"], "")
#         self.assertEqual(data_components["y"]["dimension"], None)
#         self.assertEqual(data_components["y"]["initial_value"], None)
#         self.assertEqual(data_components["y"]["attributes"], ["PRIVATE"])
#         self.assertEqual(type["description"], "")
#         self.assertIsNone(type["extends"])
#         self.assertEqual(type["generic_interfaces"], {})

#     def test_public_and_private_data_components_with_comments(self):
#         self.fs.create_file(
#             "/fake/path/types.f90",
#             contents="""\
#  module simple_module
#    implicit none
#    private

#    public :: simple_type

#    type :: simple_type
#      !!* declares x *!
#      real, public :: x
#      !!* declares y *!
#      integer, private :: y
#    contains
#      procedure, public :: init
#      procedure, public :: get_x
#      procedure, public :: get_y
#    end type simple_type
# end module simple_module
#  """,
#         )
#         result = extract_module_data([Path("/fake/path/types.f90")])
#         self.assertEqual(len(result), 1)
#         module = result[0]
#         self.assertEqual(module["module_name"], "simple_module")
#         types = module["types"]
#         self.assertEqual(len(types), 1)
#         type = types["simple_type"]
#         self.assertEqual(type["type_name"], "simple_type")
#         self.assertEqual(type["attributes"], ["PUBLIC"])
#         procedures = type["procedures"]
#         self.assertEqual(procedures["init"]["name"], "init")
#         self.assertEqual(procedures["init"]["description"], "")
#         data_components = type["data_components"]
#         self.assertEqual(len(data_components), 2)
#         self.assertEqual(len(data_components), 2)
#         self.assertEqual(data_components["x"]["name"], "x")
#         self.assertEqual(data_components["x"]["type"], "REAL")
#         self.assertEqual(data_components["x"]["description"], "declares x\n")
#         self.assertEqual(data_components["x"]["dimension"], None)
#         self.assertEqual(data_components["x"]["initial_value"], None)
#         self.assertEqual(data_components["x"]["attributes"], ["PUBLIC"])
#         self.assertEqual(data_components["y"]["name"], "y")
#         self.assertEqual(data_components["y"]["type"], "INTEGER")
#         self.assertEqual(data_components["y"]["description"], "declares y\n")
#         self.assertEqual(data_components["y"]["dimension"], None)
#         self.assertEqual(data_components["y"]["initial_value"], None)
#         self.assertEqual(data_components["y"]["attributes"], ["PRIVATE"])
#         self.assertEqual(type["description"], "")
#         self.assertIsNone(type["extends"])
#         self.assertEqual(type["generic_interfaces"], {})

#     def test_array_data_components_no_comments(self):
#         self.fs.create_file(
#             "/fake/path/types.f90",
#             contents="""\
# module array_module
# implicit none
# private

# public :: array_type

# type :: array_type
#     real, dimension(3,3), public :: matrix
#     integer, dimension(:), allocatable, public :: vector
#     complex, dimension(2,2,2), public :: tensor
# contains
#     procedure, public :: init
# end type array_type
# end module array_module
#     """,
#         )
#         result = extract_module_data([Path("/fake/path/types.f90")])
#         self.assertEqual(len(result), 1)
#         module = result[0]
#         self.assertEqual(module["module_name"], "array_module")
#         types = module["types"]
#         self.assertEqual(len(types), 1)
#         type = types["array_type"]
#         self.assertEqual(type["type_name"], "array_type")
#         self.assertEqual(type["attributes"], ["public"])        
#         data_components = type["data_components"]
#         self.assertEqual(len(data_components), 3)
#         self.assertEqual(data_components["matrix"]["name"], "matrix")
#         self.assertEqual(data_components["matrix"]["type"], "real")
#         self.assertEqual(data_components["matrix"]["dimension"], { "dimensions": [3, 3] })
#         self.assertEqual(data_components["matrix"]["attributes"], ["public"])
#         self.assertEqual(data_components["matrix"]["description"], "")
#         self.assertEqual(data_components["vector"]["name"], "vector")
#         self.assertEqual(data_components["vector"]["type"], "integer")
#         self.assertEqual(data_components["vector"]["dimension"], { "dimensions": [":"] })
#         self.assertEqual(data_components["vector"]["attributes"], ["allocatable", "public"])
#         self.assertEqual(data_components["vector"]["description"], "")
#         self.assertEqual(data_components["tensor"]["name"], "tensor")
#         self.assertEqual(data_components["tensor"]["type"], "complex")
#         self.assertEqual(data_components["tensor"]["dimension"], { "dimensions": [2, 2, 2] })
#         self.assertEqual(data_components["tensor"]["attributes"], ["public"])
#         self.assertEqual(data_components["tensor"]["description"], "")
#         procedures = type["procedures"]
#         self.assertEqual(len(procedures), 1)
#         self.assertEqual(procedures["init"]["name"], "init")

#     def test_array_data_components_with_comments(self):
#         self.fs.create_file(
#             "/fake/path/types.f90",
#             contents="""\
# module array_module
# implicit none
# private

# public :: array_type

# type :: array_type
# !!* a matrix *!
#     real, dimension(3,3), public :: matrix
# !!*
# ! a vector 
# !*!
#     integer, dimension(:), allocatable, public :: vector
# !!* a tensor
# !*!
#     complex, dimension(2,2,2), public :: tensor
# contains
#     procedure, public :: init
# end type array_type
# end module array_module
#     """,
#         )
#         result = extract_module_data([Path("/fake/path/types.f90")])
#         self.assertEqual(len(result), 1)
#         module = result[0]
#         self.assertEqual(module["module_name"], "array_module")
#         types = module["types"]
#         self.assertEqual(len(types), 1)
#         type = types["array_type"]
#         self.assertEqual(type["type_name"], "array_type")
#         self.assertEqual(type["attributes"], ["public"])        
#         data_components = type["data_components"]
#         self.assertEqual(len(data_components), 3)
#         self.assertEqual(data_components["matrix"]["name"], "matrix")
#         self.assertEqual(data_components["matrix"]["type"], "real")
#         self.assertEqual(data_components["matrix"]["dimension"], { "dimensions": [3, 3] })
#         self.assertEqual(data_components["matrix"]["attributes"], ["public"])
#         self.assertEqual(data_components["matrix"]["description"], "a matrix\n")
#         self.assertEqual(data_components["vector"]["name"], "vector")
#         self.assertEqual(data_components["vector"]["type"], "integer")
#         self.assertEqual(data_components["vector"]["dimension"], { "dimensions": [":"] })
#         self.assertEqual(data_components["vector"]["attributes"], ["allocatable", "public"])
#         self.assertEqual(data_components["vector"]["description"], "\na vector\n\n")
#         self.assertEqual(data_components["tensor"]["name"], "tensor")
#         self.assertEqual(data_components["tensor"]["type"], "complex")
#         self.assertEqual(data_components["tensor"]["dimension"], { "dimensions": [2, 2, 2] })
#         self.assertEqual(data_components["tensor"]["attributes"], ["public"])
#         self.assertEqual(data_components["tensor"]["description"], "a tensor\n\n")
#         procedures = type["procedures"]
#         self.assertEqual(len(procedures), 1)
#         self.assertEqual(procedures["init"]["name"], "init")

#     def test_character_data_components_no_comments(self):
#         self.fs.create_file(
#             "/fake/path/types.f90",
#             contents="""\
#     module character_module
#     implicit none
#     private

#     public :: character_type

#     type :: character_type
#         character(len=20), public :: fixed_length_string
#         character(len=:), allocatable, public :: variable_length_string
#         character(kind=selected_char_kind("ISO_10646")), public :: unicode_char
#         character(len=10, kind=selected_char_kind("ASCII")), public :: ascii_string
#     contains
#         procedure, public :: init
#     end type character_type
#     end module character_module
#     """,
#         )
#         result = extract_module_data([Path("/fake/path/types.f90")])
#         self.assertEqual(len(result), 1)
#         module = result[0]
#         self.assertEqual(module["module_name"], "character_module")
#         types = module["types"]
#         self.assertEqual(len(types), 1)
#         type = types["character_type"]
#         self.assertEqual(type["type_name"], "character_type")
#         self.assertEqual(type["attributes"], ["public"])
#         data_components = type["data_components"]
#         self.assertEqual(len(data_components), 4)
#         self.assertEqual(data_components["fixed_length_string"]["name"], "fixed_length_string")
#         self.assertEqual(data_components["fixed_length_string"]["type"], "character")
#         self.assertEqual(data_components["fixed_length_string"]["len"], "20")
#         self.assertEqual(data_components["fixed_length_string"]["attributes"], ["public"])
#         self.assertEqual(data_components["variable_length_string"]["name"], "variable_length_string")
#         self.assertEqual(data_components["variable_length_string"]["type"], "character")
#         self.assertEqual(data_components["variable_length_string"]["attributes"], ["allocatable", "public"])
#         self.assertEqual(data_components["variable_length_string"]["len"], ":")
#         self.assertEqual(data_components["unicode_char"]["name"], "unicode_char")
#         self.assertEqual(data_components["unicode_char"]["type"], "character")
#         self.assertEqual(data_components["unicode_char"]["attributes"], ["public"])
#         self.assertIsNone(data_components["unicode_char"]["len"])
#         self.assertEqual(data_components["ascii_string"]["name"], "ascii_string")
#         self.assertEqual(data_components["ascii_string"]["type"], "character")
#         self.assertEqual(data_components["ascii_string"]["attributes"], ["public"])
#         self.assertEqual(data_components["ascii_string"]["len"], "10")
#         procedures = type["procedures"]
#         self.assertEqual(len(procedures), 1)
#         self.assertEqual(procedures["init"]["name"], "init")

#     def test_character_data_components_with_comments(self):
#         self.fs.create_file(
#             "/fake/path/types.f90",
#             contents="""\
#     module character_module
#     implicit none
#     private

#     public :: character_type

#     type :: character_type
#         !!* The first one *!
#         character(len=20), public :: fixed_length_string
#         !!* The second one *!
#         character(len=:), allocatable, public :: variable_length_string
#         !!* The third one *!
#         character(kind=selected_char_kind("ISO_10646")), public :: unicode_char
#         !!* The fourth one *!
#         character(len=10, kind=selected_char_kind("ASCII")), public :: ascii_string
#         !!* The fifth one *!
#         character(kind=selected_char_kind("ASCII"), len=30), public :: other_ascii_string
#     contains
#         procedure, public :: init
#     end type character_type
#     end module character_module
#     """,
#         )
#         result = extract_module_data([Path("/fake/path/types.f90")])
#         self.assertEqual(len(result), 1)
#         module = result[0]
#         self.assertEqual(module["module_name"], "character_module")
#         types = module["types"]
#         self.assertEqual(len(types), 1)
#         type = types["character_type"]
#         self.assertEqual(type["type_name"], "character_type")
#         self.assertEqual(type["attributes"], ["public"])
#         data_components = type["data_components"]
#         self.assertEqual(len(data_components), 5)
#         self.assertEqual(data_components["fixed_length_string"]["name"], "fixed_length_string")
#         self.assertEqual(data_components["fixed_length_string"]["type"], "character")
#         self.assertEqual(data_components["fixed_length_string"]["len"], "20")
#         self.assertEqual(data_components["fixed_length_string"]["attributes"], ["public"])
#         self.assertEqual(data_components["fixed_length_string"]["description"], "The first one\n")
#         self.assertEqual(data_components["variable_length_string"]["name"], "variable_length_string")
#         self.assertEqual(data_components["variable_length_string"]["type"], "character")
#         self.assertEqual(data_components["variable_length_string"]["attributes"], ["allocatable", "public"])
#         self.assertEqual(data_components["variable_length_string"]["description"], "The second one\n")
#         self.assertEqual(data_components["unicode_char"]["name"], "unicode_char")
#         self.assertEqual(data_components["unicode_char"]["type"], "character")
#         self.assertEqual(data_components["unicode_char"]["attributes"], ["public"])
#         self.assertEqual(data_components["unicode_char"]["description"], "The third one\n")        
#         self.assertEqual(data_components["ascii_string"]["name"], "ascii_string")
#         self.assertEqual(data_components["ascii_string"]["type"], "character")
#         self.assertEqual(data_components["ascii_string"]["attributes"], ["public"])
#         self.assertEqual(data_components["ascii_string"]["description"], "The fourth one\n")
#         self.assertEqual(data_components["ascii_string"]["len"], "10")
#         self.assertEqual(data_components["other_ascii_string"]["name"], "other_ascii_string")
#         self.assertEqual(data_components["other_ascii_string"]["type"], "character")
#         self.assertEqual(data_components["other_ascii_string"]["attributes"], ["public"])
#         self.assertEqual(data_components["other_ascii_string"]["description"], "The fifth one\n")
#         self.assertEqual(data_components["other_ascii_string"]["len"], "30")
#         procedures = type["procedures"]
#         self.assertEqual(len(procedures), 1)
#         self.assertEqual(procedures["init"]["name"], "init")

#     def test_kind_specification_components_no_comments(self):
#         self.fs.create_file(
#             "/fake/path/types.f90",
#             contents="""\
#     module kind_module
#     use, intrinsic :: iso_fortran_env, only: sp => real32, dp => real64, int64
#     implicit none
#     private

#     public :: kind_type

#     type :: kind_type
#         integer(kind=int64), public :: big_int
#         real(kind=sp), public :: single_precision
#         real(kind=dp), public :: double_precision
#         complex, public :: complex_dp
#         logical(kind=1), public :: small_logical
#     contains
#         procedure, public :: init
#     end type kind_type
#     end module kind_module
#     """,
#         )
#         result = extract_module_data([Path("/fake/path/types.f90")])
#         self.assertEqual(len(result), 1)
#         module = result[0]
#         self.assertEqual(module["module_name"], "kind_module")
#         types = module["types"]
#         self.assertEqual(len(types), 1)
#         type = types["kind_type"]
#         self.assertEqual(type["type_name"], "kind_type")
#         self.assertEqual(type["attributes"], ["public"])
#         data_components = type["data_components"]
#         self.assertEqual(len(data_components), 5)
#         self.assertEqual(data_components["big_int"]["name"], "big_int")
#         self.assertEqual(data_components["big_int"]["type"], "integer")
#         self.assertEqual(data_components["big_int"]["kind"], "int64")
#         self.assertEqual(data_components["big_int"]["attributes"], ["public"])
#         self.assertEqual(data_components["single_precision"]["name"], "single_precision")
#         self.assertEqual(data_components["single_precision"]["type"], "real")
#         self.assertEqual(data_components["single_precision"]["kind"], "sp")
#         self.assertEqual(data_components["single_precision"]["attributes"], ["public"])
#         self.assertEqual(data_components["double_precision"]["name"], "double_precision")
#         self.assertEqual(data_components["double_precision"]["type"], "real")
#         self.assertEqual(data_components["double_precision"]["kind"], "dp")
#         self.assertEqual(data_components["double_precision"]["attributes"], ["public"])
#         self.assertEqual(data_components["complex_dp"]["name"], "complex_dp")
#         self.assertEqual(data_components["complex_dp"]["type"], "complex")
#         self.assertIsNone(data_components["complex_dp"]["kind"])
#         self.assertEqual(data_components["complex_dp"]["attributes"], ["public"])
#         self.assertEqual(data_components["small_logical"]["name"], "small_logical")
#         self.assertEqual(data_components["small_logical"]["type"], "logical")
#         self.assertEqual(data_components["small_logical"]["kind"], "1")
#         self.assertEqual(data_components["small_logical"]["attributes"], ["public"])
#         procedures = type["procedures"]
#         self.assertEqual(len(procedures), 1)
#         self.assertEqual(procedures["init"]["name"], "init")

#     def test_kind_specification_components_with_comments(self):
#         self.fs.create_file(
#             "/fake/path/types.f90",
#             contents="""\
#     module kind_module
#     use, intrinsic :: iso_fortran_env, only: sp => real32, dp => real64, int64
#     implicit none
#     private

#     public :: kind_type

#     type :: kind_type
#     !!* one *!
#         integer(kind=int64), public :: big_int
#     !!* two *!
#         real(kind=sp), public :: single_precision
#     !!* three *!
#         real(kind=dp), public :: double_precision
#     !!* four *!
#         complex, public :: complex_dp
#     !!* five *!
#         logical(kind=1), public :: small_logical
#     contains
#         procedure, public :: init
#     end type kind_type
#     end module kind_module
#     """,
#         )
#         result = extract_module_data([Path("/fake/path/types.f90")])
#         self.assertEqual(len(result), 1)
#         module = result[0]
#         self.assertEqual(module["module_name"], "kind_module")
#         types = module["types"]
#         self.assertEqual(len(types), 1)
#         type = types["kind_type"]
#         self.assertEqual(type["type_name"], "kind_type")
#         self.assertEqual(type["attributes"], ["PUBLIC"])
#         data_components = type["data_components"]
#         self.assertEqual(len(data_components), 5)
#         self.assertEqual(data_components["big_int"]["name"], "big_int")
#         self.assertEqual(data_components["big_int"]["type"], "INTEGER")
#         self.assertEqual(data_components["big_int"]["kind"], "int64")
#         self.assertEqual(data_components["big_int"]["attributes"], ["PUBLIC"])
#         self.assertEqual(data_components["big_int"]["description"], "one\n")
#         self.assertEqual(data_components["single_precision"]["name"], "single_precision")
#         self.assertEqual(data_components["single_precision"]["type"], "REAL")
#         self.assertEqual(data_components["single_precision"]["kind"], "sp")
#         self.assertEqual(data_components["single_precision"]["attributes"], ["PUBLIC"])
#         self.assertEqual(data_components["single_precision"]["description"], "two\n")
#         self.assertEqual(data_components["double_precision"]["name"], "double_precision")
#         self.assertEqual(data_components["double_precision"]["type"], "reREALal")
#         self.assertEqual(data_components["double_precision"]["kind"], "dp")
#         self.assertEqual(data_components["double_precision"]["attributes"], ["PUBLIC"])
#         self.assertEqual(data_components["double_precision"]["description"], "three\n")
#         self.assertEqual(data_components["complex_dp"]["name"], "complex_dp")
#         self.assertEqual(data_components["complex_dp"]["type"], "complex")
#         self.assertIsNone(data_components["complex_dp"]["kind"])
#         self.assertEqual(data_components["complex_dp"]["attributes"], ["PUBLIC"])
#         self.assertEqual(data_components["complex_dp"]["description"], "four\n")
#         self.assertEqual(data_components["small_logical"]["name"], "small_logical")
#         self.assertEqual(data_components["small_logical"]["type"], "LOGICAL")
#         self.assertEqual(data_components["small_logical"]["kind"], "1")
#         self.assertEqual(data_components["small_logical"]["attributes"], ["public"])
#         self.assertEqual(data_components["small_logical"]["description"], "five\n")        
#         procedures = type["procedures"]
#         self.assertEqual(len(procedures), 1)
#         self.assertEqual(procedures["init"]["name"], "init")

#     def test_array_with_initialization(self):
#         self.fs.create_file(
#             "/fake/path/types.f90",
#             contents="""\
#     module array_init_module
#     implicit none
#     private

#     public :: array_init_type

#     type :: array_init_type
#         integer, dimension(2), public :: int_arr = [1, 2]
#         real, dimension(3), public :: real_arr = [1.0, 2.0, 3.0]
#         logical, dimension(2,2), public :: log_arr = reshape([.true., .false., .false., .true.], [2,2])
#         character(len=5), dimension(2), public :: char_arr = ["Hello", "World"]
#     contains
#         procedure, public :: init
#     end type array_init_type
#     end module array_init_module
#     """,
#         )
#         result = extract_module_data([Path("/fake/path/types.f90")])
#         self.assertEqual(len(result), 1)
#         module = result[0]
#         self.assertEqual(module["module_name"], "array_init_module")
#         types = module["types"]
#         self.assertEqual(len(types), 1)
#         type = types["array_init_type"]
#         self.assertEqual(type["type_name"], "array_init_type")
#         self.assertEqual(type["attributes"], ["public"])
        
#         data_components = type["data_components"]
#         self.assertEqual(len(data_components), 4)
#         self.assertEqual(data_components["int_arr"]["name"], "int_arr")
#         self.assertEqual(data_components["int_arr"]["type"], "integer")
#         self.assertEqual(data_components["int_arr"]["dimension"], {"dimensions": [2]})
#         self.assertEqual(data_components["int_arr"]["initial_value"], "[1, 2]")
#         self.assertEqual(data_components["int_arr"]["attributes"], ["public"])
#         self.assertEqual(data_components["real_arr"]["name"], "real_arr")
#         self.assertEqual(data_components["real_arr"]["type"], "real")
#         self.assertEqual(data_components["real_arr"]["dimension"], {"dimensions": [3]})
#         self.assertEqual(data_components["real_arr"]["initial_value"], "[1.0, 2.0, 3.0]")
#         self.assertEqual(data_components["real_arr"]["attributes"], ["public"])
#         self.assertEqual(data_components["log_arr"]["name"], "log_arr")
#         self.assertEqual(data_components["log_arr"]["type"], "logical")
#         self.assertEqual(data_components["log_arr"]["dimension"], {"dimensions": [2, 2]})
#         self.assertEqual(data_components["log_arr"]["initial_value"], "reshape([.true., .false., .false., .true.], [2,2])")
#         self.assertEqual(data_components["log_arr"]["attributes"], ["public"])
#         self.assertEqual(data_components["char_arr"]["name"], "char_arr")
#         self.assertEqual(data_components["char_arr"]["type"], "character")
#         self.assertEqual(data_components["char_arr"]["dimension"], {"dimensions": [2]})
#         self.assertEqual(data_components["char_arr"]["initial_value"], '["Hello", "World"]')
#         self.assertEqual(data_components["char_arr"]["attributes"], ["public"])    
#         procedures = type["procedures"]
#         self.assertEqual(len(procedures), 1)
#         self.assertEqual(procedures["init"]["name"], "init")

#     def test_allocatable_arrays(self):
#         self.fs.create_file(
#             "/fake/path/types.f90",
#             contents="""\
#     module allocatable_module
#     implicit none
#     private

#     public :: allocatable_type

#     type :: allocatable_type
#         real, allocatable, public :: alloc_x(:)
#         integer, allocatable, public :: alloc_matrix(:,:)
#         character(len=:), allocatable, public :: alloc_string
#         complex, allocatable, public :: alloc_complex(:,:,:)
#         logical, allocatable, public :: alloc_logical(:)
#     contains
#         procedure, public :: init
#     end type allocatable_type
#     end module allocatable_module
#     """,
#         )
#         result = extract_module_data([Path("/fake/path/types.f90")])
#         self.assertEqual(len(result), 1)
#         module = result[0]
#         self.assertEqual(module["module_name"], "allocatable_module")
#         types = module["types"]
#         self.assertEqual(len(types), 1)
#         type = types["allocatable_type"]
#         self.assertEqual(type["type_name"], "allocatable_type")
#         self.assertEqual(type["attributes"], ["public"])
#         data_components = type["data_components"]
#         self.assertEqual(len(data_components), 5)
#         self.assertEqual(data_components["alloc_x"]["name"], "alloc_x")
#         self.assertEqual(data_components["alloc_x"]["type"], "real")
#         self.assertEqual(data_components["alloc_x"]["dimension"], {"dimensions": [":"]})
#         self.assertEqual(data_components["alloc_x"]["attributes"], ["allocatable", "public"])
#         self.assertEqual(data_components["alloc_matrix"]["name"], "alloc_matrix")
#         self.assertEqual(data_components["alloc_matrix"]["type"], "integer")
#         self.assertEqual(data_components["alloc_matrix"]["dimension"], {"dimensions": [":", ":"]})
#         self.assertEqual(data_components["alloc_matrix"]["attributes"], ["allocatable", "public"])
#         self.assertEqual(data_components["alloc_string"]["name"], "alloc_string")
#         self.assertEqual(data_components["alloc_string"]["type"], "character")
#         self.assertEqual(data_components["alloc_string"]["attributes"], ["allocatable", "public"])
#         self.assertEqual(data_components["alloc_complex"]["name"], "alloc_complex")
#         self.assertEqual(data_components["alloc_complex"]["type"], "complex")
#         self.assertEqual(data_components["alloc_complex"]["dimension"], {"dimensions": [":", ":", ":"]})
#         self.assertEqual(data_components["alloc_complex"]["attributes"], ["allocatable", "public"])
#         self.assertEqual(data_components["alloc_logical"]["name"], "alloc_logical")
#         self.assertEqual(data_components["alloc_logical"]["type"], "logical")
#         self.assertEqual(data_components["alloc_logical"]["dimension"], {"dimensions": [":"]})
#         self.assertEqual(data_components["alloc_logical"]["attributes"], ["allocatable", "public"])
#         procedures = type["procedures"]
#         self.assertEqual(len(procedures), 1)
#         self.assertEqual(procedures["init"]["name"], "init")
        
#     def test_public_data_components_declared_first_no_comments(self):
#         self.fs.create_file(
#             "/fake/path/types.f90",
#             contents="""\
#  module simple_module
#    implicit none
#    private

#    ! Public type declaration
#    public :: simple_type
#    public :: simple_type%x, simple_type%y

#    type :: simple_type
#      real :: x
#      integer :: y
#    contains
#      procedure, public :: init
#      procedure, public :: get_x
#      procedure, public :: get_y
#    end type simple_type
# end module simple_module
#  """,
#         )
#         result = extract_module_data([Path("/fake/path/types.f90")])
#         self.assertEqual(len(result), 1)
#         module = result[0]
#         self.assertEqual(module["module_name"], "types")
#         types = module["types"]
#         self.assertEqual(len(types), 1)
#         type = types["simple_type"]
#         self.assertEqual(type["type_name"], "simple_type")
#         self.assertEqual(type["attributes"], ["public"])
#         procedures = type["procedures"]
#         self.assertEqual(procedures["init"]["name"], "init")
#         self.assertEqual(procedures["init"]["description"], "")
#         self.assertEqual(type["data_components"], {})
#         self.assertEqual(type["description"], "")
#         self.assertIsNone(type["extends"])
#         self.assertEqual(type["generic_interfaces"], {})

#     def test_public_data_components_declared_last_no_comments(self):
#         self.fs.create_file(
#             "/fake/path/types.f90",
#             contents="""\
#  module simple_module
#    implicit none
#    private

#    type :: simple_type
#      real :: x
#      integer :: y
#    contains
#      procedure, public :: init
#      procedure, public :: get_x
#      procedure, public :: get_y
#    end type simple_type

#    public :: simple_type
#    public :: simple_type%x, simple_type%y

# end module simple_module
#  """,
#         )
#         result = extract_module_data([Path("/fake/path/types.f90")])
#         self.assertEqual(len(result), 1)
#         module = result[0]
#         self.assertEqual(module["module_name"], "types")
#         types = module["types"]
#         self.assertEqual(len(types), 1)
#         type = types["simple_type"]
#         self.assertEqual(type["type_name"], "simple_type")
#         self.assertEqual(type["attributes"], ["public"])
#         procedures = type["procedures"]
#         self.assertEqual(procedures["init"]["name"], "init")
#         self.assertEqual(procedures["init"]["description"], "")
#         self.assertEqual(type["data_components"], {})
#         self.assertEqual(type["description"], "")
#         self.assertIsNone(type["extends"])
#         self.assertEqual(type["generic_interfaces"], {})

if __name__ == "__main__":
    unittest.main()
