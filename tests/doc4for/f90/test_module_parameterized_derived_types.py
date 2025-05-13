import unittest
from pathlib import Path
from pyfakefs.fake_filesystem_unittest import TestCase
from doc4for.f90.generate_module_tree import extract_module_data

class TestParameterizedTypes(TestCase):

    def setUp(self):
        self.setUpPyfakefs()

    def test_parameterized_derived_type(self):
        self.fs.create_file(
            "/fake/path/pdt.f90",
            contents="""\
    module pdt_module
        implicit none
        private
        public :: string, matrix

        !!* String type with configurable length *!
        type :: string(len)
            !!* Length parameter specifies string capacity *!
            integer, len :: len
            !!* The character data with parameterized length *!
            character(len=len) :: data
        contains
            !!* Initialize the string with a value *!
            procedure :: init => init_string
            !!* Get the string length *!
            procedure :: length => string_length
        end type string

        !!* Matrix type with configurable precision and dimensions *!
        type :: matrix(k, rows, cols)
            !!* Kind parameter for precision control *!
            integer, kind :: k = kind(1.0)  ! Default kind
            !!* Number of rows in the matrix *!
            integer, len :: rows
            !!* Number of columns in the matrix *!
            integer, len :: cols
            !!* Matrix data with parameterized kind and dimensions *!
            real(kind=k), allocatable :: data(:,:)
        contains
            !!* Initialize the matrix with zeroes *!
            procedure :: init => init_matrix
        end type matrix

    contains
        !!* Initialize string with a value, truncating if needed *!
        subroutine init_string(this, value)
            class(string), intent(inout) :: this
            character(len=*), intent(in) :: value
            this%data = value
        end subroutine init_string

        !!* Get the current string length *!
        function string_length(this) result(length)
            class(string), intent(in) :: this
            integer :: length
            length = len_trim(this%data)
        end function string_length

        !!* Initialize the matrix with all zeroes *!
        subroutine init_matrix(this)
            class(matrix), intent(inout) :: this
            allocate(this%data(this%rows, this%cols))
            this%data = real(0.0, kind=this%k)  ! Use real() intrinsic with kind= argument
        end subroutine init_matrix
    end module pdt_module
    """,
        )
        result = extract_module_data([Path("/fake/path/pdt.f90")])
        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(module["module_name"], "pdt_module")
        types = module["types"]
        self.assertEqual(len(types), 2)
        
        # Test string type
        string_type = types["string"]
        self.assertEqual(string_type["type_name"], "string")
        self.assertEqual(string_type["attributes"], ["PUBLIC"])
        self.assertEqual(string_type["description"], "String type with configurable length\n")
        
        # Check type parameters
        self.assertEqual(len(string_type["type_parameters"]), 1)
        len_param = string_type["type_parameters"]["len"]
        self.assertEqual(len_param["name"], "len")
        self.assertEqual(len_param["type"], "INTEGER")
        self.assertEqual(len_param["parameter_type"], "LEN")
        self.assertEqual(len_param["description"], "Length parameter specifies string capacity\n")
        
        # Check data components
        self.assertEqual(len(string_type["data_components"]), 1)
        data_comp = string_type["data_components"]["data"]
        self.assertEqual(data_comp["name"], "data")
        self.assertEqual(data_comp["type"], "CHARACTER")
        self.assertEqual(data_comp["len"], "len")  # Parameterized length
        self.assertEqual(data_comp["description"], "The character data with parameterized length\n")
        
        # Check procedures
        self.assertEqual(len(string_type["procedures"]), 2)
        init_proc = string_type["procedures"]["init"]
        self.assertEqual(init_proc["name"], "init")
        self.assertEqual(init_proc["implementation"], "init_string")
        self.assertEqual(init_proc["description"], "Initialize the string with a value\n")
        
        length_proc = string_type["procedures"]["length"]
        self.assertEqual(length_proc["name"], "length")
        self.assertEqual(length_proc["implementation"], "string_length")
        self.assertEqual(length_proc["description"], "Get the string length\n")
        
        # Test matrix type
        matrix_type = types["matrix"]
        self.assertEqual(matrix_type["type_name"], "matrix")
        self.assertEqual(matrix_type["attributes"], ["PUBLIC"])
        self.assertEqual(matrix_type["description"], "Matrix type with configurable precision and dimensions\n")
        
        # Check type parameters
        self.assertEqual(len(matrix_type["type_parameters"]), 3)
        
        k_param = matrix_type["type_parameters"]["k"]
        self.assertEqual(k_param["name"], "k")
        self.assertEqual(k_param["type"], "INTEGER")
        self.assertEqual(k_param["parameter_type"], "KIND")
        self.assertEqual(k_param["default"], "kind(1.0)")
        self.assertEqual(k_param["description"], "Kind parameter for precision control\n")
        
        rows_param = matrix_type["type_parameters"]["rows"]
        self.assertEqual(rows_param["name"], "rows")
        self.assertEqual(rows_param["type"], "INTEGER")
        self.assertEqual(rows_param["parameter_type"], "LEN")
        self.assertEqual(rows_param["description"], "Number of rows in the matrix\n")
        
        cols_param = matrix_type["type_parameters"]["cols"]
        self.assertEqual(cols_param["name"], "cols")
        self.assertEqual(cols_param["type"], "INTEGER")
        self.assertEqual(cols_param["parameter_type"], "LEN")
        self.assertEqual(cols_param["description"], "Number of columns in the matrix\n")
        
        # Check data components
        self.assertEqual(len(matrix_type["data_components"]), 1)
        data_comp = matrix_type["data_components"]["data"]
        self.assertEqual(data_comp["name"], "data")
        self.assertEqual(data_comp["type"], "REAL")
        self.assertEqual(data_comp["kind"], "k")  # Parameterized kind
        self.assertEqual(data_comp["attributes"], ["ALLOCATABLE"])
        self.assertEqual(data_comp["dimension"]["rank"], 2)
        self.assertEqual(data_comp["description"], "Matrix data with parameterized kind and dimensions\n")
        
        # Check procedures
        self.assertEqual(len(matrix_type["procedures"]), 1)
        init_proc = matrix_type["procedures"]["init"]
        self.assertEqual(init_proc["name"], "init")
        self.assertEqual(init_proc["implementation"], "init_matrix")
        self.assertEqual(init_proc["description"], "Initialize the matrix with zeroes\n")


#TODO
# type, extends(base(n=)), private :: derived(m)
#   integer, kind :: m
# end type
if __name__ == "__main__":
    unittest.main()

