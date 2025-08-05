import unittest
from pathlib import Path
from pyfakefs.fake_filesystem_unittest import TestCase
from doc4for.f90.generate_module_tree import extract_module_data
from doc4for.models.dimension_models import ArrayBound, BoundType
from doc4for.models.common import Expression, ExpressionType

class TestParameterizedTypes(TestCase):
    maxDiff=None

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
        self.assertCountEqual(data_comp["attributes"], ["ALLOCATABLE", "PUBLIC"])
        self.assertEqual(data_comp["dimension"]["dimensions"], 
                         [ArrayBound(BoundType.DEFERRED), ArrayBound(BoundType.DEFERRED)])
        self.assertEqual(data_comp["description"], "Matrix data with parameterized kind and dimensions\n")
        
        # Check procedures
        self.assertEqual(len(matrix_type["procedures"]), 1)
        init_proc = matrix_type["procedures"]["init"]
        self.assertEqual(init_proc["name"], "init")
        self.assertEqual(init_proc["implementation"], "init_matrix")
        self.assertEqual(init_proc["description"], "Initialize the matrix with zeroes\n")

    def test_parameterized_derived_type_inheritance(self):
        self.fs.create_file(
            "/fake/path/pdt_inheritance.f90",
            contents="""\
    module pdt_inheritance_module
        implicit none
        private
        public :: base, derived

        !!* Base parameterized type with a single parameter *!
        type :: base(n)
            !!* Length parameter for base type *!
            integer, len :: n
            !!* Base data array with parameterized length *!
            real :: data(n)
        contains
            !!* Initialize base data with a value *!
            procedure :: init_base
        end type base

        !!* Derived type extending base type with its own parameter *!
        type, extends(base), private :: derived(m)
            !!* Kind parameter for additional precision control *!
            integer, kind :: m
            !!* Additional data with parameterized kind *!
            real(kind=m) :: extra_data
        contains
            !!* Initialize both base and derived data *!
            procedure :: init_derived
        end type derived

    contains
        !!* Initialize base data with given value *!
        subroutine init_base(this, value)
            class(base), intent(inout) :: this
            real, intent(in) :: value
            this%data = value
        end subroutine init_base

        !!* Initialize derived type data *!
        subroutine init_derived(this, base_value, extra_value)
            class(derived), intent(inout) :: this
            real, intent(in) :: base_value
            real, intent(in) :: extra_value
            call this%init_base(base_value)
            this%extra_data = real(extra_value, kind=this%m)
        end subroutine init_derived
    end module pdt_inheritance_module
    """,
        )
        
        result = extract_module_data([Path("/fake/path/pdt_inheritance.f90")])
        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(module["module_name"], "pdt_inheritance_module")
        types = module["types"]
        self.assertEqual(len(types), 2)
        
        # Test base type
        base_type = types["base"]
        self.assertEqual(base_type["type_name"], "base")
        self.assertEqual(base_type["attributes"], ["PUBLIC"])
        self.assertEqual(base_type["description"], "Base parameterized type with a single parameter\n")
        self.assertIsNone(base_type["extends"])
        
        # Check base type parameters
        self.assertEqual(len(base_type["type_parameters"]), 1)
        n_param = base_type["type_parameters"]["n"]
        self.assertEqual(n_param["name"], "n")
        self.assertEqual(n_param["type"], "INTEGER")
        self.assertEqual(n_param["parameter_type"], "LEN")
        self.assertIsNone(n_param["default"])
        self.assertEqual(n_param["description"], "Length parameter for base type\n")
        
        # Check base type data component
        base_data = base_type["data_components"]["data"]
        self.assertEqual(base_data["type"], "REAL")
        self.assertEqual(base_data["dimension"]["dimensions"],
                         [ArrayBound(BoundType.VARIABLE, Expression(ExpressionType.LITERAL, "1"), Expression(ExpressionType.VARIABLE, "n"))])
        self.assertEqual(base_data["description"], "Base data array with parameterized length\n")
        
        # Check base type procedure
        base_init = base_type["procedures"]["init_base"]
        self.assertEqual(base_init["name"], "init_base")
        self.assertEqual(base_init["description"], "Initialize base data with a value\n")
        
        # Test derived type
        derived_type = types["derived"]
        self.assertEqual(derived_type["type_name"], "derived")
        #TODO
#        self.assertEqual(derived_type["attributes"], ["PRIVATE"])
        self.assertEqual(derived_type["description"], "Derived type extending base type with its own parameter\n")
        self.assertEqual(derived_type["extends"], "base")
        
        # Check derived type parameters
        self.assertEqual(len(derived_type["type_parameters"]), 1)
        m_param = derived_type["type_parameters"]["m"]
        self.assertEqual(m_param["name"], "m")
        self.assertEqual(m_param["type"], "INTEGER")
        self.assertEqual(m_param["parameter_type"], "KIND")
        self.assertIsNone(m_param["default"])
        self.assertEqual(m_param["description"], "Kind parameter for additional precision control\n")
        
        # Check derived type data component
        derived_extra = derived_type["data_components"]["extra_data"]
        self.assertEqual(derived_extra["type"], "REAL")
        self.assertEqual(derived_extra["kind"], "m")
        self.assertEqual(derived_extra["description"], "Additional data with parameterized kind\n")
        
        # Check derived type procedure
        derived_init = derived_type["procedures"]["init_derived"]
        self.assertEqual(derived_init["name"], "init_derived")
        self.assertEqual(derived_init["description"], "Initialize both base and derived data\n")

    def test_parameterized_derived_type_advanced_features(self):
        self.fs.create_file(
            "/fake/path/pdt_advanced.f90",
            contents="""\
    module pdt_advanced_module
        implicit none
        private
        public :: flexible_array, sparse_matrix, typed_buffer, config_type
        public :: create_flexible_array, create_sparse_matrix

        !!* Flexible array with runtime-determined size and precision *!
        type :: flexible_array(k, n)
            !!* Kind parameter for element precision (default: double) *!
            integer, kind :: k = kind(1.0d0)
            !!* Length parameter for array size *!
            integer, len :: n
            !!* Data array with parameterized kind and size *!
            real(kind=k) :: data(n)
            !!* Workspace array sized as twice the data array *!
            real(kind=k) :: workspace(2*n)
            !!* Status flags for each element *!
            logical :: flags(n)
        contains
            !!* Initialize array with scalar value *!
            procedure :: init => init_flexible_array
            !!* Get the allocated size *!
            procedure :: size => get_array_size
        end type flexible_array

        !!* Sparse matrix with multiple parameterized attributes *!
        type :: sparse_matrix(k, max_entries, rows, cols)
            !!* Precision kind for matrix elements *!
            integer, kind :: k = kind(1.0)
            !!* Storage kind for indices (default: 32-bit) *!
            integer, kind :: idx_kind = int32
            !!* Maximum number of non-zero entries *!
            integer, len :: max_entries
            !!* Number of rows *!
            integer, len :: rows
            !!* Number of columns *!
            integer, len :: cols
            !!* Non-zero values array *!
            real(kind=k), allocatable :: values(:)
            !!* Row indices for non-zero values *!
            integer(kind=idx_kind), allocatable :: row_indices(:)
            !!* Column indices for non-zero values *!
            integer(kind=idx_kind), allocatable :: col_indices(:)
            !!* Current number of stored entries *!
            integer :: nnz = 0
        contains
            !!* Allocate storage for sparse matrix *!
            procedure :: allocate => allocate_sparse_matrix
            !!* Add a non-zero entry *!
            procedure :: add_entry => add_sparse_entry
        end type sparse_matrix

        !!* Generic buffer with configurable element type and size *!
        type :: typed_buffer(elem_kind, elem_size, capacity)
            !!* Kind parameter for buffer elements *!
            integer, kind :: elem_kind
            !!* Size in bytes of each element *!
            integer, kind :: elem_size = 4
            !!* Maximum capacity of the buffer *!
            integer, len :: capacity
            !!* Buffer data storage *!
            integer(kind=elem_kind), allocatable :: data(:)
            !!* Current number of elements in buffer *!
            integer :: count = 0
            !!* Total allocated size in bytes *!
            integer :: total_bytes
        contains
            !!* Resize buffer within capacity limits *!
            procedure :: resize => resize_buffer
            !!* Clear buffer contents *!
            procedure :: clear => clear_buffer
        end type typed_buffer

        !!* Configuration type with nested parameterized components *!
        type :: config_type(name_len, max_options)
            !!* Maximum length for configuration name *!
            integer, len :: name_len = 64
            !!* Maximum number of options *!
            integer, len :: max_options = 100
            !!* Configuration name *!
            character(len=name_len) :: name
            !!* Array of option values *!
            type(flexible_array(kind(1.0), :)), allocatable :: options(:)
            !!* Metadata buffer *!
            type(typed_buffer(int32, 4, :)), allocatable :: metadata
        contains
            !!* Initialize configuration with defaults *!
            procedure :: init => init_config
        end type config_type

    contains
        !!* Create and allocate a flexible array 
        ! @in size Size of the array to create 
        ! @in kind_val Optional kind parameter value 
        ! @return Allocated flexible array 
        !*!
        function create_flexible_array(size, kind_val) result(arr)
            integer, intent(in) :: size
            integer, intent(in), optional :: kind_val
            type(flexible_array(:, :)), allocatable :: arr
            
            if (present(kind_val)) then
                allocate(flexible_array(kind_val, size) :: arr)
            else
                allocate(flexible_array(kind(1.0d0), size) :: arr)
            end if
        end function create_flexible_array

        !!* Create and allocate a sparse matrix 
        ! @in rows Number of rows 
        ! @in cols Number of columns 
        ! @in max_nnz Maximum non-zero entries 
        ! @in precision Optional precision kind 
        !*!
        function create_sparse_matrix(rows, cols, max_nnz, precision) result(matrix)
            integer, intent(in) :: rows
            integer, intent(in) :: cols
            integer, intent(in) :: max_nnz
            integer, intent(in), optional :: precision
            type(sparse_matrix(:, :, :, :)), allocatable :: matrix
            
            if (present(precision)) then
                allocate(sparse_matrix(precision, max_nnz, rows, cols) :: matrix)
            else
                allocate(sparse_matrix(kind(1.0), max_nnz, rows, cols) :: matrix)
            end if
            call matrix%allocate()
        end function create_sparse_matrix

        !!* 
        ! Initialize flexible array with a scalar value 
        ! @inout this Array instance 
        ! @in value Value to initialize with
        !*!
        subroutine init_flexible_array(this, value)
            class(flexible_array), intent(inout) :: this
            real, intent(in) :: value
            
            this%data = real(value, kind=this%k)
            this%workspace = 0.0_k
            this%flags = .false.
        end subroutine init_flexible_array

        !!* Get the size of the flexible array 
        !!* @in this Array instance 
        !!* @return Size of the array 
        !*!
        pure function get_array_size(this) result(size)
            class(flexible_array), intent(in) :: this
            integer :: size
            
            size = this%n
        end function get_array_size

        !!* 
        ! Allocate storage for sparse matrix 
        ! @inout this Sparse matrix instance 
        !*!
        subroutine allocate_sparse_matrix(this)
            class(sparse_matrix), intent(inout) :: this
            
            if (allocated(this%values)) deallocate(this%values)
            if (allocated(this%row_indices)) deallocate(this%row_indices)
            if (allocated(this%col_indices)) deallocate(this%col_indices)
            
            allocate(this%values(this%max_entries))
            allocate(this%row_indices(this%max_entries))
            allocate(this%col_indices(this%max_entries))
            
            this%nnz = 0
        end subroutine allocate_sparse_matrix

        !!* 
        ! Add a non-zero entry to sparse matrix 
        ! @inout this Sparse matrix instance
        ! @in row Row index
        ! @in col Column index
        ! @in value Value to add 
        !*!
        subroutine add_sparse_entry(this, row, col, value)
            class(sparse_matrix), intent(inout) :: this
            integer, intent(in) :: row
            integer, intent(in) :: col
            real, intent(in) :: value
            
            if (this%nnz < this%max_entries .and. &
                row <= this%rows .and. col <= this%cols) then
                this%nnz = this%nnz + 1
                this%values(this%nnz) = real(value, kind=this%k)
                this%row_indices(this%nnz) = int(row, kind=this%idx_kind)
                this%col_indices(this%nnz) = int(col, kind=this%idx_kind)
            end if
        end subroutine add_sparse_entry

        !!* 
        ! Resize buffer maintaining data 
        !*!
        subroutine resize_buffer(this, new_size)
            class(typed_buffer), allocatable, intent(inout) :: this
            integer, intent(in) :: new_size
            type(typed_buffer(this%elem_kind, this%elem_size, :)), allocatable :: temp
            
            if (new_size <= this%capacity .and. new_size > 0) then
                allocate(typed_buffer(this%elem_kind, this%elem_size, new_size) :: temp)
                ! Move data to new buffer
                call move_alloc(from=temp, to=this)
                this%total_bytes = new_size * this%elem_size
            end if
        end subroutine resize_buffer

        !!* Clear buffer contents *!
        subroutine clear_buffer(this)
            class(typed_buffer), intent(inout) :: this
            
            if (allocated(this%data)) then
                this%data = 0
                this%count = 0
            end if
        end subroutine clear_buffer

        !!* Initialize configuration type *!
        subroutine init_config(this, num_options)
            class(config_type), intent(inout) :: this
            integer, intent(in) :: num_options
            integer :: i
            
            this%name = "Default Configuration"
            
            if (allocated(this%options)) deallocate(this%options)
            allocate(this%options(num_options))
            
            do i = 1, num_options
                allocate(flexible_array(kind(1.0), 10) :: this%options(i))
                call this%options(i)%init(0.0)
            end do
            
            if (allocated(this%metadata)) deallocate(this%metadata)
            allocate(typed_buffer(int32, 4, 1024) :: this%metadata)
        end subroutine init_config

    end module pdt_advanced_module
    """,
        )
        
        result = extract_module_data([Path("/fake/path/pdt_advanced.f90")])
        self.assertEqual(len(result), 1)
        module = result[0]
        self.assertEqual(module["module_name"], "pdt_advanced_module")
        
        # Check public procedures
        self.assertIn("create_flexible_array", module["functions"])
        self.assertIn("create_sparse_matrix", module["functions"])
        
        types = module["types"]
        self.assertEqual(len(types), 4)
        
        # Test flexible_array type
        flex_array = types["flexible_array"]
        self.assertEqual(flex_array["type_name"], "flexible_array")
        self.assertEqual(flex_array["attributes"], ["PUBLIC"])
        
        # Check parameters with defaults
        k_param = flex_array["type_parameters"]["k"]
        self.assertEqual(k_param["parameter_type"], "KIND")
        self.assertEqual(k_param["default"], "kind(1.0d0)")
        
        # Check components using parameters in expressions
        workspace = flex_array["data_components"]["workspace"]
        self.assertEqual(workspace["type"], "REAL")
        self.assertEqual(workspace["kind"], "k")
        # The dimension should use the parameter in an expression
        self.assertEqual(workspace["dimension"]["dimensions"],
                        [ArrayBound(BoundType.VARIABLE, 
                                    Expression(ExpressionType.LITERAL, "1"), 
                                    Expression(ExpressionType.LITERAL, "2 * n"))]) 
        
        # Test sparse_matrix with multiple kind parameters
        sparse = types["sparse_matrix"]
        self.assertEqual(len(sparse["type_parameters"]), 5)  # k, idx_kind, max_entries, rows, cols
        
        idx_kind = sparse["type_parameters"]["idx_kind"]
        self.assertEqual(idx_kind["parameter_type"], "KIND")
        self.assertEqual(idx_kind["default"], "int32")
        
        # Check allocatable components with parameterized types
        values = sparse["data_components"]["values"]
        self.assertCountEqual(values["attributes"], ["ALLOCATABLE", "PUBLIC"])
        self.assertEqual(values["kind"], "k")
        
        row_indices = sparse["data_components"]["row_indices"]
        self.assertEqual(row_indices["type"], "INTEGER")
        self.assertEqual(row_indices["kind"], "idx_kind")
        
        # Test typed_buffer with allocatable procedure argument
        buffer = types["typed_buffer"]
        resize_proc = buffer["procedures"]["resize"]
        self.assertEqual(resize_proc["implementation"], "resize_buffer")
        
        # Test config_type with nested parameterized components
        config = types["config_type"]
        
        # Check default value for len parameter
        name_len = config["type_parameters"]["name_len"]
        self.assertEqual(name_len["parameter_type"], "LEN")
        self.assertEqual(name_len["default"], "64")
                
        # Check nested parameterized type component
        options = config["data_components"]["options"]
        self.assertEqual(options["type"], "flexible_array")
        self.assertEqual(options["type_params"], "(kind(1.0), :)")
        self.assertCountEqual(options["attributes"], ["ALLOCATABLE", "PUBLIC"])
        # Should indicate it's an array of parameterized types
        self.assertEqual(options["dimension"]["dimensions"],
                        [ArrayBound(BoundType.DEFERRED)])

        metadata = config["data_components"]["metadata"]
        self.assertEqual(metadata["type"], "typed_buffer")
        self.assertEqual(metadata["type_params"], "(int32, 4, :)")
        self.assertCountEqual(metadata["attributes"], ["ALLOCATABLE", "PUBLIC"])

        # Test module procedures that return allocatable parameterized types
        procedures = module["functions"]

        create_array = procedures["create_flexible_array"]
        self.assertEqual(create_array["return"]["type"], "flexible_array")
        self.assertEqual(create_array["return"]["type_params"], "(:, :)")
        self.assertEqual(create_array["return"]["attributes"], ["ALLOCATABLE"])
        self.assertEqual(create_array["description"], "Create and allocate a flexible array\n\n")

        # Check procedure arguments
        self.assertEqual(len(create_array["arguments"]), 2)
        size_arg = create_array["in"]["size"]
        self.assertEqual(size_arg["type"], "INTEGER")
        self.assertEqual(size_arg["description"], "Size of the array to create")

        kind_arg = create_array["in"]["kind_val"]
        self.assertEqual(kind_arg["type"], "INTEGER")
        self.assertEqual(kind_arg["attributes"], ["OPTIONAL"])
        self.assertEqual(kind_arg["description"], "Optional kind parameter value")

        create_matrix = procedures["create_sparse_matrix"]
        self.assertEqual(create_matrix["return"]["type"], "sparse_matrix")
        self.assertEqual(create_matrix["return"]["type_params"], "(:, :, :, :)")
        self.assertEqual(create_matrix["return"]["attributes"], ["ALLOCATABLE"])

        # Add more detailed TypeParameter assertions for the types
        # For flexible_array
        self.assertEqual(len(flex_array["type_parameters"]), 2)
        k_param = flex_array["type_parameters"]["k"]
        self.assertEqual(k_param["name"], "k")
        self.assertEqual(k_param["type"], "INTEGER")
        self.assertEqual(k_param["parameter_type"], "KIND")
        self.assertEqual(k_param["default"], "kind(1.0d0)")
        self.assertEqual(k_param["description"], "Kind parameter for element precision (default: double)\n")

        n_param = flex_array["type_parameters"]["n"]
        self.assertEqual(n_param["name"], "n")
        self.assertEqual(n_param["type"], "INTEGER")
        self.assertEqual(n_param["parameter_type"], "LEN")
        self.assertIsNone(n_param["default"])
        self.assertEqual(n_param["description"], "Length parameter for array size\n")

        # For sparse_matrix - test all parameters including idx_kind
        self.assertEqual(len(sparse["type_parameters"]), 5)
        sparse_k = sparse["type_parameters"]["k"]
        self.assertEqual(sparse_k["name"], "k")
        self.assertEqual(sparse_k["type"], "INTEGER")
        self.assertEqual(sparse_k["parameter_type"], "KIND")
        self.assertEqual(sparse_k["default"], "kind(1.0)")
        self.assertEqual(sparse_k["description"], "Precision kind for matrix elements\n")

        idx_kind = sparse["type_parameters"]["idx_kind"]
        self.assertEqual(idx_kind["name"], "idx_kind")
        self.assertEqual(idx_kind["type"], "INTEGER")
        self.assertEqual(idx_kind["parameter_type"], "KIND")
        self.assertEqual(idx_kind["default"], "int32")
        self.assertEqual(idx_kind["description"], "Storage kind for indices (default: 32-bit)\n")

        max_entries = sparse["type_parameters"]["max_entries"]
        self.assertEqual(max_entries["name"], "max_entries")
        self.assertEqual(max_entries["type"], "INTEGER")
        self.assertEqual(max_entries["parameter_type"], "LEN")
        self.assertIsNone(max_entries["default"])
        self.assertEqual(max_entries["description"], "Maximum number of non-zero entries\n")

        # For typed_buffer
        buffer_params = buffer["type_parameters"]
        self.assertEqual(len(buffer_params), 3)

        elem_kind = buffer_params["elem_kind"]
        self.assertEqual(elem_kind["name"], "elem_kind")
        self.assertEqual(elem_kind["type"], "INTEGER")
        self.assertEqual(elem_kind["parameter_type"], "KIND")
        self.assertIsNone(elem_kind["default"])
        self.assertEqual(elem_kind["description"], "Kind parameter for buffer elements\n")

        elem_size = buffer_params["elem_size"]
        self.assertEqual(elem_size["name"], "elem_size")
        self.assertEqual(elem_size["type"], "INTEGER")
        self.assertEqual(elem_size["parameter_type"], "KIND")
        self.assertEqual(elem_size["default"], "4")
        self.assertEqual(elem_size["description"], "Size in bytes of each element\n")

        capacity = buffer_params["capacity"]
        self.assertEqual(capacity["name"], "capacity")
        self.assertEqual(capacity["type"], "INTEGER")
        self.assertEqual(capacity["parameter_type"], "LEN")
        self.assertIsNone(capacity["default"])
        self.assertEqual(capacity["description"], "Maximum capacity of the buffer\n")

        # For config_type
        config_params = config["type_parameters"]
        self.assertEqual(len(config_params), 2)

        name_len = config_params["name_len"]
        self.assertEqual(name_len["name"], "name_len")
        self.assertEqual(name_len["type"], "INTEGER")
        self.assertEqual(name_len["parameter_type"], "LEN")
        self.assertEqual(name_len["default"], "64")
        self.assertEqual(name_len["description"], "Maximum length for configuration name\n")

        max_options = config_params["max_options"]
        self.assertEqual(max_options["name"], "max_options")
        self.assertEqual(max_options["type"], "INTEGER")
        self.assertEqual(max_options["parameter_type"], "LEN")
        self.assertEqual(max_options["default"], "100")
        self.assertEqual(max_options["description"], "Maximum number of options\n")                                
if __name__ == "__main__":
    unittest.main()

