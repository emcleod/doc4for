import unittest
from unittest import TestCase
from unittest.mock import Mock
from typing import List, Dict, Any, Optional, Union, Tuple

from doc4for.f90.populate_data_models import parse_variable
from doc4for.models.common import Expression, ExpressionType
from doc4for.models.variable_models import VariableDescription

class TestArrays(TestCase):
    maxDiff=None
    def setUp(self):
        self.base_expected = {
            "description": "",
            "attributes": [],
            "kind": None,
            "initial_value": None,
        }

    def create_literal(self, value: str) -> Expression:
        return Expression(ExpressionType.LITERAL, value)

    def create_variable(self, name: str) -> Expression:
        return Expression(ExpressionType.VARIABLE, name)

    def create_function_call(self, name: str, args: List[Union[str, Expression]]) -> Expression:
        processed_args = [
            arg if isinstance(arg, Expression) else self.create_literal(arg)
            for arg in args
        ]
        return Expression(
            ExpressionType.FUNCTION_CALL,
            f"{name}({','.join(arg.value for arg in processed_args)})",
            name,
            processed_args
        )

    def create_dimension(self, 
                        lower: Optional[Union[str, Expression]] = None,
                        upper: Optional[Union[str, Expression]] = None,
                        stride: Optional[Union[str, Expression]] = None) -> Dict[str, Optional[Expression]]:
        def process_value(v: Optional[Union[str, Expression]]) -> Optional[Expression]:
            if v is None:
                return None
            if isinstance(v, Expression):
                return v
            if v.isdigit() or (v[0] in '+-' and v[1:].isdigit()):
                return self.create_literal(v)
            return self.create_variable(v)

        # Always include all keys, even when None
        return {
            "lower": process_value(lower),
            "upper": process_value(upper),
            "stride": process_value(stride)
        }

    def create_declaration(self, 
                        name: str, 
                        type_name: str = "real",
                        dims: Optional[List[Dict[str, Optional[Expression]]]] = None,
                        attributes: List[str] = None,
                        initial_value: Optional[str] = None,
                        length: Optional[str] = None) -> VariableDescription:
        result = {**self.base_expected, "type": type_name, "name": name}
        if dims:
            result["dimension"] = {"dimensions": dims}
        if attributes:
            result["attributes"] = attributes
        if initial_value is not None:
            result["initial_value"] = initial_value
        result["length"] = length
        return result

    def create_mock_declaration(self,
                               line: str,
                               decls: List[str],
                               type_name: str = "real",
                               attrspec: List[str] = None,
                               selector: Tuple[str, str] = None) -> Mock:
        declaration = Mock()
        declaration.name = type_name
        declaration.item.line = line
        declaration.attrspec = attrspec or []
        declaration.entity_decls = decls
        declaration.selector = selector
        return declaration

    def test_simple_array_declarations(self):
        test_cases = [
            ("real x(10)", ['x(10)']),
            ("real :: x(10)", ['x(10)']),
            ("real x(10, 20)", ['x(10, 20)']),
            ("real :: x(10,20)", ['x(10,20)']),
        ]
        
        for line, decls in test_cases:
            declaration = self.create_mock_declaration(line, decls)
            result = parse_variable(declaration, [])
            
            dims = 2 if ',' in decls[0] else 1
            expected_dims = [self.create_dimension(lower="1", upper="10")]            
            if dims == 2:
                expected_dims.append(self.create_dimension(lower="1", upper="20"))
            expected = [self.create_declaration(decls[0].split('(')[0], dims=expected_dims)]            
            self.assertEqual(result, expected)  

    def test_explicit_bounds(self):
        test_cases = [
            ("real x(0:9)", ['x(0:9)']),
            ("real :: x(-5 : 5)", ['x(-5:5)']),
            ("real :: x(2:n)", ['x(2:n)']),
        ]
        
        expected_bounds = [
            [self.create_dimension(lower="0", upper="9")],
            [self.create_dimension(lower="-5", upper="5")],
            [self.create_dimension(lower="2", upper="n")],
        ]
        
        for i, (line, decls) in enumerate(test_cases):
            declaration = self.create_mock_declaration(line, decls)
            result = parse_variable(declaration, [])
            expected = [self.create_declaration(decls[0].split('(')[0], dims=expected_bounds[i])]
            self.assertEqual(result, expected)

    def test_variable_dimensions(self):
        test_cases = [
            ("real :: x(n)", ['x(n)']),
            ("real :: x(f(1,2), 10)", ['x(f(1,2), 10)']),
            ("real :: x(2*5, n+1)", ['x(2*5, n+1)']),
            ("real :: x(n:m, 1:10)", ['x(n:m, 1:10)']),
        ]
        
        expected_bounds = [
            [self.create_dimension(lower="1", upper="n")],
            [
                self.create_dimension(lower="1", upper=self.create_function_call("f", ["1", "2"])),
                self.create_dimension(lower="1", upper="10")
            ],
            [
                self.create_dimension(lower="1", upper="2*5"),
                self.create_dimension(lower="1", upper="n+1")
            ],
            [
                self.create_dimension(lower="n", upper="m"),
                self.create_dimension(lower="1", upper="10")
            ],
        ]
        
        for i, (line, decls) in enumerate(test_cases):
            declaration = self.create_mock_declaration(line, decls)
            result = parse_variable(declaration, [])
            expected = [self.create_declaration(decls[0].split('(')[0], dims=expected_bounds[i])]
            self.assertEqual(result, expected)

    def test_dimension_attribute_style(self):
        test_cases = [
            ("real dimension(10):: x", ["dimension(10)"], ['x']),
            ("real dimension(10, 20):: x", ["dimension(10, 20)"], ['x']),
            ("real dimension(-5:5):: x", ["dimension(-5:5)"], ['x']),
            ("real dimension(-5:5,10,n:20+n):: x", ["dimension(-5:5,10,n:20+n)"], ['x']),
        ]
        
        expected_bounds = [
            [self.create_dimension(lower="1", upper="10")],
            [
                self.create_dimension(lower="1", upper="10"),
                self.create_dimension(lower="1", upper="20")
            ],
            [self.create_dimension(lower="-5", upper="5")],
            [
                self.create_dimension(lower="-5", upper="5"),
                self.create_dimension(lower="1", upper="10"),
                self.create_dimension(lower="n", upper="20+n")
            ],
        ]
        
        for i, (line, attr, decls) in enumerate(test_cases):
            declaration = self.create_mock_declaration(line, decls, attrspec=attr)
            result = parse_variable(declaration, [])
            expected = [self.create_declaration(decls[0], dims=expected_bounds[i])]
            self.assertEqual(result, expected)

    def test_array_initialization(self):
        test_cases = [
            ("real x(2,2) = reshape((/1,2,3,4/), (/2,2/))", 
             ['x(2,2) = reshape((/1,2,3,4/), (/2,2/))'], 
             "reshape((/1,2,3,4/), (/2,2/))"),
            ("real y(2,2) = ((1,2),(3,4))", 
             ['y(2,2) = ((1,2),(3,4))'], 
             "((1,2),(3,4))"),
            ("integer, parameter, dimension(3) :: arr = [1, 2, 3]",
             ['arr = [1, 2, 3]'],
             "[1, 2, 3]"),
        ]
        
        for line, decls, init_value in test_cases:
            type_name = "real" if "real" in line else "integer"
            attrspec = ["parameter", "dimension(3)"] if "parameter" in line else []
            declaration = self.create_mock_declaration(line, decls, type_name=type_name, attrspec=attrspec)
            result = parse_variable(declaration, [])
            self.assertEqual(result[0]["initial_value"], init_value)

    def test_multiple_array_declarations(self):
        declaration = self.create_mock_declaration(
            "real x(10, 20), y, z(5)",
            ['x(10, 20)', 'y', 'z(5)']
        )

        result = parse_variable(declaration, [])
        expected = [
            self.create_declaration("x", dims=[
                self.create_dimension(lower="1", upper="10"),
                self.create_dimension(lower="1", upper="20")
            ]),
            self.create_declaration("y"),
            self.create_declaration("z", dims=[
                self.create_dimension(lower="1", upper="5")
            ]),
        ]

        self.assertEqual(len(result), len(expected))
        
        for r, e in zip(result, expected):
            # Check name and type
            self.assertEqual(r["name"], e["name"])
            self.assertEqual(r["type"], e["type"])
            
            # Check dimensions if they exist
            if "dimension" in e:
                self.assertIn("dimension", r)
                self.assertEqual(
                    r["dimension"]["dimensions"],
                    e["dimension"]["dimensions"]
                )

    def test_multiple_arrays_same_attributes(self):
        declaration = self.create_mock_declaration(
            "real, dimension(10,10) :: x, y, z",
            ['x', 'y', 'z'],
            attrspec=["dimension(10,10)"]
        )

        result = parse_variable(declaration, [])
        expected_dims = [
            self.create_dimension(lower="1", upper="10"),
            self.create_dimension(lower="1", upper="10")
        ]
        for var in result:
            self.assertEqual(var["dimension"]["dimensions"], expected_dims)

    def test_allocatable_arrays(self):
        test_cases = [
            ("real, allocatable :: x(:)", ["x"], 1),
            ("real, allocatable :: matrix(:,:)", ["matrix"], 2),
            ("real, allocatable :: cube(:,:,:)", ["cube"], 3),
            ("real, allocatable :: x(:), y(:), z(:)", ["x", "y", "z"], 1),
        ]
        
        for line, names, dim_count in test_cases:
            declaration = self.create_mock_declaration(
                line, 
                [f"{name}(:{',' * (dim_count-1)})" for name in names],
                attrspec=["allocatable"]
            )
            result = parse_variable(declaration, [])
            
            expected = [
                self.create_declaration(
                    name,
                    dims=[self.create_dimension()] * dim_count,
                    attributes=["allocatable"]
                )
                for name in names
            ]
            
            self.assertEqual(len(result), len(expected))
            for r, e in zip(result, expected):
                self.assertEqual(r["name"], e["name"])
                self.assertEqual(r["type"], e["type"])
                self.assertEqual(r["attributes"], e["attributes"])
                self.assertEqual(
                    r["dimension"]["dimensions"],
                    e["dimension"]["dimensions"]
                )

    def test_assumed_size_arrays(self):
        declaration = self.create_mock_declaration("real :: x(*)", ['x(*)'])
        result = parse_variable(declaration, [])
        expected = [self.create_declaration("x", dims=[
            self.create_dimension(lower="1", upper=None)
        ])]
        self.assertEqual(result, expected)

    def test_arrays_with_strides(self):
        declaration = self.create_mock_declaration(
            "integer :: arr(1:10:2)",
            ['arr(1:10:2)'],
            type_name="integer"
        )
        result = parse_variable(declaration, [])
        expected = [self.create_declaration("arr", type_name="integer", dims=[
            self.create_dimension(lower="1", upper="10", stride="2")
        ])]
        self.assertEqual(result, expected)

    def test_complex_strides(self):
        test_cases = [
            ("integer :: arr(1:10:n)", "arr", "integer", [
                self.create_dimension(lower="1", upper="10", stride="n")
            ]),
            ("real :: x(0:100:2*k)", "x", "real", [
                self.create_dimension(lower="0", upper="100", stride="2*k")
            ]),
            ("real :: z(1:10:get_stride())", "z", "real", [
                self.create_dimension(
                    lower="1",
                    upper="10",
                    stride=self.create_function_call("get_stride", [])
                )
            ]),
            ("integer :: rev(10:1:-1)", "rev", "integer", [
                self.create_dimension(lower="10", upper="1", stride="-1")
            ]),
            ("real :: matrix(1:10:2, 1:20:5)", "matrix", "real", [
                self.create_dimension(lower="1", upper="10", stride="2"),
                self.create_dimension(lower="1", upper="20", stride="5")
            ]),
        ]
        
        for line, name, type_name, expected_dims in test_cases:
            declaration = self.create_mock_declaration(
                line,
                [line.split("::")[1].strip()],
                type_name=type_name
            )
            result = parse_variable(declaration, [])
            expected = [self.create_declaration(name, type_name=type_name, dims=expected_dims)]
            self.assertEqual(result, expected)

    def test_function_calls_in_dimensions(self):
        declaration = self.create_mock_declaration(
            "real :: x(f(1,2), g(3,4))",
            ['x(f(1,2), g(3,4))']
        )
        result = parse_variable(declaration, [])
        expected = [self.create_declaration("x", dims=[
            self.create_dimension(lower="1", upper=self.create_function_call("f", ["1", "2"])),
            self.create_dimension(lower="1", upper=self.create_function_call("g", ["3", "4"]))
        ])]
        self.assertEqual(result, expected)

    def test_complex_expressions_in_bounds(self):
        declaration = self.create_mock_declaration(
            "real :: x(2*f(1,2):g(3,4))",
            ['x(2*f(1,2):g(3,4))']
        )
        result = parse_variable(declaration, [])
        expected = [self.create_declaration("x", dims=[
            self.create_dimension(
                lower = self.create_function_call("2*f", ["1", "2"]),
                upper = self.create_function_call("g", ["3", "4"])
            )
        ])]
        self.assertEqual(result, expected)

    def test_character_arrays(self):
        test_cases = [
            (
                "character(len=10) :: names(100)",
                ['names(100)'],
                ["len=10"]
            ),
            (
                "character(10) names(5,10)",  # Alternative syntax
                ['names(5,10)'],
                ["10"]  # length specification comes differently in fparser
            ),
            (
                "character(len=20), dimension(50) :: strings",
                ['strings'],
                ["len=20", "dimension(50)"]
            ),
        ]
        
        expected_dims = [
            [self.create_dimension(lower="1", upper="100")],
            [
                self.create_dimension(lower="1", upper="5"),
                self.create_dimension(lower="1", upper="10")
            ],
            [self.create_dimension(lower="1", upper="50")]
        ]
        
        for i, (line, decls, attrs) in enumerate(test_cases):
            declaration = self.create_mock_declaration(
                line, 
                decls,
                type_name="character",
                attrspec=attrs
            )
            result = parse_variable(declaration, [])
            expected = [self.create_declaration(
                decls[0].split('(')[0],
                type_name="character",
                dims=expected_dims[i],
                attributes=[],
                length="20" if i == 2 else "10"  # or "20" for the third case
            )]            
            self.assertEqual(result, expected)

    def test_assumed_length_character_array(self):
        declaration = self.create_mock_declaration(
            "character(len=*) :: x(5)",
            ['x(5)'],
            type_name="character",
            attrspec=["len=*"]
        )
        result = parse_variable(declaration, [])
        expected = [self.create_declaration(
            "x",
            type_name="character",
            dims=[self.create_dimension(lower="1", upper="5")],
            length="*"
        )]
        self.assertEqual(result, expected)         

    def test_deferred_length_allocatable_character_array(self):
        declaration = self.create_mock_declaration(
            "character(len=:), allocatable :: x(:)",
            ['x(:)'],
            type_name="character",
            attrspec=["len=:", "allocatable"]
        )
        result = parse_variable(declaration, [])
        expected = [self.create_declaration(
            "x",
            type_name="character",
            dims=[self.create_dimension()],
            attributes=["allocatable"],
            length=":"
        )]
        self.assertEqual(result, expected)       

    def test_character_array_initialization(self):
        declaration = self.create_mock_declaration(
            "character(10) :: x(2) = (/'Hello', 'World'/)",
            ['x(2) = (/"Hello", "World"/)'],  # Updated to use double quotes as fparser might
            type_name="character",
            attrspec=["10"]
        )
        result = parse_variable(declaration, [])
        expected = [self.create_declaration(
            "x",
            type_name="character",
            dims=[self.create_dimension(lower="1", upper="2")],
            initial_value='"Hello", "World"',  # Initial value as string
            length="10"
        )]
        self.assertEqual(result, expected)

    def test_pointer_array(self):
        declaration = self.create_mock_declaration(
            "real, pointer :: x(:,:)",
            ['x(:,:)'],
            attrspec=["pointer"]
        )
        result = parse_variable(declaration, [])
        expected = [self.create_declaration(
            "x",
            dims=[self.create_dimension(), self.create_dimension()],
            attributes=["pointer"]
        )]
        self.assertEqual(result, expected)

    def test_target_array(self):
        declaration = self.create_mock_declaration(
            "real, target :: x(10,20)",
            ['x(10,20)'],
            attrspec=["target"]
        )
        result = parse_variable(declaration, [])
        expected = [self.create_declaration(
            "x",
            dims=[
                self.create_dimension(lower="1", upper="10"),
                self.create_dimension(lower="1", upper="20")
            ],
            attributes=["target"]
        )]
        self.assertEqual(result, expected)        

    def test_array_with_pointer_and_dimension_attributes(self):
        declaration = self.create_mock_declaration(
            "real, pointer, dimension(10,10) :: x",
            ['x'],
            attrspec=["pointer", "dimension(10,10)"]
        )
        result = parse_variable(declaration, [])
        expected = [self.create_declaration(
            "x",
            dims=[
                self.create_dimension(lower="1", upper="10"),
                self.create_dimension(lower="1", upper="10")
            ],
            attributes=["pointer"]
        )]
        self.assertEqual(result, expected)        

    def test_f77_dimension_statement_style(self):
        declaration = self.create_mock_declaration(
            "DIMENSION X(10), Y(20)",
            ['X(10)', 'Y(20)'],
            type_name=None  
        )
        result = parse_variable(declaration, [])
        expected = [
            self.create_declaration(
                "X",
                type_name='none',  
                dims=[self.create_dimension(lower="1", upper="10")]
            ),
            self.create_declaration(
                "Y",
                type_name='none',  
                dims=[self.create_dimension(lower="1", upper="20")]
            )
        ]
        self.assertEqual(result, expected)

    def test_f77_character_length_array(self):
        declaration = self.create_mock_declaration(
            "character*10 names(100)",
            ['names(100)'],
            type_name="character",
            attrspec=[],
            selector=('10', '')
        )
        result = parse_variable(declaration, [])
        expected = [self.create_declaration(
            "names",
            type_name="character",
            dims=[self.create_dimension(lower="1", upper="100")],
            length="10"
        )]
        self.assertEqual(result, expected)    

    def test_array_initialization_implied_do_loop(self):
        declaration = self.create_mock_declaration(
            "real :: x(10) = (i, i=1,10)",
            ['x(10) = (i, i=1,10)'],
            type_name="real"
        )
        result = parse_variable(declaration, [])
        expected = [self.create_declaration(
            "x",
            type_name="real",
            dims=[self.create_dimension(lower="1", upper="10")],
            initial_value="(i, i=1,10)"
        )]
        self.assertEqual(result, expected)
            
    def test_array_of_derived_type(self):
        declaration = self.create_mock_declaration(
            "type(point) :: points(100)",
            ['points(100)'],
            type_name="type(point)"  # Note the type is now a derived type
        )
        result = parse_variable(declaration, [])
        expected = [self.create_declaration(
            "points",
            type_name="type(point)",
            dims=[self.create_dimension(lower="1", upper="100")]
        )]
        self.assertEqual(result, expected)   

    def test_implied_shape_array(self):
        declaration = self.create_mock_declaration(
            "integer :: x(*) = [1, 2, 3, 4, 5]",
            ['x(*) = [1, 2, 3, 4, 5]'],
            type_name="integer",
        )
        result = parse_variable(declaration, [])
        expected = [self.create_declaration(
            "x",
            type_name="integer",
            dims=[self.create_dimension(lower="1", upper=None)],  # Implied shape using [None, None]
            initial_value="1, 2, 3, 4, 5"
        )]
        self.assertEqual(result, expected)

    def test_allocatable_polymorphic_array(self):
        declaration = self.create_mock_declaration(
            "class(shape), allocatable :: shapes(:)",
            ['shapes(:)'],
            type_name="class(shape)",
            attrspec=["allocatable"]
        )
        result = parse_variable(declaration, [])
        expected = [self.create_declaration(
            "shapes",
            type_name="class(shape)",
            dims=[self.create_dimension()],
            attributes=["allocatable"]
        )]
        self.assertEqual(result, expected)

    def test_polymorphic_arrays(self):
        test_cases = [
            ("class(shape), allocatable :: x(:)", ['x(:)'], ["allocatable"]),
            ("class(shape), pointer :: y(:)", ['y(:)'], ["pointer"])
        ]
        
        for line, decls, attrs in test_cases:
            declaration = self.create_mock_declaration(
                line,
                decls,
                type_name="class(shape)",
                attrspec=attrs
            )
            result = parse_variable(declaration, [])
            expected = [self.create_declaration(
                decls[0].split('(')[0],
                type_name="class(shape)",
                dims=[self.create_dimension()],
                attributes=attrs
            )]
            self.assertEqual(result, expected)  

    def test_contiguous_array(self):
        declaration = self.create_mock_declaration(
            "real, contiguous :: x(:)",
            ['x(:)'],
            attrspec=["contiguous"]
        )
        result = parse_variable(declaration, [])
        expected = [self.create_declaration(
            "x",
            dims=[self.create_dimension()],
            attributes=["contiguous"]
        )]
        self.assertEqual(result, expected)     


    def test_arrays_in_derived_types(self):
        # Simulate a variable declaration inside a derived type
        declaration = self.create_mock_declaration(
            "real :: coordinates(3)",
            ['coordinates(3)'],
            type_name="real"
        )
        result = parse_variable(declaration, [])
        expected = [self.create_declaration(
            "coordinates",
            dims=[self.create_dimension(lower="1", upper="3")]
        )]
        self.assertEqual(result, expected)

    def test_coarray_declarations_1(self):
        test_cases = [
            ("integer :: x[*]", ['x[*]'], ["codimension[*]"]),
            ("real :: a(10)[3,*]", ['a(10)[3,*]'], ["codimension[3,*]"]),
            ("integer, allocatable :: d[:,:,:]", ['d[:,:,:]'], ["allocatable", "codimension[:,:,:]"])
        ]
        
        for line, decls, attrs in test_cases:
            declaration = self.create_mock_declaration(
                line,
                decls,
                attrspec=attrs
            )
            result = parse_variable(declaration, [])
            expected = [self.create_declaration(
                decls[0].split('[')[0].split('(')[0].strip(),
                dims=[self.create_dimension(lower="1", upper="10")] if '(' in decls[0] else None,
                attributes=attrs
            )]
            self.assertEqual(result, expected)

    def test_coarray_declarations_2(self):
        test_cases = [
            ("integer :: a[*]", ['a[*]'], "integer", [], [self.create_dimension()]),
            ("real :: b(10)[*]", ['b(10)[*]'], "real", [], 
            [self.create_dimension(lower="1", upper="10"), self.create_dimension()]),
            ("real :: c(10)[10,*]", ['c(10)[10,*]'], "real", [], 
            [self.create_dimension(lower="1", upper="10"), 
            self.create_dimension(lower="1", upper="10"), self.create_dimension()])
        ]
        
        for line, decls, type_name, attrs, dims in test_cases:
            declaration = self.create_mock_declaration(
                line,
                decls,
                type_name=type_name,
                attrspec=attrs
            )
            result = parse_variable(declaration, [])
            expected = [self.create_declaration(
                decls[0].split('[')[0].split('(')[0],
                type_name=type_name,
                dims=dims
            )]
            self.assertEqual(result, expected)   

if __name__ == "__main__":
    unittest.main()


