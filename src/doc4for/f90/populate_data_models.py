from typing import List, Type, Dict, Optional, Tuple
import re
from functools import wraps
from fparser.one.block_statements import (
    Module,
    Comment,
    Function,
    Subroutine,
    Program,
    Public,
    Type,
    Use,
    BlockData,
    Common,
    Data,
)
from fparser.one.typedecl_statements import TypeDeclarationStatement
from doc4for.data_models import (
    FunctionDescription,
    SubroutineDescription,
    ParameterDescription,
    TypeDescription,
    ModuleDescription,
    ProgramDescription,
    Uses,
    VariableDescription,
    BlockDataDescription,
    Dimension,
    ArrayBound,
    Expression,
    ExpressionType
)
from doc4for.comment_utils import is_doc4for_comment, format_comments
from doc4for.argument_utils import (
    update_arguments_with_comment_data,
    update_arguments_with_parsed_data,
)
from doc4for.type_utils import update_type_with_parsed_data, extract_dimension

def handle_dimension_errors(func):
    @wraps(func)
    def wrapper(*args, **kwargs):
        try:
            return func(*args, **kwargs)
        except Exception as e:
            # For now, just re-raise, but could log or handle differently in future
            raise
    return wrapper

def parse_program(
    program: Program, comment_stack: List[Comment], file_name: str
) -> ProgramDescription:
    program_details: ProgramDescription = {
        "program_name": program.name,
        "file_name": file_name,
        "program_description": "",
        "uses": {},
    }
    if is_doc4for_comment(comment_stack):
        program_details["program_description"] = format_comments(comment_stack)
    for program_child in program.content:
        if isinstance(program_child, Use):
            module_name = program_child.name
            if module_name not in program_details["uses"]:
                uses: Uses = {"module_name": program_child.name, "selections": []}
                program_details["uses"][module_name] = uses
            if (
                not program_child.items
                and program_details["uses"][module_name]["selections"]
            ):
                # everything in the module is used, so any selections are overwritten
                program_details["uses"][module_name]["selections"] = []
            else:
                program_details["uses"][module_name]["selections"].extend(
                    program_child.items
                )
    return program_details


def parse_block_data(
    block_data: BlockData, comment_stack: List[Comment]
) -> BlockDataDescription:
    block_data_details: BlockDataDescription = {
        "name": block_data.name,
        "description": "",
        "common_blocks": {},
    }
    if is_doc4for_comment(comment_stack):
        block_data_details["description"] = format_comments(comment_stack)

    # Track variables we've seen and their types
    variable_descriptions: Dict[str, VariableDescription] = {}
    for item in block_data.content:
        if isinstance(item, TypeDeclarationStatement):
            variables = parse_variable(item, [])  # TODO maintain a comment stack
            for variable in variables:
                variable_descriptions[variable["name"]] = variable
    for item in block_data.content:
        if isinstance(item, Common):
            common_name = item.items[0][0]
            variable_names = item.items[0][1]
            block_data_details["common_blocks"]["name"] = {}
            for variable_name in variable_names:
                block_data_details["common_blocks"]["name"][common_name] = {
                    "description": "",
                    "type": "",
                    "name": variable_name,
                    "dimension": None,
                    "attributes": [],
                    "kind": None,
                    "initial_value": None,
                }
            # # Create entries for these variables, using type info if we already have it
            # block_data_details["common_blocks"][common_name] = {
            #     name: variable_types.get(
            #         name,
            #         {
            #             "description": "",
            #             "type": "",
            #             "name": name,
            #             "dimension": None,
            #             "attributes": [],
            #             "kind": None,
            #             "initial_value": None,
            #         },
            #     )
            #     for name in variables
            # }

        elif isinstance(item, TypeDeclarationStatement):
            temp = parse_variable(item, [])
            x = 1
            # var_type = item.get_type()
            # for item in item.items:
            #     var_info = {
            #         "description": "",
            #         "type": var_type,
            #         "name": item.name,
            #         "dimension": (
            #             parse_dimension(item.dimension) if item.dimension else None
            #         ),
            #         "attributes": [],
            #         "kind": item.kind if hasattr(item, "kind") else None,
            #         "initial_value": None,
            #     }

            #     # Update our tracking dict
            #     variable_types[item.name] = var_info

            #     # Update any existing common block entries
            #     for common_block in block_data_details["common_blocks"].values():
            #         if item.name in common_block:
            #             common_block[item.name].update(var_info)
        elif isinstance(item, Data):
            pass
            # for var_name, value in zip(item.items, item.values):
            #     for common_block in block_data_details["common_blocks"].values():
            #         if var_name in common_block:
            #             common_block[var_name]["initial_value"] = str(value)

    return block_data_details


def parse_module(
    module: Module, comment_stack: List[Comment], file_name: str
) -> ModuleDescription:
    module_data: ModuleDescription = {
        "module_name": module.name,
        "parameters": {},
        "variables": {},
        "functions": {},
        "subroutines": {},
        "types": {},
        "file_name": file_name,
        "module_description": "",
    }
    if is_doc4for_comment(comment_stack):
        module_data["module_description"] = format_comments(comment_stack)
    return module_data


def parse_function(
    function: Function, comment_stack: List[Comment]
) -> FunctionDescription:
    attributes: List[str] = [
        attr.strip().lower() for attr in function.prefix.split() if attr.strip()
    ]
    function_description: FunctionDescription = {
        "attributes": attributes,
        "description": "",
        "arguments": function.args,
        "in": {},
        "out": {},
        "return": {},
        "binding_type": "",
        "interface": "",
    }
    update_arguments_with_parsed_data(function, function_description)
    if comment_stack:
        update_arguments_with_comment_data(comment_stack, function_description)
    return function_description


def parse_subroutine(
    subroutine: Subroutine, comment_stack: List[Comment]
) -> SubroutineDescription:
    attributes: List[str] = [
        attr.strip().lower() for attr in subroutine.prefix.split() if attr.strip()
    ]
    subroutine_description: SubroutineDescription = {
        "attributes": attributes,
        "description": "",
        "arguments": subroutine.args,
        "in": {},
        "out": {},
        "binding_type": "",
        "interface": "",
    }
    update_arguments_with_parsed_data(subroutine, subroutine_description)
    if comment_stack:
        update_arguments_with_comment_data(comment_stack, subroutine_description)
    return subroutine_description


def parse_type(
    type: Type, comment_stack: List[Comment], public_declarations: List[str]
) -> TypeDescription:
    type_name: str = type.name
    type_description: TypeDescription = {
        "type_name": type_name,
        "attributes": [],
        "description": "",
        "data_components": {},
        "procedures": {},
        "generic_interfaces": {},
        "extends": None,
    }
    if any(spec.startswith("extends") for spec in type.specs):
        extends_spec = next(spec for spec in type.specs if spec.startswith("extends"))
        match = re.search(r"extends\s*\(\s*(\w+)\s*\)", extends_spec)
        if match:
            base_type = match.group(1)
            type_description["extends"] = base_type
    else:
        type_description["attributes"].extend(type.specs)
    if type_name in public_declarations:
        type_description["attributes"].append("public")
    update_type_with_parsed_data(type, type_description)
    if comment_stack:
        type_description["description"] = format_comments(comment_stack)
    return type_description

def parse_variable(
    declaration: TypeDeclarationStatement, comment_stack: List[Comment]
) -> List[VariableDescription]:
    description = (
        format_comments(comment_stack) if is_doc4for_comment(comment_stack) else ""
    )
    base_type = str(declaration.name).lower()
    shared_attributes = get_attributes(declaration)
    kind = extract_kind(declaration)

    # Extract dimension from attributes
    dimension_from_attr = extract_dimension_from_attributes(shared_attributes)
    
    # Remove 'dimension' from shared_attributes if present
    shared_attributes = [attr for attr in shared_attributes if not attr.startswith("dimension(")]

    variable_descriptions: List[VariableDescription] = []

    # fparser gives us each entity declaration separately
    for entity in declaration.entity_decls:
        if "=" in entity:
            full_name, initial_value = [x.strip() for x in entity.split("=", 1)]
        else:
            full_name = entity.strip()
            initial_value = None

        # Extract dimension using the new method
        dimension = extract_variable_dimension(full_name)

        # Use dimension from attributes if not found in name
        if not dimension and dimension_from_attr:
            dimension = dimension_from_attr

        # Remove any parenthetical expressions from the name
        name = re.sub(r"\(.*\)", "", full_name).strip()

        # Handle allocatable arrays
        if "allocatable" in shared_attributes and "(:)" in name:
            name = name.replace("(:)", "")

        variable_description: VariableDescription = {
            "description": description,
            "type": base_type,
            "name": name,
            "dimension": dimension,
            "attributes": shared_attributes,
            "kind": kind,
            "initial_value": initial_value,
        }

        variable_descriptions.append(variable_description)

    return variable_descriptions

# TODO public declaration should be handled here?
def parse_parameter(
    declaration: TypeDeclarationStatement, comment_stack: List[Comment]
) -> ParameterDescription:
    item_str = declaration.item.line
    name_match = re.search(r"::\s*(\w+)\s*=", item_str)
    if name_match:
        name = name_match.group(1)
        value = item_str.split("=", 1)[1].strip()
    else:
        # Handle cases where the parameter declaration format is not recognized
        name = ""
        value = ""
    parameter_description: ParameterDescription = {
        "description": "",
        "type": str(declaration.name),
        "name": name.strip(),
        "value": value.strip(),
        "dimension": "",
        "attributes": [],
    }
    if is_doc4for_comment(comment_stack):
        parameter_description["description"] = format_comments(comment_stack)
    return parameter_description


def is_parameter(declaration: TypeDeclarationStatement) -> bool:
    """Determines if a TypeDeclarationStatement is a parameter declaration.

    Args:
        declaration: The TypeDeclarationStatement to check

    Returns:
        bool: True if the declaration has the parameter attribute
    """
    # Check if 'parameter' is in the attributes
    return (
        any(attr.lower() == "parameter" for attr in declaration.attrspec)
        if declaration.attrspec
        else False
    )


def get_attributes(declaration: TypeDeclarationStatement) -> List[str]:
    """Extracts attributes from a TypeDeclarationStatement.

    Args:
        declaration: The TypeDeclarationStatement to process

    Returns:
        List[str]: List of attributes (public, private, etc.)
    """
    return declaration.attrspec if declaration.attrspec else []


def extract_kind(declaration: TypeDeclarationStatement) -> Optional[str]:
    """Extract kind specification from a type declaration.

    Args:
        declaration: The type declaration statement

    Returns:
        Optional[str]: The kind specification if present, None otherwise
    """
    if hasattr(declaration, "selector") and isinstance(declaration.selector, Tuple):
        star_value, kind_value = declaration.selector
        return star_value or kind_value or None
    return None

def extract_dimension_from_attributes(attributes: List[str]) -> Optional[Dimension]:
    """Extract dimension information from a list of attributes."""
    for attr in attributes:
        if attr.startswith("dimension(") and attr.endswith(")"):
            dim_str = attr[10:-1]  # 10 is len("dimension(")
            return extract_variable_dimension(f"x({dim_str})")
    return None

@handle_dimension_errors
def extract_variable_dimension(name: str) -> Optional[Dimension]:
    """Extract dimension information from a variable name with array specification.

    Args:
        name (str): Variable name potentially including dimension specification

    Returns:
        Optional[Dimension]: The dimension information if present, None otherwise

    Examples:
        Simple dimensions:
            "x(10)"     -> {'dimensions': [{'lower': '1', 'upper': '10'}]}
            "x(10,20)"  -> {'dimensions': [{'lower': '1', 'upper': '10'}, 
                                        {'lower': '1', 'upper': '20'}]}
        
        Explicit bounds:
            "x(0:10)"   -> {'dimensions': [{'lower': '0', 'upper': '10'}]}
            "x(-5:5)"   -> {'dimensions': [{'lower': '-5', 'upper': '5'}]}
        
        Variable bounds:
            "x(n)"      -> {'dimensions': [{'lower': '1', 'upper': 'n'}]}
            "x(-n:n)"   -> {'dimensions': [{'lower': '-n', 'upper': 'n'}]}
        
        Expressions:
            "x(2*n+1)"  -> {'dimensions': [{'lower': '1', 'upper': '2*n+1'}]}
            "x(0:n-1)"  -> {'dimensions': [{'lower': '0', 'upper': 'n-1'}]}

        Allocatable arrays:
            "x(:)"      -> {'dimensions': [{'lower': None, 'upper': None}]}
            "x(:,:)"    -> {'dimensions': [{'lower': None, 'upper': None},
                                        {'lower': None, 'upper': None}]}
        
        Not an array:
            "x"         -> None
    """    
    dim_str = extract_dimension_string(name)
    if dim_str is None:
        return None
    
    dims = [parse_dimension_spec(d) for d in split_dimensions(dim_str)]
    return {"dimensions": dims} if dims else None

def extract_dimension_string(name: str) -> Optional[str]:
    """Extract the dimension specification string from a variable name."""
    if "(" not in name or ")" not in name:
        return None
    
    start = name.index('(')
    end = find_matching_parenthesis(name, start)
    if end == -1:
        return None
    
    return name[start + 1: end]

def find_matching_parenthesis(s: str, start: int = 0) -> int:
    """Find the index of the matching closing parenthesis.
    
    Args:
        s: The string to search in
        start: The index of the opening parenthesis
    
    Returns:
        The index of the matching closing parenthesis, or -1 if not found
    """
    count = 0
    for i in range(start, len(s)):
        if s[i] == '(':
            count += 1
        elif s[i] == ')':
            count -= 1
            if count == 0:
                return i
    return -1


def split_dimensions(s: str) -> List[str]:
    """Split dimension specifications, respecting nested parentheses."""
    result = []
    current = ""
    paren_count = 0
    
    for char in s:
        if char == ',' and paren_count == 0:
            result.append(current.strip())
            current = ""
        else:
            current += char
            if char == '(':
                paren_count += 1
            elif char == ')':
                paren_count -= 1
    
    result.append(current.strip())
    
    # Replace any empty strings with ':'
    return [':' if not dim else dim for dim in result]

def split_args(s: str) -> List[str]:
    """Split function arguments, respecting nested parentheses.
    
    Args:
        s: The argument string
    
    Returns:
        A list of individual arguments
    
    Examples:
        split_args("1, 2")            -> ["1", "2"]
        split_args("f(1,2), 3")       -> ["f(1,2)", "3"]
        split_args("1, g(2,h(3,4))") -> ["1", "g(2,h(3,4))"]
    """
    result = []
    current = ""
    paren_count = 0
    
    for char in s:
        if char == ',' and paren_count == 0:
            result.append(current.strip())
            current = ""
        else:
            current += char
            if char == '(':
                paren_count += 1
            elif char == ')':
                paren_count -= 1
    
    if current:
        result.append(current.strip())
    return result

def parse_expression(expr: str) -> Expression:
    if '(' in expr:
        # This is a function call
        fname = expr[:expr.index('(')]
        args_str = expr[expr.index('(')+1:expr.rindex(')')]
        args = [parse_expression(arg.strip()) for arg in split_args(args_str)]
        return Expression(
            expr_type=ExpressionType.FUNCTION_CALL,
            value=expr,
            function_name=fname,
            arguments=args
        )
    elif expr.isdigit() or expr[0] in '+-' and expr[1:].isdigit():
        # This is a literal
        return Expression(expr_type=ExpressionType.LITERAL, value=expr)
    else:
        # This is a variable
        return Expression(expr_type=ExpressionType.VARIABLE, value=expr)

def parse_dimension_spec(d: str) -> ArrayBound:
    """Parse a single dimension specification into an ArrayBound."""
    d = d.strip()

    # assumed size, first index is 1 with no upper bound
    if d == '*':
        return {"lower": Expression(ExpressionType.LITERAL, "1"), "upper": None, "stride": None}
 
    # allocatable, no explicit dimensions
    if d == ':':
        return {"lower": None, "upper": None, "stride": None}
    
    if ':' in d:
        parts = [p.strip() for p in d.split(':')]
        if len(parts) == 2:
            lower, upper = parts
            return {
                "lower": parse_expression(lower) if lower else None,
                "upper": parse_expression(upper) if upper else None,
                "stride": None
            }
        elif len(parts) == 3:
            lower, upper, stride = parts
            return {
                "lower": parse_expression(lower) if lower else None,
                "upper": parse_expression(upper) if upper else None,
                "stride": parse_expression(stride) if stride else None
            }
    
    return {
        "lower": Expression(ExpressionType.LITERAL, "1"),
        "upper": parse_expression(d),
        "stride": None
    }

#TODO: this is to link any function calls in dimensions to their definition -
# will need to have a function list for this to work
# def process_dimensions_for_documentation(dims: List[ArrayBound], 
#                                         function_registry: Dict[str, 'FunctionInfo']):
#     function_references = []
#     for dim in dims:
#         for bound in (dim['lower'], dim['upper'], dim['stride']):
#             if bound and bound.expr_type == ExpressionType.FUNCTION_CALL:
#                 if bound.function_name in function_registry:
#                     function_references.append({
#                         'name': bound.function_name,
#                         'info': function_registry[bound.function_name]
#                     })
#     return function_references

# {% for dim in dimensions %}
#   {% if dim.upper.expr_type == 'function_call' %}
#     <a href="#function-{{ dim.upper.function_name }}">{{ dim.upper.value }}</a>
#   {% else %}
#     {{ dim.upper.value }}
#   {% endif %}
# {% endfor %}