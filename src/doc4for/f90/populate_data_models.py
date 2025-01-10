from typing import List, Type, Dict
import re
from fparser.one.block_statements import (
    Module,
    Comment,
    Program,
    Public,
    Type,
    Use,
    BlockData,
    Common,
    Data,
)
from fparser.one.typedecl_statements import TypeDeclarationStatement
from doc4for.models.module_models import ModuleDescription, ProgramDescription, Uses, BlockDataDescription
from doc4for.models.variable_models import VariableDescription
from doc4for.models.type_models import TypeDescription
from doc4for.models.variable_models import VariableDescription
from doc4for.utils.comment_utils import is_doc4for_comment, format_comments
from doc4for.parse.type_parser import update_type_with_parsed_data
from doc4for.parse.variable_parser import parse_variables
from doc4for.parse.common_parser import get_attributes

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
                uses: Uses = {
                    "module_name": program_child.name, "selections": []}
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
            # TODO maintain a comment stack
            variables = parse_variable(item, [])
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
        extends_spec = next(
            spec for spec in type.specs if spec.startswith("extends"))
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
    declaration: TypeDeclarationStatement, 
    comment_stack: List[Comment]
) -> List[VariableDescription]:
    """Parse variable declarations into variable descriptions.
    
    Args:
        declaration: The type declaration statement to parse
        comment_stack: Stack of comments preceding the declaration
        
    Returns:
        List of variable descriptions for both array and scalar variables
    """
    description = format_comments(comment_stack) if is_doc4for_comment(comment_stack) else ""
    # base_type = str(declaration.name).lower()
    shared_attributes = get_attributes(declaration)
    
    # length: Optional[str] = None
    try:
        return parse_variables(declaration, description, shared_attributes)     
    except Exception as e:
        # You might want to log this or handle it differently
        raise ValueError(f"Error parsing variable declaration: {str(e)}") from e
    
# TODO public declaration should be handled here?


# def split_args(s: str) -> List[str]:
#     """Split function arguments, respecting nested parentheses.

#     Args:
#         s: The argument string

#     Returns:
#         A list of individual arguments

#     Examples:
#         split_args("1, 2")            -> ["1", "2"]
#         split_args("f(1,2), 3")       -> ["f(1,2)", "3"]
#         split_args("1, g(2,h(3,4))") -> ["1", "g(2,h(3,4))"]
#     """
#     result = []
#     current = ""
#     paren_count = 0

#     for char in s:
#         if char == ',' and paren_count == 0:
#             result.append(current.strip())
#             current = ""
#         else:
#             current += char
#             if char == '(':
#                 paren_count += 1
#             elif char == ')':
#                 paren_count -= 1

#     if current:
#         result.append(current.strip())
#     return result

# TODO: this is to link any function calls in dimensions to their definition -
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
