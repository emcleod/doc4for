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
    Bind
)
from fparser.one.typedecl_statements import TypeDeclarationStatement
from doc4for.models.common import BindingType, BindingTypeEnum
from doc4for.models.module_models import ModuleDescription, ProgramDescription, Uses, BlockDataDescription
from doc4for.models.variable_models import VariableDescription
from doc4for.utils.comment_utils import is_doc4for_comment, format_comments
from doc4for.parse.variable_parser import parse_variable
from doc4for.parse.array_utils import calculate_array_size, expand_array_values

# TODO move these to separate parsers like type_parser
# TODO should use the handlers instead of testing for cases

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

    internal_comment_stack = []
    variable_descriptions: Dict[str, VariableDescription] = {}
    common_block_comments: Dict[str, str] = {}
    common_block_bindings: Dict[str, BindingType] = {}  # Track bindings for common blocks

    # First pass: collect type declarations, common block comments, and bindings
    for item in block_data.content:
        if isinstance(item, Comment) and item.content:
            internal_comment_stack.append(item)
            continue

        if isinstance(item, TypeDeclarationStatement):
            # Handle variable declarations (existing code)
            variables = parse_variable(item, [])
            for variable in variables:
                variable_descriptions[variable["name"]] = variable
                if is_doc4for_comment(internal_comment_stack):
                    variable_descriptions[variable["name"]]["description"] = format_comments(
                        internal_comment_stack)
        elif isinstance(item, Common):
            # Handle common block (existing code)
            common_name = item.items[0][0] or ""
            if is_doc4for_comment(internal_comment_stack):
                common_block_comments[common_name] = format_comments(
                    internal_comment_stack)
        elif isinstance(item, Bind):
            # Handle binding for common blocks
            binding_type = extract_binding(item)
            # Parse the common block names from the items field
            # Each item has format "/ block_name /"
            for common_item in item.items:
                # Strip slashes and whitespace to get the actual common block name
                common_name = common_item.strip('/').strip()
                if common_name:
                    common_block_bindings[common_name] = binding_type

        # Clear after each non-comment item
        internal_comment_stack.clear()

    # Second pass: build the block data structure
    for item in block_data.content:
        if isinstance(item, Common):
            common_name = item.items[0][0] or ""
            variable_names = item.items[0][1]
            
            # Create common block structure with binding information
            block_data_details["common_blocks"][common_name] = {}
            common_block = block_data_details["common_blocks"][common_name]
            common_description = common_block_comments.get(common_name, "")
            
            # Add binding type if available, otherwise use default
            if common_name in common_block_bindings:
                common_block["binding_type"] = common_block_bindings[common_name]
            else:
                common_block["binding_type"] = {
                    "type": BindingTypeEnum.DEFAULT,
                    "name": None
                }
            
            # Create a variables dict to hold the actual variables
            common_block["variables"] = {}
            
            # Add variables to the common block
            for variable_name in variable_names:
                if variable_name in variable_descriptions:
                    common_block["variables"][variable_name] = variable_descriptions[variable_name].copy()
                    if not common_block["variables"][variable_name]["description"] and common_description:
                        common_block["variables"][variable_name]["description"] = common_description
                else:
                    common_block["variables"][variable_name] = {
                        "description": common_description,
                        "type": "",
                        "name": variable_name,
                        "dimension": None,
                        "attributes": [],
                        "kind": None,
                        "initial_value": None,
                        "length": None
                    }
        elif isinstance(item, Data):
            for var_names, values in item.stmts:
                parse_data_statement(var_names, values, block_data_details)

    return block_data_details


def parse_data_statement(var_names, values, block_data_details):
    value_index = 0
    for var_name in var_names:
        for common_block in block_data_details["common_blocks"].values():
            # Check if this common block has a variables dict and the variable is in it
            if "variables" in common_block and var_name in common_block["variables"]:
                var_info = common_block["variables"][var_name]
                if var_info.get("dimension"):
                    # It's an array, determine how many values to take
                    array_size = calculate_array_size(
                        var_info["dimension"]["dimensions"])

                    # Expand any repeat expressions in the values
                    var_info["initial_value"], offset = expand_array_values(
                        values, array_size, value_index)
                    value_index += offset
                else:
                    # It's a scalar
                    var_info["initial_value"] = str(values[value_index])
                    value_index += 1

def initialise_module_description(
    module: Module, comment_stack: List[Comment], file_name: str
) -> ModuleDescription:
    module_data: ModuleDescription = {
        "module_name": module.name,
        "parameters": {},
        "variables": {},
        "functions": {},
        "subroutines": {},
        "interfaces": [],
        "types": {},
        "file_name": file_name,
        "module_description": "",
    }
    if is_doc4for_comment(comment_stack):
        module_data["module_description"] = format_comments(comment_stack)
    return module_data


def extract_binding(bind_item) -> BindingType:
    """
    Extract binding type information directly from fparser Bind object.
    
    Args:
        bind_item: fparser Bind object
    
    Returns:
        BindingType with type and name fields
    """
    binding_type: BindingType = {
        'type': BindingTypeEnum.DEFAULT,
        'name': None
    }
    
    if not hasattr(bind_item, 'specs') or not bind_item.specs:
        return binding_type
        
    # First element is typically the language code
    if bind_item.specs and bind_item.specs[0].upper() == 'C':
        binding_type['type'] = BindingTypeEnum.BIND_C
        
        # Look for name parameter in any position in the specs
        for spec in bind_item.specs[1:]:
            if 'NAME' in spec.upper():
                # Extract the name string, handling different quote styles
                name_match = re.search(r"NAME\s*=\s*['\"](.+?)['\"]", spec, re.IGNORECASE)
                if name_match:
                    binding_type['name'] = name_match.group(1)
                    
    return binding_type

# TODO public declaration should be handled here?


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
