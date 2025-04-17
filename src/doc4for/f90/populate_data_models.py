from typing import List, Type, Dict
import re
from fparser.one.block_statements import (
    Module,
    Comment,
    Program,
    Use,
    BlockData,
    Common,
    Data,
    Bind
)
from fparser.one.typedecl_statements import TypeDeclarationStatement
from doc4for.models.common import BindingType, BindingTypeEnum
from doc4for.models.module_models import ModuleDescription, ProgramDescription, BlockDataDescription, CommonBlockDescription, DataStatementDescription
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
            from doc4for.parse.base_parser import handle_use
            handle_use(program_child, program_details, comment_stack)
    return program_details

def parse_block_data(block_data: BlockData, comment_stack: List[Comment]) -> BlockDataDescription:
    block_data_details: BlockDataDescription = {
        "name": block_data.name,
        "description": "",
        "common_blocks": {},
        "data_statements": [],
        "other_variables": {}
    }
    
    if is_doc4for_comment(comment_stack):
        block_data_details["description"] = format_comments(comment_stack)
    
    # Collect declarations, comments, and bindings
    declarations = collect_declarations_and_bindings(block_data)
    
    # Process each item in the block data
    for item in block_data.content:
        if isinstance(item, Common):
            common_block = parse_common_block(item, declarations)
            block_data_details["common_blocks"][common_block["name"]] = common_block
            
            # Remove variables that are now in common blocks from the general variable list
            for var_name in common_block["variables"]:
                if var_name in declarations["variables"]:
                    del declarations["variables"][var_name]
        elif isinstance(item, Data):
            data_statements = parse_data_statements(item, block_data_details)
            block_data_details["data_statements"].extend(data_statements)
    
    # In proper block data units, all variables should be in common blocks. However, we
    # store any that aren't
    if declarations["variables"] and "variables" not in block_data_details:
        block_data_details["other_variables"] = declarations["variables"]
    
    return block_data_details


def collect_declarations_and_bindings(block_data: BlockData) -> Dict:
    """First pass: collect type declarations, common block comments, and bindings"""
    declarations = {
        "variables": {},
        "common_comments": {},
        "common_bindings": {}
    }
    
    internal_comment_stack = []
    
    for item in block_data.content:
        if isinstance(item, Comment) and item.content:
            internal_comment_stack.append(item)
            continue
            
        if isinstance(item, TypeDeclarationStatement):
            variables = parse_variable(item, internal_comment_stack)
            for variable in variables:
                declarations["variables"][variable["name"]] = variable
        elif isinstance(item, Common):
            common_name = item.items[0][0] or ""
            if is_doc4for_comment(internal_comment_stack):
                declarations["common_comments"][common_name] = format_comments(internal_comment_stack)
                
        elif isinstance(item, Bind):
            binding_type = extract_binding(item)
            for common_item in item.items:
                common_name = common_item.strip('/').strip()
                if common_name:
                    declarations["common_bindings"][common_name] = binding_type
        
        internal_comment_stack.clear()
    
    return declarations

def parse_common_block(common_item: Common, declarations: Dict) -> CommonBlockDescription:
    common_name = common_item.items[0][0] or ""
    variable_names = common_item.items[0][1]
    
    common_block = {
        "name": common_name,
        "description": declarations["common_comments"].get(common_name, ""),
        "binding_type": declarations["common_bindings"].get(
            common_name, 
            {"type": BindingTypeEnum.DEFAULT, "name": None}
        ),
        "variables": {}
    }
    
    for variable_name in variable_names:
        if variable_name in declarations["variables"]:
            # Move the variable to the common block
            common_block["variables"][variable_name] = declarations["variables"][variable_name]
            # Optionally update description if needed
            if not common_block["variables"][variable_name]["description"] and common_block["description"]:
                common_block["variables"][variable_name]["description"] = common_block["description"]
        else:
            pass
    
    return common_block

def parse_data_statements(data_item: Data, block_data_details: BlockDataDescription) -> List[DataStatementDescription]:
    """Parse data statements and update variable initial values"""
    data_statements = []
    value_index = 0
    
    for var_names, values in data_item.stmts:
        for var_name in var_names:
            # Check if this is an implied DO loop
            is_do_loop = var_name.startswith('(') and '=' in var_name
            # Check if this is a substring (contains colon)
            is_substring = ':' in var_name and '(' in var_name
            
            base_name = var_name
            implied_init = ""
            initial_value = str(values[value_index])  # Default to single value
            
            # Extract the real variable name if needed
            if is_do_loop:
                # Handle DO loops as before
                inner_content = var_name.strip('()')
                
                if '(' in inner_content:
                    base_name = inner_content.split('(', 1)[0].strip()
                    implied_init = inner_content
                else:
                    parts = inner_content.split(',', 1)
                    base_name = parts[0].strip()
                    if len(parts) > 1:
                        implied_init = parts[1].strip()
                        
            elif is_substring:
                # Handle substring initializations
                base_name = var_name.split('(', 1)[0].strip()
                substring_part = var_name.split('(', 1)[1].strip(')')
                implied_init = f"substring({substring_part})"

            # Find the variable in common blocks to check dimensions
            var_info = None
            for common_block in block_data_details["common_blocks"].values():
                if base_name in common_block["variables"]:
                    var_info = common_block["variables"][base_name]
                    break
            
            # Determine if this is an array and expand values if needed
            if var_info and var_info.get("dimension"):
                if not is_do_loop:
                    array_size = calculate_array_size(var_info["dimension"]["dimensions"])
                    expanded_value, offset = expand_array_values(values, array_size, value_index)
                    initial_value = expanded_value  # Use expanded value for data statement
                    value_index += offset
                else:
                    # DO loop case
                    initial_value = str(values[value_index])
            else:
                # Scalar case
                initial_value = str(values[value_index])
                value_index += 1
            
            # Record the data statement with the proper value
            data_stmt = {
                "variable": base_name,
                "value": initial_value,  # Use the calculated value
                "description": None,
                "implied_initialisation": implied_init
            }
            data_statements.append(data_stmt)
            
            # Update initial value in variables (look for base_name in common blocks)
            if var_info:
                var_info["initial_value"] = initial_value
    
    return data_statements

def initialise_module_description(
    module: Module, comment_stack: List[Comment], file_name: str
) -> ModuleDescription:
    module_data: ModuleDescription = {
        "module_name": module.name,
        "parameters": {},
        "variables": {},
        "functions": {},
        "subroutines": {},
        "enums": {},
        "interfaces": [],
        "types": {},
        "uses": {},
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
