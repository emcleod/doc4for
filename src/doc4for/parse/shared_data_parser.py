import logging
from typing import List, Type, Dict, Tuple, Any
from fparser.two.Fortran2003 import (
    Block_Data,
    Block_Data_Stmt,
    Comment,
    Name,
    Specification_Part,
    Type_Declaration_Stmt,
    Common_Stmt,
    Data_Stmt,
    Implicit_Part
)
from fparser.two.utils import walk
from doc4for.models.common import BindingTypeEnum
from doc4for.models.module_models import BlockDataDescription, DataStatementDescription
from doc4for.utils.comment_utils import format_comments
from doc4for.parse.variable_parser import parse_variable

from doc4for.logging_config import setup_logging

logger: logging.Logger = logging.getLogger(__name__)


def parse_block_data(block_data: Block_Data, comment_stack: List[Comment]) -> Tuple[str, BlockDataDescription]:
    block_data_decl = walk(block_data, Block_Data_Stmt)
    block_data_names = walk(block_data_decl, Name)
    name = block_data_names[0].string if block_data_names else ""
    decl_comment_stack = []
    common_blocks = {}
    data_statements = []
    other_variables = {}
    variable_to_common_block = {}  # Map variable names to their common block names

    for child in block_data.children:
        if isinstance(child, Specification_Part):
            # First pass: collect comments, common block declarations and variable mappings
            for spec_child in child.children:
                if isinstance(spec_child, Comment):
                    decl_comment_stack.append(spec_child)
                elif isinstance(spec_child, Implicit_Part):
                    decl_comment_stack.extend(walk(spec_child, Comment))
                elif isinstance(spec_child, Common_Stmt):
                    for common_decl_children in spec_child.children:
                        for common_decl_child in common_decl_children:
                            common_block_name = common_decl_child[0].string if common_decl_child[0] else ""
                            variable_names = [var_name.string for var_name in common_decl_child[1].children]
                            
                            # Map each variable to its common block
                            for var_name in variable_names:
                                variable_to_common_block[var_name] = common_block_name

                            common_block = {
                                "name": common_block_name,
                                "description": format_comments(decl_comment_stack),
                                "variables": {},
                                "binding_type": None,  # TODO
                            }
                            common_blocks[common_block_name] = common_block
                    decl_comment_stack.clear()
                else:
                    # don't want to pick up any comments that aren't in front of common blocks
                    decl_comment_stack.clear()

            # Second pass: process type declarations and data statements
            for spec_child in child.children:
                if isinstance(spec_child, Comment):
                    decl_comment_stack.append(spec_child)
                elif isinstance(spec_child, Implicit_Part):
                    decl_comment_stack.extend(walk(spec_child, Comment))
                elif isinstance(spec_child, Type_Declaration_Stmt):
                    # get the type data from the declaration
                    variable_descs = parse_variable(spec_child, decl_comment_stack, [])
                    for variable_desc in variable_descs:
                        variable_name = variable_desc["name"]
                        # Find which common block this variable belongs to
                        if variable_name in variable_to_common_block:
                            common_block_name = variable_to_common_block[variable_name]
                            common_blocks[common_block_name]["variables"][variable_name] = variable_desc
                        else:
                            other_variables[variable_name] = variable_desc
                    decl_comment_stack.clear()
                elif isinstance(spec_child, Data_Stmt):
                    new_data_statements = _process_data_statement(
                        spec_child, 
                        decl_comment_stack, 
                        variable_to_common_block, 
                        common_blocks, 
                        other_variables
                    )
                    data_statements.extend(new_data_statements)
                    decl_comment_stack.clear()
                else:
                    # don't want any comments that aren't immediately before type declarations or data statements
                    decl_comment_stack.clear()

    return name, {
        "name": name,
        "description": format_comments(comment_stack),
        "common_blocks": common_blocks,
        "data_statements": data_statements,
        "other_variables": other_variables,
    }

# def parse_save_statement():
#     pass #TODO

# def parse_equivalence_statement():
#     pass #TODO


def parse_data_statement(data_stmt: Data_Stmt, comment_stack: List[Comment]) -> List[DataStatementDescription]:
    # Since we don't have the context of common blocks and other variables,
    # we'll create empty dictionaries and ignore the side effects of updating initial values
    empty_variable_to_common_block = {}
    empty_common_blocks = {}
    empty_other_variables = {}
    
    return _process_data_statement(
        data_stmt,
        comment_stack,
        empty_variable_to_common_block,
        empty_common_blocks,
        empty_other_variables
    )

def _update_variable_initial_value(
    variable_name: str,
    initial_value: str,
    variable_to_common_block: Dict[str, str],
    common_blocks: Dict[str, Any],
    other_variables: Dict[str, Any]
) -> None:
    """Update the initial value for a variable in the appropriate location."""
    if variable_name in variable_to_common_block:
        common_block_name = variable_to_common_block[variable_name]
        if (common_block_name in common_blocks and 
            variable_name in common_blocks[common_block_name]["variables"]):
            common_blocks[common_block_name]["variables"][variable_name]["initial_value"] = initial_value
    elif variable_name in other_variables:
        other_variables[variable_name]["initial_value"] = initial_value

def _get_variable_info(
    variable_name: str,
    variable_to_common_block: Dict[str, str],
    common_blocks: Dict[str, Any],
    other_variables: Dict[str, Any]
) -> Tuple[Dict[str, Any], Dict[str, Any]]:
    if variable_name in variable_to_common_block:
        common_block_name = variable_to_common_block[variable_name]
        if common_block_name in common_blocks:
            target_common_block = common_blocks[common_block_name]
            if variable_name in target_common_block["variables"]:
                return target_common_block["variables"][variable_name], target_common_block
    elif variable_name in other_variables:
        return other_variables[variable_name], None
    
    return None, None

def _process_data_statement(
    data_stmt: Data_Stmt, 
    comment_stack: List[Comment],
    variable_to_common_block: Dict[str, str],
    common_blocks: Dict[str, Any],
    other_variables: Dict[str, Any]
) -> List[DataStatementDescription]:
    data_statements = []
    data_description = format_comments(comment_stack)
    data_stmt_sets = [child for child in data_stmt.children]

    for data_set in data_stmt_sets:
        if len(data_set.children) >= 2:
            var_names = data_set.children[0]
            var_values = data_set.children[1]
            implied_init = ""

            var_name_list = [var.string for var in var_names.children]
            value_list = [val.string for val in var_values.children]
            value_index = 0

            for var_name_str in var_name_list:
                var_info, _ = _get_variable_info(var_name_str, variable_to_common_block, common_blocks, other_variables)

                # Calculate array size
                array_size = 1
                if var_info and var_info.get("dimension") and var_info["dimension"].get("dimensions"):
                    for dim in var_info["dimension"]["dimensions"]:
                        if hasattr(dim, "upper") and hasattr(dim, "lower"):
                            upper_val = int(dim.upper.value)
                            lower_val = int(dim.lower.value)
                            array_size *= upper_val - lower_val + 1

                # Extract values for this variable
                if value_index + array_size <= len(value_list):
                    var_values_for_this_var = value_list[value_index:value_index + array_size]
                    value_index += array_size
                else:
                    var_values_for_this_var = value_list[value_index:]
                    value_index = len(value_list)

                var_value_str = ", ".join(var_values_for_this_var)

                # Create data statement entry
                data_statements.append({
                    "variable": var_name_str,
                    "value": var_value_str,
                    "description": data_description,
                    "implied_initialisation": implied_init,
                })

                # Update variable's initial value using separated function
                _update_variable_initial_value(
                    var_name_str, var_value_str, variable_to_common_block, 
                    common_blocks, other_variables
                )
    
    return data_statements
# def _process_data_statement(
#     data_stmt: Data_Stmt, 
#     comment_stack: List[Comment],
#     variable_to_common_block: Dict[str, str],
#     common_blocks: Dict[str, Any],
#     other_variables: Dict[str, Any]
# ) -> List[DataStatementDescription]:
#     data_statements = []
    
#     # Get the description from any preceding comments
#     data_description = format_comments(comment_stack)
#     # A Data_Stmt can contain multiple Data_Stmt_Set objects
#     data_stmt_sets = [child for child in data_stmt.children]

#     for data_set in data_stmt_sets:
#         if len(data_set.children) >= 2:
#             var_names = data_set.children[0]  # Data_Stmt_Object_List
#             var_values = data_set.children[1]  # Data_Stmt_Value_List

#             # TODO check if this is an implied DO loop or other complex initialization
#             implied_init = ""

#             # Get all variable names and all values
#             var_name_list = [var.string for var in var_names.children]
#             value_list = [val.string for val in var_values.children]

#             # Distribute values among variables based on their array sizes
#             value_index = 0

#             for var_name_str in var_name_list:
#                 # Find the variable to get its size
#                 var_info = None
#                 target_common_block = None
                
#                 # Look for the variable in the appropriate common block
#                 if var_name_str in variable_to_common_block:
#                     common_block_name = variable_to_common_block[var_name_str]
#                     if common_block_name in common_blocks:
#                         target_common_block = common_blocks[common_block_name]
#                         if var_name_str in target_common_block["variables"]:
#                             var_info = target_common_block["variables"][var_name_str]
#                 elif var_name_str in other_variables:
#                     var_info = other_variables[var_name_str]

#                 # Determine how many values this variable needs
#                 if var_info and var_info.get("dimension") and var_info["dimension"].get("dimensions"):                                
#                     # Calculate total array size
#                     array_size = 1
#                     for dim in var_info["dimension"]["dimensions"]:
#                         if hasattr(dim, "upper") and hasattr(dim, "lower"):
#                             upper_val = int(dim.upper.value)
#                             lower_val = int(dim.lower.value)
#                             array_size *= upper_val - lower_val + 1
#                 else:
#                     # Scalar variable
#                     array_size = 1

#                 # Extract the values for this variable
#                 if value_index + array_size <= len(value_list):
#                     var_values_for_this_var = value_list[
#                         value_index : value_index + array_size
#                     ]
#                     var_value_str = ", ".join(var_values_for_this_var)
#                     value_index += array_size
#                 else:
#                     # Not enough values left - this might be an error in the Fortran code
#                     # but we'll handle it gracefully
#                     remaining_values = value_list[value_index:]
#                     var_value_str = ", ".join(remaining_values)
#                     value_index = len(value_list)

#                 # Create data statement entry
#                 data_stmt_entry = {
#                     "variable": var_name_str,
#                     "value": var_value_str,
#                     "description": data_description,
#                     "implied_initialisation": implied_init,
#                 }
#                 data_statements.append(data_stmt_entry)

#                 # Update the variable's initial_value in the correct location
#                 if target_common_block and var_name_str in target_common_block["variables"]:
#                     target_common_block["variables"][var_name_str]["initial_value"] = var_value_str
#                 elif var_name_str in other_variables:
#                     other_variables[var_name_str]["initial_value"] = var_value_str
    
#     return data_statements




