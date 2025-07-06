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
    Implicit_Part,
    Bind_Stmt,
    Language_Binding_Spec,
    Bind_Entity,
    Save_Stmt,
    Saved_Entity
)
from fparser.two.utils import walk
from doc4for.models.module_models import BlockDataDescription, DataStatementDescription, CommonBlockDescription
from doc4for.models.variable_models import VariableDescription
from doc4for.utils.comment_utils import format_comments
from doc4for.parse.variable_parser import parse_variable
from doc4for.models.dimension_models import Dimension
from doc4for.parse.common_parser import _extract_dimension_info, _extract_binding_type

from doc4for.logging_config import setup_logging

logger: logging.Logger = logging.getLogger(__name__)

def parse_common_block(common_block_stmt: Common_Stmt, comment_stack: List[Comment]) -> Dict[str, CommonBlockDescription]:
    common_blocks = {}
    for child in common_block_stmt.children:
        common_block_name = None
        variables = {}
        for block in child:
            common_block_decl, declarations = block
            common_block_name = common_block_decl.string if common_block_decl else ""
            variables = {}
            for declaration in declarations.children:
                name = declaration.children[0].string if declaration.children else declaration.string
                dimension: Dimension = None
                if len(declaration.children) > 1:
                    dimension = _extract_dimension_info(declaration.children[1])
                # just get the dimensions - the post-processing step will
                # match up the remainder of the fields
                variable_description: VariableDescription = {
                    "description": "",
                    "type": "",
                    "name": name,
                    "dimension": dimension,
                    "polymorphism_type": None,
                    "attributes": [],
                    "kind": "",
                    "initial_value": "",
                    "length": "",
                    "binding_type": None,
                    "is_saved": False
                }        
                variables[name] = variable_description
            if common_block_name in common_blocks:
                common_blocks[common_block_name]["variables"].update(variables)
                # Merge descriptions
                existing_desc = common_blocks[common_block_name]["description"]
                new_desc = format_comments(comment_stack)
                if existing_desc and new_desc:
                    common_blocks[common_block_name]["description"] = f"{existing_desc}\n{new_desc}"
                elif new_desc:  # Only new description exists
                    common_blocks[common_block_name]["description"] = new_desc                
            if common_block_name in common_blocks:
                common_blocks[common_block_name]["variables"].update(variables)
            else:
                common_block = {
                    "name": common_block_name,
                    "description": format_comments(comment_stack),
                    "variables": variables,
                    "binding_type": None,  
                }
                common_blocks[common_block_name] = common_block
    return common_blocks

def parse_block_data(block_data: Block_Data, comment_stack: List[Comment]) -> Tuple[str, BlockDataDescription]:
    block_data_decl = walk(block_data, Block_Data_Stmt)
    block_data_names = walk(block_data_decl, Name)
    name = block_data_names[0].string if block_data_names else ""
    decl_comment_stack = []
    common_blocks = {}
    data_statements = []
    other_variables = {}
    variable_to_common_block = {}  # Map variable names to their common block names
    
    # Add save tracking
    save_variables: List[str] = []
    save_common_blocks: List[str] = []
    save_all: bool = False

    for child in block_data.children:
        if isinstance(child, Specification_Part):
            # First pass: collect comments, common block declarations and variable mappings
            for spec_child in child.children:
                if isinstance(spec_child, Comment):
                    decl_comment_stack.append(spec_child)
                elif isinstance(spec_child, Implicit_Part):
                    decl_comment_stack.extend(walk(spec_child, Comment))
                elif isinstance(spec_child, Common_Stmt):
                    common_blocks_desc = parse_common_block(spec_child, decl_comment_stack)
                    for common_block_name, common_block in common_blocks_desc.items():
                        for variable in common_block["variables"]:
                            variable_to_common_block[variable] = common_block_name
                    common_blocks.update(common_blocks_desc)
                    decl_comment_stack.clear()
                elif isinstance(spec_child, Save_Stmt):
                    # Process SAVE statements
                    _, saved_entity_list = spec_child.children
                    if saved_entity_list is None:
                        # Blanket save - saves everything
                        save_all = True
                    else:
                        # Process each saved entity
                        for saved_entity in saved_entity_list.children:
                            if isinstance(saved_entity, Saved_Entity):
                                # Common block format: Saved_Entity('/', Name('counters'), '/')
                                if len(saved_entity.children) == 3 and saved_entity.children[0] == '/' and saved_entity.children[2] == '/':
                                    common_name = saved_entity.children[1].string
                                    save_common_blocks.append(common_name)
                            elif isinstance(saved_entity, Name):
                                # Regular variable
                                save_variables.append(saved_entity.string)
                    decl_comment_stack.clear()
                else:
                    # don't want to pick up any comments that aren't in front of common blocks
                    decl_comment_stack.clear()

            # Second pass: process type declarations, c bindings and data statements
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
                elif isinstance(spec_child, Bind_Stmt):
                    binding_type = _extract_binding_type(walk(spec_child, Language_Binding_Spec))
                    bindings = spec_child.children[1]
                    for binding in bindings.children:
                        if isinstance(binding, Name):
                            # Binding to a variable
                            var_name = binding.string
                            if var_name in other_variables:
                                other_variables[var_name]["binding_type"] = binding_type
                            else:
                                # Check if it's in a common block
                                if var_name in variable_to_common_block:
                                    common_block_name = variable_to_common_block[var_name]
                                    common_blocks[common_block_name]["variables"][var_name]["binding_type"] = binding_type
                                else:
                                    logger.warning(f"BIND statement for variable '{var_name}' but variable not found")
                        elif isinstance(binding, Bind_Entity):
                            binding_name = walk(binding, Name)[0].string
                            if binding_name in common_blocks:
                                common_blocks[binding_name]["binding_type"] = binding_type
                            else:
                                #TODO something sensible or log
                                pass
                        else:
                            #TODO something sensible or log
                            pass
                else:
                    # don't want any comments that aren't immediately before type declarations or data statements
                    decl_comment_stack.clear()

    block_data_desc: BlockDataDescription = {
        "name": name,
        "description": format_comments(comment_stack),
        "common_blocks": common_blocks,
        "data_statements": data_statements,
        "other_variables": other_variables,
    }
    
    # Process save statements using the same function as modules
    if save_all or save_variables or save_common_blocks:
        process_save_statements_block_data(block_data_desc, save_variables, save_common_blocks, save_all)
    
    return name, block_data_desc


def process_save_statements_block_data(block_data: BlockDataDescription, 
                                      save_variables: List[str], 
                                      save_common_blocks: List[str], 
                                      save_all: bool) -> None:
    """Process SAVE statements and mark variables as saved in block data."""
    
    if save_all:
        # Mark all variables as saved (both in common blocks and other_variables)
        for common_block in block_data.get("common_blocks", {}).values():
            for var_desc in common_block["variables"].values():
                var_desc["is_saved"] = True
        for var_desc in block_data.get("other_variables", {}).values():
            var_desc["is_saved"] = True
    else:
        # Process explicit variable saves
        for var_name in save_variables:
            # Check in common blocks
            found = False
            for common_block in block_data.get("common_blocks", {}).values():
                if var_name in common_block["variables"]:
                    common_block["variables"][var_name]["is_saved"] = True
                    found = True
                    break
            # Check in other_variables
            if not found and var_name in block_data.get("other_variables", {}):
                block_data["other_variables"][var_name]["is_saved"] = True
                found = True
            if not found:
                logger.warning(f"SAVE statement for variable '{var_name}' but variable not found in block data")
        
        # Process common block saves (mark all variables in the common block as saved)
        for common_name in save_common_blocks:
            if common_name in block_data.get("common_blocks", {}):
                common_block = block_data["common_blocks"][common_name]
                # Mark all variables in this common block as saved
                for var_desc in common_block["variables"].values():
                    var_desc["is_saved"] = True
            else:
                logger.warning(f"SAVE statement for common block '/{common_name}/' but common block not found in block data")

# def parse_block_data(block_data: Block_Data, comment_stack: List[Comment]) -> Tuple[str, BlockDataDescription]:
#     block_data_decl = walk(block_data, Block_Data_Stmt)
#     block_data_names = walk(block_data_decl, Name)
#     name = block_data_names[0].string if block_data_names else ""
#     decl_comment_stack = []
#     common_blocks = {}
#     data_statements = []
#     other_variables = {}
#     variable_to_common_block = {}  # Map variable names to their common block names

#     for child in block_data.children:
#         if isinstance(child, Specification_Part):
#             # First pass: collect comments, common block declarations and variable mappings
#             for spec_child in child.children:
#                 if isinstance(spec_child, Comment):
#                     decl_comment_stack.append(spec_child)
#                 elif isinstance(spec_child, Implicit_Part):
#                     decl_comment_stack.extend(walk(spec_child, Comment))
#                 elif isinstance(spec_child, Common_Stmt):
#                     common_blocks_desc = parse_common_block(spec_child, decl_comment_stack)
#                     for common_block_name, common_block in common_blocks_desc.items():
#                         for variable in common_block["variables"]:
#                             variable_to_common_block[variable] = common_block_name
#                     common_blocks.update(common_blocks_desc)
#                     decl_comment_stack.clear()
#                 else:
#                     # don't want to pick up any comments that aren't in front of common blocks
#                     decl_comment_stack.clear()

#             # Second pass: process type declarations, c bindings and data statements
#             for spec_child in child.children:
#                 if isinstance(spec_child, Comment):
#                     decl_comment_stack.append(spec_child)
#                 elif isinstance(spec_child, Implicit_Part):
#                     decl_comment_stack.extend(walk(spec_child, Comment))
#                 elif isinstance(spec_child, Type_Declaration_Stmt):
#                     # get the type data from the declaration
#                     variable_descs = parse_variable(spec_child, decl_comment_stack, [])
#                     for variable_desc in variable_descs:
#                         variable_name = variable_desc["name"]
#                         # Find which common block this variable belongs to
#                         if variable_name in variable_to_common_block:
#                             common_block_name = variable_to_common_block[variable_name]
#                             common_blocks[common_block_name]["variables"][variable_name] = variable_desc
#                         else:
#                             other_variables[variable_name] = variable_desc
#                     decl_comment_stack.clear()
#                 elif isinstance(spec_child, Data_Stmt):
#                     new_data_statements = _process_data_statement(
#                         spec_child, 
#                         decl_comment_stack, 
#                         variable_to_common_block, 
#                         common_blocks, 
#                         other_variables
#                     )
#                     data_statements.extend(new_data_statements)
#                     decl_comment_stack.clear()
#                 elif isinstance(spec_child, Bind_Stmt):
#                     binding_type = _extract_binding_type(walk(spec_child, Language_Binding_Spec))
#                     bindings = spec_child.children[1]
#                     for binding in bindings.children:
#                         if isinstance(binding, Name):
#                             # TODO have a binding to a variable which we can't handle now
#                             pass
#                         elif isinstance(binding, Bind_Entity):
#                             binding_name = walk(binding, Name)[0].string
#                             if binding_name in common_blocks:
#                                 common_blocks[binding_name]["binding_type"] = binding_type
#                             else:
#                                 #TODO something sensible or log
#                                 pass
#                         else:
#                             #TODO something sensible or log
#                             pass
#                 else:
#                     # don't want any comments that aren't immediately before type declarations or data statements
#                     decl_comment_stack.clear()

#     return name, {
#         "name": name,
#         "description": format_comments(comment_stack),
#         "common_blocks": common_blocks,
#         "data_statements": data_statements,
#         "other_variables": other_variables,
#     }


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

# def parse_save_statement():
#     pass #TODO

# def parse_equivalence_statement():
#     pass #TODO

