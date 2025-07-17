import logging
from typing import List, Type, Dict, Tuple, Any, Optional, Callable
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
    Saved_Entity,
    Data_Implied_Do,
    Data_Stmt_Value,
    Int_Literal_Constant,
    Complex_Literal_Constant,
    Real_Literal_Constant,
    Logical_Literal_Constant,
    Char_Literal_Constant,
    Part_Ref,
    Data_Stmt_Set,
    Section_Subscript_List,
    Subscript_Triplet
)
from fparser.two.utils import walk
from doc4for.models.module_models import BlockDataDescription, CommonBlockDescription
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
                    _process_data_statement(
                        spec_child,  
                        variable_to_common_block, 
                        common_blocks, 
                        other_variables
                    )
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

def extract_data_value(node) -> str:
    """Extract string representation from various data value node types."""
    if isinstance(node, Data_Stmt_Value):
        count_literal, value_literal = node.children
        
        # Handle parameter names in count position
        if isinstance(count_literal, Name):
            count = count_literal.string
        else:
            count = count_literal.children[0] if count_literal.children else "1"
        
        # Handle parameter names in value position
        if isinstance(value_literal, Name):
            value = value_literal.string
        else:
            value = extract_data_value(value_literal)
            
        return f"{count}*{value}"
    elif isinstance(node, Name):
        # Handle direct parameter names
        return node.string
    elif isinstance(node, Complex_Literal_Constant):
        real_part, imag_part = node.children
        real_val = real_part.children[0] if real_part.children else "0"
        imag_val = imag_part.children[0] if imag_part.children else "0"
        return f"({real_val}, {imag_val})"
    elif isinstance(node, (Int_Literal_Constant, Real_Literal_Constant, 
                          Char_Literal_Constant, Logical_Literal_Constant)):
        return node.children[0]
    else:
        # Fallback to string representation
        return str(node)

def process_data_statement_core(
    data_stmt: Data_Stmt,
    update_variable_func: Callable[[str, str], None],
    get_variable_info_func: Callable[[str], Tuple[Optional[Dict], Optional[Dict]]]
) -> None:
    """
    Core logic for processing DATA statements that works for both module and block data contexts.
    
    Args:
        data_stmt: The DATA statement to process
        update_variable_func: Function to update a variable's initial value
        get_variable_info_func: Function to retrieve variable information
    """
    
    def process_implied_do(do_obj):
        """Process implied DO and return the pattern string and base variable name."""
        first_child = do_obj.children[0]  # Data_I_Do_Object_List
        loop_var = do_obj.children[1].string
        start = do_obj.children[2].string
        end = do_obj.children[3].string
        
        # Check if the first child contains another implied DO (nested case)
        nested_implied_do = walk(first_child, Data_Implied_Do)
        if nested_implied_do:
            inner_str, var_name = process_implied_do(nested_implied_do[0])
            # For nested loops, wrap inner in parentheses
            return f"({inner_str}), {loop_var}={start},{end}", var_name
        else:
            # Base case: extract variable from Data_I_Do_Object_List
            var_refs = walk(first_child, Part_Ref)
            if var_refs:
                var_ref = var_refs[0]
                var_name = var_ref.children[0].string
                var_pattern = var_ref.string
                return f"{var_pattern}, {loop_var}={start},{end}", var_name
            else:
                # Fallback if Part_Ref not found
                name_nodes = walk(first_child, Name)
                if name_nodes:
                    var_name = name_nodes[0].string
                    return f"{var_name}, {loop_var}={start},{end}", var_name
                return f"unknown, {loop_var}={start},{end}", "unknown"
    
    # Process each data statement set
    data_stmt_sets = walk(data_stmt, Data_Stmt_Set)
    for data_stmt_set in data_stmt_sets:
        object_list, value_list = data_stmt_set.children
        
        # Check if this is an implied DO loop
        implied_do_nodes = walk(object_list, Data_Implied_Do)
        if implied_do_nodes:
            implied_do = implied_do_nodes[0]
            
            # Process the implied DO and get both the pattern and variable name
            implied_pattern, var_name = process_implied_do(implied_do)
            
            # Get values using the improved extraction function
            values = []
            for child in value_list.children:
                values.append(extract_data_value(child))
            
            value_str = ", ".join(values)
            
            # Create the full implied DO initialization string
            full_init_value = f"({implied_pattern}) = {value_str}"
            
            # Update the variable
            update_variable_func(var_name, full_init_value)
            
        else:
            # Handle regular DATA statements (non-implied DO)
            # Get variable names and handle substring references
            var_infos = []
            for child in object_list.children:
                if isinstance(child, Name):
                    var_infos.append((child.string, None))
                elif isinstance(child, Part_Ref):
                    base_name = child.children[0].string
                    # Extract substring range if present
                    section_info = None
                    if len(child.children) > 1:
                        section_subscripts = walk(child, Section_Subscript_List)
                        if section_subscripts:
                            section_list = section_subscripts[0]
                            triplets = walk(section_list, Subscript_Triplet)
                            if triplets:
                                triplet = triplets[0]
                                start = triplet.children[0].string if triplet.children[0] else "1"
                                end = triplet.children[1].string if triplet.children[1] else None
                                section_info = f"({start}:{end})"
                    var_infos.append((base_name, section_info))
            
            # Get values using the improved extraction function
            values = []
            for child in value_list.children:
                values.append(extract_data_value(child))
            
            # Handle different variable-to-value mappings
            if len(var_infos) == 1 and len(values) > 1:
                # Single variable, multiple values (array initialization)
                var_name, section_info = var_infos[0]
                value_str = ", ".join(values)
                
                if section_info:
                    value_str = f"{section_info}={value_str}"
                
                update_variable_func(var_name, value_str)
                
            elif len(var_infos) == len(values):
                # One-to-one mapping
                for (var_name, section_info), value in zip(var_infos, values):
                    if section_info:
                        value = f"{section_info}={value}"
                    
                    update_variable_func(var_name, value)
            elif len(var_infos) > 1 and len(values) > len(var_infos):
                # Multiple variables, multiple values - distribute based on array sizes
                value_index = 0
                
                for var_name, section_info in var_infos:
                    # Get variable info to determine array size
                    var_info, _ = get_variable_info_func(var_name)
                    
                    if var_info and var_info.get("dimension"):
                        # Calculate total array size
                        array_size = 1
                        for dim in var_info["dimension"]["dimensions"]:
                            # Assuming you have a way to get the size from bounds
                            lower = int(dim.lower.value) if dim.lower else 1
                            upper = int(dim.upper.value) if dim.upper else 1
                            array_size *= (upper - lower + 1)
                    else:
                        # Scalar variable
                        array_size = 1
                    
                    # Extract the appropriate number of values
                    var_values = values[value_index:value_index + array_size]
                    value_str = ", ".join(var_values)
                    
                    if section_info:
                        value_str = f"{section_info}={value_str}"
                    
                    update_variable_func(var_name, value_str)
                    value_index += array_size                    
            else:
                # Handle special cases like substring initialization
                for var_name, section_info in var_infos:
                    base_var_name = var_name.split('(')[0] if '(' in var_name else var_name
                    
                    if section_info:
                        # Substring initialization
                        var_info, _ = get_variable_info_func(base_var_name)
                        existing_value = var_info.get("initial_value", "") if var_info else ""
                        
                        cleaned_value = values[0].strip("'") if values else ""
                        
                        if existing_value:
                            combined_value = f"{existing_value}, {section_info}='{cleaned_value}'"
                        else:
                            combined_value = f"{section_info}='{cleaned_value}'"
                        
                        update_variable_func(base_var_name, combined_value)
                    else:
                        # Regular initialization
                        if values:
                            update_variable_func(var_name, values[0])

# For block data context
def create_block_data_adapters(variable_to_common_block, common_blocks, other_variables):
    """Create adapter functions for block data context."""
    
    def update_variable(var_name: str, value: str) -> None:
        _update_variable_initial_value(
            var_name, value, variable_to_common_block, 
            common_blocks, other_variables
        )
    
    def get_variable_info(var_name: str) -> Tuple[Optional[Dict], Optional[Dict]]:
        return _get_variable_info(
            var_name, variable_to_common_block, common_blocks, other_variables
        )
    
    return update_variable, get_variable_info

# For module context
def create_module_adapters(module_data, variable_initializations):
    """Create adapter functions for module context."""
    
    def update_variable(var_name: str, value: str) -> None:
        if var_name in variable_initializations:
            variable_initializations[var_name].append(value)
        else:
            variable_initializations[var_name] = [value]
    
    def get_variable_info(var_name: str) -> Tuple[Optional[Dict], Optional[Dict]]:
        if var_name in module_data["variables"]:
            return module_data["variables"][var_name], None
        return None, None
    
    return update_variable, get_variable_info

# Updated block data function
def _process_data_statement(
    data_stmt: Data_Stmt,
    variable_to_common_block: Dict[str, str],
    common_blocks: Dict[str, Any],
    other_variables: Dict[str, Any]
) -> None:
    update_func, get_info_func = create_block_data_adapters(
        variable_to_common_block, common_blocks, other_variables
    )
    
    process_data_statement_core(data_stmt, update_func, get_info_func)
                          

# def parse_equivalence_statement():
#     pass #TODO

