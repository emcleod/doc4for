import logging
from typing import Any, Optional, List, Dict
from fparser.two.utils import walk
from fparser.two.Fortran2003 import (
    Comment,
    Type_Declaration_Stmt,
    Derived_Type_Def,
    Specification_Part,
    Implicit_Part,
    Dimension_Stmt,
    Parameter_Stmt,
    Named_Constant_Def,
    Access_Stmt,
    Name,
    Module_Subprogram_Part,
    Function_Subprogram,
    Subroutine_Subprogram,
    Interface_Block,
    Enum_Def,
    Common_Stmt,
    Bind_Stmt,
    Language_Binding_Spec,
    Save_Stmt,
    Saved_Entity,
    Equivalence_Stmt,
    Data_Stmt,
    Use_Stmt
)
from doc4for.models.module_models import ModuleDescription
from doc4for.models.variable_models import ParameterDescription
from doc4for.parse.common_parser import FortranHandler, _extract_binding_type
from doc4for.models.common import BindingType
from doc4for.parse.shared_data_parser import create_module_adapters, process_data_statement_core#

from doc4for.parse.base_parser import (    
    VisibilityState,
    handle_function,
    handle_subroutine,
    handle_type_declaration,
    handle_derived_type,
    handle_interface,
    handle_enum,
    handle_use,
    handle_common_block,
    handle_equivalence
)
from doc4for.logging_config import setup_logging

logger: logging.Logger = logging.getLogger(__name__)

ModuleHandler = FortranHandler[ModuleDescription]

_module_handler_instance: Optional[ModuleHandler] = None

#TODO
# Namelist groups
def _get_module_handler() -> ModuleHandler:
    """Get an instance of ModuleHandler and initialize if necessary.

    Returns:
        The singleton instance of ModuleHandler.
    """
    global _module_handler_instance
    if _module_handler_instance is None:
        handler = ModuleHandler()
        handler.register_handler(Function_Subprogram, handle_function)
        handler.register_handler(Subroutine_Subprogram, handle_subroutine)
        handler.register_handler(
            Type_Declaration_Stmt, handle_type_declaration)
        handler.register_handler(
            Derived_Type_Def, handle_derived_type)
        handler.register_handler(Interface_Block, handle_interface)
        handler.register_handler(Enum_Def, handle_enum)
        handler.register_handler(Use_Stmt, handle_use)
        handler.register_handler(Common_Stmt, handle_common_block)
        handler.register_handler(Equivalence_Stmt, handle_equivalence)
        _module_handler_instance = handler
    return _module_handler_instance

def parse_module_content(module: Any, module_data: ModuleDescription, comment_stack: List[Comment],
                                     remove_private: bool = True) -> None:
    handlers = _get_module_handler()
        
    # only get the parts we care about
    content = [child for child in module.children if isinstance(child, (Specification_Part, Module_Subprogram_Part))]
    if not content:
        return
    
    # keep track of access level of entities
    default_access: str = "PUBLIC"
    #TODO does this need to be moved inside the loop?
    # for older-style fortran, dimension and type declarations can be on different lines
    dimension_stack: List[Dimension_Stmt] = []
    # for older-style fortran, can declare something to be a parameter after the type declaration
    parameter_stack: List[Parameter_Stmt] = []
    # keep track of access statements before declaration entity_name -> access
    access_stack: Dict[str, str] = {}
    # keep track of lone bind statements e.g. for binding a common block
    bind_stack: Dict[str, Optional[BindingType]] = {}
    # gather data statements to populate initial values in variables
    data_statement_stack: List[Data_Stmt] = []
    # keep track of save statements
    save_variables: List[str] = []
    save_common_blocks: List[str] = []
    save_all: bool = False
    
    for module_nodes in content:
        for node in module_nodes.children:
            if isinstance(node, Implicit_Part):
                # we don't have any object we're going to parse, so collect up the comments
                for subnode in node.children:
                    if isinstance(subnode, Comment) and hasattr(subnode, "items") and subnode.items[0]: # it's not empty                  
                        comment_stack.append(subnode)
                    elif isinstance(subnode, Parameter_Stmt):
                        parameter_stack.append(subnode)
            else:
                if isinstance(node, Dimension_Stmt):
                    dimension_stack.append(node)
                elif isinstance(node, Access_Stmt):
                    access_value, access_ids = node.children                        
                    # If access_ids is None, it's a global access statement like "private"
                    if access_ids:
                        access_stack.update({
                            name.string: access_value for name in walk(access_ids, Name)
                        })  
                    else:
                        default_access = access_value
                elif isinstance(node, Bind_Stmt):
                    # Extract binding info during parsing (like access_stack does)
                    binding_info = _extract_binding_type(walk(node, Language_Binding_Spec))
                    # Get the target entities
                    _, entity_list = node.children
                    if entity_list:
                        for entity in walk(entity_list, Name):
                            entity_name = entity.string
                            # Handle common blocks by stripping slashes
                            if entity_name.startswith('/') and entity_name.endswith('/'):
                                entity_name = entity_name[1:-1]  # Strip slashes for common blocks
                            bind_stack[entity_name] = binding_info
                elif isinstance(node, Save_Stmt):
                    # Handle save statements
                    _, saved_entity_list = node.children
                    if saved_entity_list is None:
                        # Bare 'save' - saves everything
                        save_all = True
                    else:
                        # Process the saved entities
                        for entity in saved_entity_list.children:
                            if isinstance(entity, Saved_Entity):
                                # It's a common block: save /block_name/
                                # Saved_Entity has structure: ('/', Name, '/')
                                _, name_node, _ = entity.children
                                save_common_blocks.append(name_node.string)
                            elif isinstance(entity, Name):
                                # It's a variable name
                                save_variables.append(entity.string)
                elif isinstance(node, Data_Stmt):
                    data_statement_stack.append(node)
                else:
                    handler = handlers.get_handler(type(node))
                    if handler:
                        handler(node, module_data, comment_stack, default_access, dimension_stack=dimension_stack)
                        comment_stack.clear()
                    else:
                        logger.warning(f"Did not find a handler for {type(node)}")
    
    # post-process - if there's anything in the parameter stack, look at the variables,
    # convert them to parameters and add the initial value
    if parameter_stack:
        process_parameter_statements(module_data, parameter_stack)
    if remove_private and access_stack:
        process_access_statements(module_data, access_stack)
    if bind_stack:
        process_bind_statements(module_data, bind_stack)
    if data_statement_stack:
        process_data_statements(module_data, data_statement_stack)
    if save_all or save_variables or save_common_blocks:
        process_save_statements(module_data, save_variables, save_common_blocks, save_all)
    # common block variables and their type declarations need to be matched
    process_common_block_variables(module_data)

def process_parameter_statements(module_data: ModuleDescription, parameter_statements: List[Parameter_Stmt]) -> None:
    for param_stmt in parameter_statements:
        named_def_constants = walk(param_stmt, Named_Constant_Def)
        for named_def_constant in named_def_constants:
            name, value = named_def_constant.children
            # Check if this variable exists
            if name.string in module_data["variables"]:
                # Move from variables to parameters
                variable = module_data["variables"].pop(name.string)
                
                # Create parameter description from variable description
                parameter: ParameterDescription = {
                    "description": variable["description"],
                    "type": variable["type"],
                    "name": variable["name"],
                    "value": value.string,
                    "dimension": variable.get("dimension"),
                    "polymorphism_type": variable.get("polymorphism_type"),
                    "attributes": variable.get("attributes"),
                    "kind": variable.get("kind"),
                    "length": variable.get("length"),
                    "binding_type": variable.get("binding_type")
                }
                
                module_data["parameters"][name.string] = parameter
            else:
                logger.warning(f"PARAMETER statement for '{name.string}' but no corresponding variable declaration found")

def process_common_block_variables(module_data: ModuleDescription) -> None:
    common_blocks = module_data["common_blocks"]
    variables = module_data["variables"]
    for _, common_block_description in common_blocks.items():
        common_block_variables = common_block_description["variables"]
        for common_block_variable_name, common_block_variable in common_block_variables.items():
            if common_block_variable_name in variables:
                variable_description = variables[common_block_variable_name]
                # see if the dimension was defined in the common block rather than the type declaration
                if not variable_description["dimension"] and common_block_variable["dimension"]:
                    variable_description["dimension"] = common_block_variable["dimension"]
                # replace the variable description in the common block with the full description
                common_block_variables[common_block_variable_name] = variables[common_block_variable_name]

#TODO remove private entities if they shouldn't be documented
#TODO in types, modules, etc: look for a top-level 'private' or 'public' attribute
# Then walk the data structure and look for any child with attributes that 
# have a 'private' attribute OR don't have any access attribute and are by 
# default private because of the top-level attribute. Then, remove if it's private
def process_access_statements(module_data: ModuleDescription, access_stack: Dict[str, str]) -> None:
    def add_access_to_entity(entity: Dict[str, Any], access: str) -> None:
        attrs = entity.get("attributes", [])
        # have overridden default access, so remove then add 
        if access == "PUBLIC" and "PRIVATE" in attrs:
            attrs.remove("PRIVATE")
        elif access == "PRIVATE" and "PUBLIC" in attrs:
            attrs.remove("PUBLIC")
        if access not in attrs:
            entity["attributes"] = attrs + [access]
    
    for _, field_value in module_data.items():
        # Skip non-collection fields
        temp = type(field_value)
        if not isinstance(field_value, (dict, list)):
            continue

        if isinstance(field_value, dict):
            # Process dict-based collections
            for name, access in access_stack.items():
                if name in field_value:
                    entity = field_value[name]
                    # Check if this entity has an attributes field
                    if isinstance(entity, dict) and "attributes" in entity:
                        add_access_to_entity(entity, access)
                        
        elif isinstance(field_value, list):
            # Process list-based collections (like interfaces)
            for entity in field_value:
                if isinstance(entity, dict) and "attributes" in entity:
                    # Need to determine the entity's name
                    name = entity.get("name") or entity.get("interface_name") or entity.get("type_name")
                    if name and name in access_stack:
                        add_access_to_entity(entity, access_stack[name])
                        

def process_bind_statements(module_data: ModuleDescription, bind_stack: Dict[str, Optional[BindingType]]) -> None:
    """Process BIND statements and attach them to the appropriate entities."""
    
    # Check common blocks
    for common_name, common_block in module_data.get("common_blocks", {}).items():
        if common_name in bind_stack:
            common_block["binding_type"] = bind_stack[common_name]
    
    # Check procedures
    for func_name, func_desc in module_data.get("functions", {}).items():
        if func_name in bind_stack:
            func_desc["binding_type"] = bind_stack[func_name]
    
    for sub_name, sub_desc in module_data.get("subroutines", {}).items():
        if sub_name in bind_stack:
            sub_desc["binding_type"] = bind_stack[sub_name]
    
    # Check variables
    for var_name, var_desc in module_data.get("variables", {}).items():
        if var_name in bind_stack:
            var_desc["binding_type"] = bind_stack[var_name]
    
    # Check derived types
    for type_name, type_desc in module_data.get("types", {}).items():
        if type_name in bind_stack:
            type_desc["binding_type"] = bind_stack[type_name]
        
    #TODO test this
    # Check enumerations
    for enum_name, enum_desc in module_data.get("enums", {}).items():
        if enum_name in bind_stack:
            enum_desc["binding_type"] = bind_stack[enum_name]

    #TODO
    # # Check interfaces 
    # for interface_name, interface_desc in module_data.get("interfaces", {}).items():
    #     if interface_name in bind_stack:
    #         interface_desc["binding_type"] = bind_stack[interface_name]    

    # Log any unmatched bind statements (similar to parameter processing)
    matched_entities = set()
    for collection in ["common_blocks", "functions", "subroutines", "variables", "types", "enums"]:
        matched_entities.update(module_data.get(collection, {}).keys())
    
    #TODO
    # # Handle interfaces separately since they're stored as a list
    # interfaces = module_data.get("interfaces", [])
    # for interface in interfaces:
    #     if isinstance(interface, dict) and "name" in interface:
    #         matched_entities.add(interface["name"])
 
    for bind_target in bind_stack:
        if bind_target not in matched_entities:
            logger.warning(f"BIND statement for '{bind_target}' but no matching entity found")

def process_data_statements(module_data: ModuleDescription, data_statement_stack: List[Data_Stmt]) -> None:
    variable_initializations = {}
    
    update_func, get_info_func = create_module_adapters(module_data, variable_initializations)
    
    for data_stmt in data_statement_stack:
        process_data_statement_core(data_stmt, update_func, get_info_func)
    
    # Apply the collected initializations
    for var_name, value_list in variable_initializations.items():
        if var_name in module_data["variables"]:
            combined_value = ", ".join(value_list)
            module_data["variables"][var_name]["initial_value"] = combined_value
        else:
            logger.warning(f"DATA statement for variable '{var_name}' but variable not found")

def process_save_statements(module_data: ModuleDescription, save_variables: List[str], save_common_blocks: List[str], 
                           save_all: bool) -> None:    
    if save_all:
        # Mark all variables as saved
        for var_name, var_desc in module_data.get("variables", {}).items():
            var_desc["is_saved"] = True
    else:
        # Process explicit variable saves
        for var_name in save_variables:
            if var_name in module_data.get("variables", {}):
                module_data["variables"][var_name]["is_saved"] = True
            else:
                logger.warning(f"SAVE statement for variable '{var_name}' but variable not found")
        
        # Process common block saves (mark all variables in the common block as saved)
        for common_name in save_common_blocks:
            if common_name in module_data.get("common_blocks", {}):
                common_block = module_data["common_blocks"][common_name]
                # Mark all variables in this common block as saved
                for var_name in common_block["variables"]:
                    if var_name in module_data.get("variables", {}):
                        module_data["variables"][var_name]["is_saved"] = True
            else:
                logger.warning(f"SAVE statement for common block '/{common_name}/' but common block not found")