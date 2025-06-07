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
    Enum_Def
)
from doc4for.models.module_models import ModuleDescription
from doc4for.models.variable_models import ParameterDescription
from doc4for.parse.common_parser import FortranHandler
from doc4for.parse.base_parser import (    
    VisibilityState,
    handle_function,
    handle_subroutine,
    handle_type_declaration,
    handle_derived_type,
    handle_interface,
    handle_enum
    # handle_use,
    # handle_common_block,
#    handle_type
)
from doc4for.logging_config import setup_logging

logger: logging.Logger = logging.getLogger(__name__)

ModuleHandler = FortranHandler[ModuleDescription]

_module_handler_instance: Optional[ModuleHandler] = None

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
#        handler.register_handler(FortranType, handle_type)
        handler.register_handler(
            Type_Declaration_Stmt, handle_type_declaration)
        handler.register_handler(
            Derived_Type_Def, handle_derived_type)
        handler.register_handler(Interface_Block, handle_interface)
        handler.register_handler(Enum_Def, handle_enum)
        # handler.register_handler(Use, handle_use)
        # handler.register_handler(Common, handle_common_block)
        _module_handler_instance = handler
    return _module_handler_instance

def parse_module_content(module: Any, module_data: ModuleDescription, comment_stack: List[Comment]) -> None:
    handlers = _get_module_handler()
        
    # only get the parts we care about
    content = [child for child in module.children if isinstance(child, (Specification_Part, Module_Subprogram_Part))]
    if not content:
        return
    
    #TODO does this need to be moved inside the loop?
    # for older-style fortran, dimension and type declarations can be on different lines
    dimension_stack: List[Dimension_Stmt] = []
    # for older-style fortran, can declare something to be a parameter after the type declaration
    parameter_stack: List[Parameter_Stmt] = []
    # keep track of access statements before declaration entity_name -> access
    access_stack: Dict[str, str] = {}
    for module_nodes in content:
        for node in module_nodes.children:
            if isinstance(node, Implicit_Part):
                # we don"t have any object we"re going to parse, so collect up the comments
                for subnode in node.children:
                    if isinstance(subnode, Comment) and hasattr(subnode, "items") and subnode.items[0]: # it"s not empty                  
                        comment_stack.append(subnode)
                    elif isinstance(subnode, Parameter_Stmt):
                        parameter_stack.append(subnode)
            else:
                if isinstance(node, Dimension_Stmt):
                    dimension_stack.append(node)
                elif isinstance(node, Access_Stmt):
                    access_value, access_ids = node.children    
                    # If access_ids is None, it"s a global access statement like "private"
                    if access_ids:
                        access_stack.update({
                            name.string: access_value for name in walk(access_ids, Name)
                        })                                                
                else:
                    handler = handlers.get_handler(type(node))
                    if handler:
                        handler(node, module_data, comment_stack, dimension_stack=dimension_stack)
                        comment_stack.clear()
                    else:
                        logger.warning(f"Did not find a handler for {type(node)}")
        # post-process - if there"s anything in the parameter stack, look at the variables,
        # convert them to parameters and add the initial value
        if parameter_stack:
            process_parameter_statements(module_data, parameter_stack)
        if access_stack:
            process_access_statements(module_data, access_stack)

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
                    "name": variable["name"],
                    "type": variable["type"],
                    "description": variable["description"],
                    "value": value.string,
                    "dimension": variable.get("dimension"),
                    "kind": variable.get("kind"),
                    "length": variable.get("length"),
                }
                
                module_data["parameters"][name.string] = parameter
            else:
                logger.warning(f"PARAMETER statement for '{name.string}' but no corresponding variable declaration found")

# Types (derived types)
# Variables (including parameters)
# Procedures (functions and subroutines)
# Interfaces (abstract and explicit)
# Operators (defined operators)
# Generic names (covered by interfaces)
# Namelist groups
# Common blocks
# Generic interfaces/operators
# Module procedures
# Named constants (PARAMETER)
#TODO remove private entities if they shouldn't be documented
def process_access_statements(module_data: ModuleDescription, access_stack: Dict[str, str]) -> None:
    module_data["types"] = {
        name: {**desc, "attributes": desc.get("attributes", []) + [access_stack[name]]}
        for name, desc in module_data["types"].items()
        if name in access_stack
    }
