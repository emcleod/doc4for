import re
import logging
from dataclasses import dataclass, field
from typing import Any, Optional, List
from fparser.one.block_statements import (
    Comment,
    Function, 
    Subroutine,
    Interface,
    Type as FortranType
)
from fparser.one.typedecl_statements import TypeDeclarationStatement
from doc4for.models.module_models import ModuleDescription
from doc4for.parse.base_parser import (
    FortranHandler,
    VisibilityState,
    handle_function, 
    handle_subroutine, 
    handle_type,
    handle_type_declaration,
    handle_interface
)
from doc4for.parse.base_parser import handle_function, handle_subroutine

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
        handler.register_handler(Function, handle_function)
        handler.register_handler(Subroutine, handle_subroutine)
        handler.register_handler(FortranType, handle_type)
        handler.register_handler(TypeDeclarationStatement, handle_type_declaration)
        handler.register_handler(Interface, handle_interface)
        _module_handler_instance = handler
    return _module_handler_instance

# TODO check that we actually need to pass in the comment stack. The comments
# should be those before the module starts i.e. its description. We might be able
# to add this to the module description after we've parsed the content
#TODO look at how file parser handles initial comments - we don't do it correctly here
# #TODO recursive modules
# TODO
# use 
# common block
# namelist
# equivalence
# entry
# enum
def parse_module_content(module: Any, module_data: ModuleDescription, comment_stack: List[Comment]) -> None:
    handlers = _get_module_handler()
    visibility: VisibilityState = VisibilityState()
    for item in module.content:
        if isinstance(item, Comment) and item.content:
            comment_stack.append(item)
        else:
            handler = handlers.get_handler(type(item))
            handler(item, module_data, comment_stack)
            comment_stack.clear()

