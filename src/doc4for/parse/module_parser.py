import re
import logging
from dataclasses import dataclass, field
from typing import Callable, Any, Optional, Type, Dict, List, Tuple, Union
from fparser.one.block_statements import (
    Comment,
    Function, 
    Subroutine,
    Interface,
    Program,
    BlockData,
    ModuleProcedure,
    Type as FortranType
)
from fparser.one.typedecl_statements import TypeDeclarationStatement
from doc4for.models.module_models import ModuleDescription
from doc4for.models.type_models import TypeDescription
from doc4for.utils.comment_utils import get_formatted_description
from doc4for.parse.type_parser import update_type_with_parsed_data
from doc4for.parse.dimension_parser import extract_dimension_from_attributes, extract_variable_dimension, extract_coarray_dimensions
from doc4for.parse.array_utils import parse_initialization_value
from doc4for.utils.comment_utils import is_doc4for_comment, format_comments
from doc4for.parse.parsing_utils import get_attributes, extract_kind, get_character_length
from doc4for.parse.parameter_parser import parse_parameter, is_parameter
from doc4for.parse.procedure_parser import parse_subroutine, parse_function, parse_interface
from doc4for.parse.base_parser import (
    FortranHandler, 
    VisibilityState,
    handle_function, 
    handle_subroutine, 
    handle_type,
    handle_type_declaration,
    handle_interface
)
from doc4for.parse.base_parser import FortranHandler, handle_function, handle_subroutine

logger: logging.Logger = logging.getLogger(__name__)

ModuleHandlerType = ModuleHandler = FortranHandler[ModuleDescription]

@dataclass
class VisibilityState:
    is_public: bool = True
    explicit_public: List[str] = field(default_factory=list)
    explicit_private: List[str] = field(default_factory=list)


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

