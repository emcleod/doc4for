import re
import logging
from dataclasses import dataclass, field
from typing import Callable, Any, Optional, Type, Dict, List, Tuple, Union
from fparser.one.block_statements import (
    Comment,
    Module,
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
from doc4for.f90.populate_data_models import (
    initialise_module_description,
    parse_type, 
    parse_variable
)
logger: logging.Logger = logging.getLogger(__name__)

ModuleHandlerType = Callable[[Any, ModuleDescription, List[Comment]], None]

@dataclass
class VisibilityState:
    is_public: bool = True
    explicit_public: List[str] = field(default_factory=list)
    explicit_private: List[str] = field(default_factory=list)

class ModuleHandler:
    """A class that maps types of Fortran module-level statements to functions that will populate
    a ModuleDescription.
    """

    def __init__(self):
        """Initialize the ModuleHandler with an empty handlers dictionary."""
        self.handlers: Dict[Type, ModuleHandlerType] = {}

    def register_handler(self, item_type: Type, handler: ModuleHandlerType) -> None:
        """Register a handler function for a specific item type.

        Args:
            item_type: The type of item to be handled.
            handler: The handler function that will populate the module description.
        """
        self.handlers[item_type] = handler

    def get_handler(self, item_type: Type) -> ModuleHandlerType:
        """Get the appropriate handler function for a given item type.

        Args:
            item_type: The type of item to get a handler for.

        Returns:
            A handler function that can process the given item type.
        """
        for handler_type, handler in self.handlers.items():
            if issubclass(item_type, handler_type):
                return handler
        return handle_other_module_item


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

def handle_other_module_item(item: Any, module_data: ModuleDescription, comment_stack: List[Comment]) -> None:
   """Handle any other type of item by doing nothing.

   Args:
       item: The item to handle.
       type_info: The type description dictionary.
       description: The description string.
   """
   logger.warning("Unhandled type %s", type(item))
   pass

def handle_function(item: Function, module_data: ModuleDescription, 
                    comment_stack: List[Comment]) -> None:
    module_data['functions'][item.name] = parse_function(item, comment_stack)


def handle_subroutine(item: Subroutine, module_data: ModuleDescription,
                      comment_stack: List[Comment]) -> None:
    module_data['subroutines'][item.name] = parse_subroutine(item, comment_stack)

def handle_type(item: FortranType, module_data: ModuleDescription,
                comment_stack: List[Comment]) -> None:
    type_description: TypeDescription = parse_type(item, comment_stack)
    module_data['types'][type_description['type_name']] = type_description

def handle_type_declaration(item: TypeDeclarationStatement, module_data: ModuleDescription,
                            comment_stack: List[Comment]) -> None:
    if is_parameter(item):
        parameter_descriptions = parse_parameter(item, comment_stack)
        for param in parameter_descriptions:
            module_data['parameters'][param['name']] = param
    else:
        variable_descriptions = parse_variable(item, comment_stack)
        for var in variable_descriptions:
            module_data['variables'][var['name']] = var                

def handle_interface(item: Interface, module_data: ModuleDescription,
                     comment_stack: List[Comment]) -> None:
    module_data['interfaces'].append(parse_interface(item, comment_stack))