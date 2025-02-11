import logging
from dataclasses import dataclass, field
from typing import Callable, Any, Optional, Type, Dict, List
from fparser.one.block_statements import (
    Comment,
    Module,
    Function, 
    Subroutine,
    Program,
    BlockData,
)
from doc4for.models.file_models import FileDescription
from doc4for.parse.procedure_parser import parse_subroutine, parse_function, parse_interface
from doc4for.f90.populate_data_models import (
    parse_block_data,
    parse_program,
    initialise_module_description
)
from doc4for.utils.comment_utils import is_doc4for_comment, format_comments, is_end_of_doc4for_comment

logger: logging.Logger = logging.getLogger(__name__)

FileHandlerType = Callable[[Any, FileDescription, str, str], None]

@dataclass
class VisibilityState:
    is_public: bool = True
    explicit_public: List[str] = field(default_factory=list)
    explicit_private: List[str] = field(default_factory=list)

class FileHandler:
    """A class that maps types of Fortran file-level statements to functions that will populate
    a FileDescription.
    """

    def __init__(self):
        """Initialize the FileHandler with an empty handlers dictionary."""
        self.handlers: Dict[Type, FileHandlerType] = {}

    def register_handler(self, item_type: Type, handler: FileHandlerType) -> None:
        """Register a handler function for a specific item type.

        Args:
            item_type: The type of item to be handled.
            handler: The handler function that will populate the file description.
        """
        self.handlers[item_type] = handler

    def get_handler(self, item_type: Type) -> FileHandlerType:
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


_file_handler_instance: Optional[FileHandler] = None


def _get_file_handler() -> FileHandler:
    """Get an instance of FileHandler and initialize if necessary.

    Returns:
        The singleton instance of FileHandler.
    """
    global _file_handler_instance
    if _file_handler_instance is None:
        handler = FileHandler()
        handler.register_handler(Module, handle_module)
        handler.register_handler(Function, handle_function)
        handler.register_handler(Subroutine, handle_subroutine)
        handler.register_handler(Program, handle_program)
        handler.register_handler(BlockData, handle_block_data)
        _file_handler_instance = handler
    return _file_handler_instance

# TODO check that we actually need to pass in the comment stack. The comments
# should be those before the module starts i.e. its description. We might be able
# to add this to the module description after we've parsed the content
# def parse_file_content(file: Any, file_data: FileDescription, comment_stack: List[Comment], file_name: str) -> None:
#     handlers = _get_file_handler()
#     visibility: VisibilityState = VisibilityState()
#     for item in file.content:
#         if isinstance(item, Comment) and item.content:
#             comment_stack.append(item)
#         else:
#             handler = handlers.get_handler(type(item))
#             handler(item, file_data, comment_stack, file_name)
#             comment_stack.clear()

def parse_file_content(file: Any, file_data: FileDescription, file_name: str) -> None:
    handlers = _get_file_handler()
    visibility: VisibilityState = VisibilityState()
    comment_stack: List[Comment] = []
    first_non_comment_node: bool = True
    file_description_set: bool = False
    
    for item in file.content:
        if isinstance(item, Comment):
            if item.content:
                comment_stack.append(item)
                if is_end_of_doc4for_comment(item) and first_non_comment_node and not file_description_set:
                    if is_doc4for_comment(comment_stack):
                        file_data['file_description'] = format_comments(comment_stack)
                        file_description_set = True
                        comment_stack.clear()
        else:
            first_non_comment_node = False
            handler = handlers.get_handler(type(item))
            handler(item, file_data, comment_stack, file_name)
            comment_stack.clear()

def handle_other_module_item(item: Any, file_data: FileDescription, comment_stack: List[Comment], file_name: str) -> None:
   """Handle any other type of item by doing nothing.

   Args:
       item: The item to handle.
       type_info: The type description dictionary.
       description: The description string.
   """
   logger.warning("Unhandled type %s", type(item))
   pass

def handle_module(item: Module, file_data: FileDescription,
                   comment_stack: List[Comment], file_name: str):
    file_data['modules'][item.name] = initialise_module_description(item, comment_stack, file_name)

def handle_function(item: Function, file_data: FileDescription, 
                    comment_stack: List[Comment], file_name: str) -> None:
    file_data['functions'][item.name] = parse_function(item, comment_stack)


def handle_subroutine(item: Subroutine, file_data: FileDescription,
                      comment_stack: List[Comment], file_name: str) -> None:
    file_data['subroutines'][item.name] = parse_subroutine(item, comment_stack)

def handle_program(item: Program, file_data: FileDescription,
                   comment_stack: List[Comment], file_name: str) -> None:
    file_data['programs'][item.name] = parse_program(item, comment_stack, file_name)

def handle_block_data(item: BlockData, file_data: FileDescription,
                      comment_stack: List[Comment], file_name: str) -> None:
    file_data['block_data'][item.name] = parse_block_data(item, comment_stack)
