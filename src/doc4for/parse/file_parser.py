import logging
from typing import Any, Optional, List
from typing import TypeVar, List, Any
from fparser.two.Fortran2003 import (
  Comment,
  Module,
  Function_Subprogram,
  Subroutine_Subprogram,
  Main_Program,
  Block_Data,
  Equivalence_Stmt,
  Use_Stmt
)
from doc4for.models.file_models import FileDescription
from doc4for.utils.comment_utils import format_comments, is_end_of_doc4for_comment
from doc4for.parse.common_parser import FortranHandler
from doc4for.parse.base_parser import ( 
    VisibilityState,
    handle_function, 
    handle_subroutine, 
    handle_block_data, 
    handle_program,
    handle_equivalence,
    handle_use
    )
from doc4for.f90.populate_data_models import initialise_module_description
from doc4for.parse.module_parser import parse_module_content

logger: logging.Logger = logging.getLogger(__name__)

FileHandler = FortranHandler[FileDescription]

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
        handler.register_handler(Function_Subprogram, handle_function)
        handler.register_handler(Subroutine_Subprogram, handle_subroutine)
        handler.register_handler(Main_Program, handle_program)
        handler.register_handler(Block_Data, handle_block_data)
        handler.register_handler(Equivalence_Stmt, handle_equivalence)
        handler.register_handler(Use_Stmt, handle_use)
        _file_handler_instance = handler
    return _file_handler_instance


def parse_file_content(file: Any, file_data: FileDescription) -> None:
    handlers = _get_file_handler()
    comment_stack: List[Comment] = []
    default_access: Optional[str] = None # access is irrelevant outside modules
    first_non_comment_node: bool = False
    # TODO look at post processing in parse_module_content
    for child in file.children:
        if isinstance(child, Comment):
            comment_stack.append(child)
            if not file_data["file_description"] and  not first_non_comment_node and is_end_of_doc4for_comment(child):
                # found the end of the first doc4for comment, so assume it was supposed to be the 
                # file description
                file_data["file_description"] = format_comments(comment_stack)
                comment_stack.clear()
        else:
            first_non_comment_node = True
            handler = handlers.get_handler(type(child))
            handler(child, file_data, comment_stack, default_access)
            comment_stack.clear()

T = TypeVar("T")

def handle_module(item: Module, data: T, comment_stack: List[Comment], default_access: Optional[str], **kwargs: Any) -> None:
    module_description = initialise_module_description(item, comment_stack, data["file_name"])
    # TODO we're populating the module description twice
    parse_module_content(item, module_description, comment_stack) 
    data["modules"][module_description["module_name"]] = module_description
