import logging
from typing import Any, Optional, List
from fparser.one.block_statements import (
    Comment,
    Module,
    Function,
    Subroutine,
    Program,
    BlockData,
)
from doc4for.models.file_models import FileDescription
from doc4for.utils.comment_utils import is_doc4for_comment, format_comments, is_end_of_doc4for_comment
from doc4for.parse.base_parser import ( 
    FortranHandler,
    VisibilityState,
    handle_function, 
    handle_subroutine, 
    handle_block_data, 
    handle_module, 
    handle_program)

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
        handler.register_handler(Function, handle_function)
        handler.register_handler(Subroutine, handle_subroutine)
        handler.register_handler(Program, handle_program)
        handler.register_handler(BlockData, handle_block_data)
        _file_handler_instance = handler
    return _file_handler_instance


def parse_file_content(file: Any, file_data: FileDescription) -> None:
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
                        file_data['file_description'] = format_comments(
                            comment_stack)
                        file_description_set = True
                        comment_stack.clear()
        else:
            first_non_comment_node = False
            handler = handlers.get_handler(type(item))
            handler(item, file_data, comment_stack)
            comment_stack.clear()
