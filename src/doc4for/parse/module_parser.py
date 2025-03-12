from typing import Any, Optional, List
from fparser.one.block_statements import (
    Comment,
    Function,
    Subroutine,
    Interface,
    Type as FortranType,
    Enum
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
    handle_interface,
    handle_enum
)

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
        handler.register_handler(
            TypeDeclarationStatement, handle_type_declaration)
        handler.register_handler(Interface, handle_interface)
        handler.register_handler(Enum, handle_enum)
        _module_handler_instance = handler
    return _module_handler_instance


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
