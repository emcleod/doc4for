import re
from functools import lru_cache
from typing import Any, List, Optional, Union, Tuple, Dict, Callable, Type
from fparser.one.block_statements import (
    Comment,
    SpecificBinding,
    FinalBinding,
    GenericBinding,
    ModuleProcedure
)
from fparser.one.typedecl_statements import TypeDeclarationStatement
from doc4for.data_models import (
    TypeDescription,
    ProcedureDescription,
    GenericInterface,
    DataComponent,
    Dimension
)
from doc4for.comment_utils import is_doc4for_comment, format_comments

class TypeHandler:
    """
    A class that maps types of Fortran statements to functions that will populate
    a TypeDescription.

    This class maintains a registry of handler functions for different types
    of Fortran statements and provides methods to register and retrieve these handlers.
    """

    def __init__(self):
        """
        Initialize the TypeHandler with an empty handlers dictionary.
        """
        self.handlers: Dict[Type, Callable[[Any, TypeDescription, List[Comment]], None]] = {}

    def register_handler(self, item_type: Type, handler: Callable[[Any, TypeDescription, List[Comment]], None]):
        """
        Register a handler function for a specific item type.

        Args:
            item_type (Type): The type of item to be handled.
            handler (Callable[[Any, TypeDescription, List[Comment]], None]): The handler function.
        """
        self.handlers[item_type] = handler

    @lru_cache(maxsize=None)
    def get_handler(self, item_type: Type) -> Callable[[Any, TypeDescription, List[Comment]], None]:
        """
        Get the appropriate handler function for a given item type.

        Args:
            item_type (Type): The type of item to get a handler for.

        Returns:
            Callable
        """
        for handler_type, handler in self.handlers.items():
            if issubclass(item_type, handler_type):
                return handler
        return handle_other_type

type_handler = TypeHandler()

def update_type_with_parsed_data(fortran_type: Any, type_info: TypeDescription) -> None:
    """
    Update the type_info dictionary with parsed data from the Fortran type.

    Args:
        fortran_type (Any): The Fortran type object to parse.
        type_info (TypeDescription): The dictionary to update with parsed information.
    """
    type_info['type_name'] = fortran_type.name
    comment_stack: List[Comment] = []
    for item in fortran_type.content:
        handler = type_handler.get_handler(type(item))
        handler(item, type_info, comment_stack)

def handle_comment(item: Comment, type_info: TypeDescription, comment_stack: List[Comment]) -> None:
    """
    Handle a Comment.

    Args:
        item (Comment): The comment item to handle.
        type_info (TypeDescription): The type description dictionary (unused in this function).
        comment_stack (List[Comment]): The stack of comments to update.
    """
    comment_stack.append(item)

def handle_specific_binding(item: SpecificBinding, type_info: TypeDescription, comment_stack: List[Comment]) -> None:
    """
    Handle a SpecificBinding.

    Args:
        item (SpecificBinding): The specific binding item to handle.
        type_info (TypeDescription): The type description dictionary to update.
        comment_stack (List[Comment]): The stack of comments to use for description.
    """
    procedure_description: ProcedureDescription = {
        'name': item.name,
        'description': get_formatted_description(comment_stack),
        'attributes': [attr.lower() for attr in item.attrs],
        'is_final': False,
    }
    type_info['procedures'][item.name] = procedure_description
    comment_stack.clear()

def handle_module_procedure(item: ModuleProcedure, type_info: TypeDescription, comment_stack: List[Comment]) -> None:
    """
    Handle a ModuleProcedure.

    Args:
        item (ModuleProcedure): The module procedure item to handle.
        type_info (TypeDescription): The type description dictionary to update.
        comment_stack (List[Comment]): The stack of comments to use for description.
    """
    for name in item.items:
        procedure_description: ProcedureDescription = {
            'name': name,
            'description': get_formatted_description(comment_stack),
            'attributes': [], #TODO
            'is_final': False
        }
        type_info['procedures'][name] = procedure_description
    comment_stack.clear()

def handle_generic_binding(item: GenericBinding, type_info: TypeDescription, comment_stack: List[Comment]) -> None:
    """
    Handle a GenericBinding.

    Args:
        item (GenericBinding): The generic binding item to handle.
        type_info (TypeDescription): The type description dictionary to update.
        comment_stack (List[Comment]): The stack of comments to use for description.
    """
    generic_description: GenericInterface = {
        'generic_spec': item.spec,
        'attributes': [item.aspec.lower()],
        'specific_procedures': item.items,
        'description': get_formatted_description(comment_stack),
    }
    type_info['generic_interfaces'][item.spec] = generic_description
    comment_stack.clear()

def handle_final_binding(item: FinalBinding, type_info: TypeDescription, comment_stack: List[Comment]) -> None:
    """
    Handle a FinalBinding.

    Args:
        item (FinalBinding): The final binding item to handle.
        type_info (TypeDescription): The type description dictionary to update.
        comment_stack (List[Comment]): The stack of comments to use for description.
    """
    for final_name in item.items:
        procedure_description: ProcedureDescription = {
            'name': final_name,
            'description': get_formatted_description(comment_stack),
            'attributes': ['final'],
            'is_final': True,
        }
    type_info['procedures'][final_name] = procedure_description
    comment_stack.clear()

def handle_type_declaration_statement(item: TypeDeclarationStatement, type_info: TypeDescription, comment_stack: List[Comment]) -> None:
    """
    Handle a TypeDeclarationStatement (e.g. Real, Integer, Character).

    Args:
        item (TypeDeclarationStatement): The type declaration statement to handle.
        type_info (TypeDescription): The type description dictionary to update.
        comment_stack (List[Comment]): The stack of comments to use for description.
    """
    for entity_decl in item.entity_decls:
        name, initial_value = get_name_and_initial_value(entity_decl)
        data_component_description: DataComponent = {
            'name': name,
            'type': item.name,
            'kind': extract_kind(item.raw_selector),
            'len': extract_len(item.raw_selector),
            'description': get_formatted_description(comment_stack),
            'dimension': extract_dimension(entity_decl, item.attrspec),
            'initial_value': initial_value,
            'attributes': [attr.lower() for attr in item.attrspec if 'dimension' not in attr]
        }
        type_info['data_components'][name] = data_component_description
    comment_stack.clear()

def handle_other_type(item: Any, type_info: TypeDescription, comment_stack: List[Comment]) -> None:
    """
    Handle any other type of item by clearing the comment stack.

    Args:
        item (Any): The item to handle (unused in this function).
        type_info (TypeDescription): The type description dictionary (unused in this function).
        comment_stack (List[Comment]): The stack of comments to clear.
    """
    comment_stack.clear()

# Register handlers
type_handler.register_handler(Comment, handle_comment)
type_handler.register_handler(SpecificBinding, handle_specific_binding)
type_handler.register_handler(ModuleProcedure, handle_module_procedure)
type_handler.register_handler(GenericBinding, handle_generic_binding)
type_handler.register_handler(FinalBinding, handle_final_binding)
type_handler.register_handler(TypeDeclarationStatement, handle_type_declaration_statement)

def get_formatted_description(comment_stack: List[Comment]) -> str:
    """
    Get a formatted description from the comment stack.

    Args:
        comment_stack (List[Comment]): The stack of comments to format.

    Returns:
        str: The formatted description or an empty string if not a doc4for comment.
    """
    return format_comments(comment_stack) if is_doc4for_comment(comment_stack) else ''

def get_name_and_initial_value(entity_decl: str) -> Tuple[str, Optional[str]]:
    """
    Extract the name and initial value from an entity declaration.

    Args:
        entity_decl (str): The entity declaration string.

    Returns:
        Tuple[str, Optional[str]]: A tuple containing the name and initial value (if any).
    """
    match: Optional[re.Match] = re.search(r'(\w+)\s*(?:=\s*(.+))?$', entity_decl)
    if match:
        name: str = match.group(1)
        value: Optional[str] = match.group(2)
        return name.strip(), value.strip() if value else None
    words: List[str] = entity_decl.split()
    if ':' in words[-1]:
        return re.sub(r'\s*\(\s*(:(?:\s*,\s*:)*)\s*\)\s*$', '', words[-1]), None
    return words[-1].strip(), None

def extract_dimension(entity_decl: str, attributes: List[str]) -> Optional[Dimension]:
    """
    Extract dimension information from an entity declaration and attributes.

    Args:
        entity_decl (str): The entity declaration string.
        attributes (List[str]): The list of attributes.

    Returns:
        Optional[Dimension]: The extracted dimension information or None if not found.
    """
    if ':' in entity_decl:
        return dimension_from_declaration(entity_decl)
    for attr in attributes:
        if attr.startswith('dimension(') and attr.endswith(')'):
            dim_str = attr[len('dimension('):-1]
            dimensions = parse_dimension_string(dim_str)
            if dimensions is not None:
                return {'dimensions': dimensions}
    return None

def dimension_from_declaration(s: str) -> Optional[Dimension]:
    """
    Extract dimension information from a declaration string.

    Args:
        s (str): The declaration string.

    Returns:
        Optional[Dimension]: The extracted dimension information or None if not found.
    """
    match: Optional[re.Match] = re.search(r'\((.*?)\)$', s)
    if match:
        dimensions: List[str] = match.group(1).split(',')
        return {'dimensions': [d.strip() if d.strip() != ':' else ':' for d in dimensions]}
    return None

def parse_dimension_string(dim_str: str) -> Optional[List[Union[int, str]]]:
    """
    Parse a dimension string into a list of dimensions. The string can be numbers 
    or allocatable (':'). 

    Args:
        dim_str (str): The dimension string to parse.

    Returns:
        Optional[List[Union[int, str]]]: A list of parsed dimensions or None if invalid.
    """
    dimensions: List[Union[int, str]] = []
    for dim in dim_str.split(','):
        dim = dim.strip()
        if dim.isdigit():
            dimensions.append(int(dim))
        elif dim == ':':
            dimensions.append(dim)
        else:
            return None
    return dimensions

def extract_kind(type_spec: str) -> Optional[str]:
    """
    Extract the 'kind' value from a declaration, if it is present. For example,
        integer(kind=int64), public :: big_int
    has a kind of int64.
    Args:
        type_spec (str): The type specification string.

    Returns:
        Optional[str]: The extracted 'kind' value or None if not found.
    """
    match: Optional[re.Match] = re.search(r'\(kind\s*=\s*(\w+)\)', type_spec)
    if match:
        return match.group(1)
    return None

def extract_len(type_spec: str) -> Optional[str]:
    """
    Extract the 'len' value from a declaration, if it is present. 'len' is
    used to declare the length of a Character array e.g. 
        character(len=20), public :: string_name
    'len' can be a number or ':' (allocatable).

    Args:
        type_spec (str): The type specification string.

    Returns:
        Optional[str]: The extracted 'len' value or None if not found.
    """
    match: Optional[re.Match] = re.search(r'\blen\s*=\s*(\d+|:)', type_spec)
    if match:
        return match.group(1)
    return None