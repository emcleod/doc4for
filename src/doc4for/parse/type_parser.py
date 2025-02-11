import re
from typing import Any, List, Optional, Union, Tuple, Dict, Callable, Type

from fparser.one.block_statements import (
   Comment,
   SpecificBinding,
   FinalBinding,
   GenericBinding,
   ModuleProcedure
)
from fparser.one.typedecl_statements import TypeDeclarationStatement
from doc4for.models.dimension_models import Dimension_TEMP
from doc4for.models.variable_models import DataComponent
from doc4for.models.type_models import TypeDescription, GenericInterface
from doc4for.models.procedure_models import ProcedureDescription
from doc4for.utils.comment_utils import is_doc4for_comment, format_comments

# Compile regex patterns
NAME_VALUE_PATTERN = re.compile(r'(\w+)\s*(?:=\s*(.+))?$')
DIMENSION_PATTERN = re.compile(r'\((.*?)\)$')
KIND_PATTERN = re.compile(r'\(kind\s*=\s*(\w+)\)')
LEN_PATTERN = re.compile(r'\blen\s*=\s*(\d+|:)')

# Type aliases
HandlerType = Callable[[Any, TypeDescription, str], None]


class TypeHandler:
   """A class that maps types of Fortran statements to functions that will populate a TypeDescription.

   This class maintains a registry of handler functions for different types
   of Fortran statements and provides methods to register and retrieve these handlers.
   """

   def __init__(self):
       """Initialize the TypeHandler with an empty handlers dictionary."""
       self.handlers: Dict[Type, HandlerType] = {}

   def register_handler(self, item_type: Type, handler: HandlerType) -> None:
       """Register a handler function for a specific item type.

       Args:
           item_type: The type of item to be handled.
           handler: The handler function. Should populate a data structure 
                   and add the description that is passed in.
       """
       self.handlers[item_type] = handler

   def get_handler(self, item_type: Type) -> HandlerType:
       """Get the appropriate handler function for a given item type.

       Args:
           item_type: The type of item to get a handler for.

       Returns:
           A handler function that can process the given item type.
       """
       for handler_type, handler in self.handlers.items():
           if issubclass(item_type, handler_type):
               return handler
       return handle_other_type


_type_handler_instance: Optional[TypeHandler] = None


def _get_type_handler() -> TypeHandler:
   """Get an instance of TypeHandler and initialize if necessary.

   Returns:
       The singleton instance of TypeHandler.
   """
   global _type_handler_instance
   if _type_handler_instance is None:
       handler = TypeHandler()
       handler.register_handler(SpecificBinding, handle_specific_binding)
       handler.register_handler(ModuleProcedure, handle_module_procedure)
       handler.register_handler(GenericBinding, handle_generic_binding)
       handler.register_handler(FinalBinding, handle_final_binding)
       handler.register_handler(TypeDeclarationStatement, handle_type_declaration_statement)
       _type_handler_instance = handler
   return _type_handler_instance


def update_type_with_parsed_data(fortran_type: Any, type_info: TypeDescription) -> None:
   """Update the type_info dictionary with parsed data from the Fortran type.

   Args:
       fortran_type: The Fortran type object to parse.
       type_info: The dictionary to update with parsed information.
   """
   type_info['type_name'] = fortran_type.name
   comment_stack: List[Comment] = []
   handlers = _get_type_handler()
   
   for item in fortran_type.content:
       if isinstance(item, Comment):
           comment_stack.append(item)
       else:
           handler = handlers.get_handler(type(item))
           description = get_formatted_description(comment_stack)
           handler(item, type_info, description)
           comment_stack.clear()


def handle_specific_binding(item: SpecificBinding, type_info: TypeDescription, description: str) -> None:
   """Handle a SpecificBinding.

   Args:
       item: The specific binding item to handle.
       type_info: The type description dictionary to update.
       description: The description of this procedure.
   """
   procedure_description: ProcedureDescription = {
       'name': item.name,
       'description': description,
       'attributes': [attr.lower() for attr in item.attrs],
       'is_final': False,
       'bound_to': item.bname
   }
   type_info['procedures'][item.name] = procedure_description


def handle_module_procedure(item: ModuleProcedure, type_info: TypeDescription, description: str) -> None:
   """Handle a ModuleProcedure.

   Args:
       item: The module procedure item to handle.
       type_info: The type description dictionary to update.
       description: The description of this procedure.
   """
   # TODO: attributes are missing
   for name in item.items:
       procedure_description: ProcedureDescription = {
           'name': name,
           'description': description,
           'attributes': [],
           'is_final': False,
           'bound_to': None
       }
       type_info['procedures'][name] = procedure_description


def handle_generic_binding(item: GenericBinding, type_info: TypeDescription, description: str) -> None:
   """Handle a GenericBinding.

   Args:
       item: The generic binding item to handle.
       type_info: The type description dictionary to update.
       description: The description of this procedure.
   """
   generic_description: GenericInterface = {
       'generic_spec': item.spec,
       'attributes': [item.aspec.lower()],
       'specific_procedures': item.items,
       'description': description,
   }
   type_info['generic_interfaces'][item.spec] = generic_description


def handle_final_binding(item: FinalBinding, type_info: TypeDescription, description: str) -> None:
   """Handle a FinalBinding.

   Args:
       item: The final binding item to handle.
       type_info: The type description dictionary to update.
       description: The description of this procedure.
   """
   for final_name in item.items:
       procedure_description: ProcedureDescription = {
           'name': final_name,
           'description': description,
           'attributes': ['final'],
           'is_final': True,
           'bound_to': None
       }
       type_info['procedures'][final_name] = procedure_description


def handle_type_declaration_statement(item: TypeDeclarationStatement, type_info: TypeDescription, description: str) -> None:
   """Handle a TypeDeclarationStatement (e.g. Real, Integer, Character).

   Args:
       item: The type declaration statement to handle.
       type_info: The type description dictionary to update.
       description: The description of this procedure.
   """
   for entity_decl in item.entity_decls:
       component = create_data_component(item, entity_decl, description)
       type_info['data_components'][component['name']] = component


def create_data_component(item: TypeDeclarationStatement, entity_decl: str, description: str) -> DataComponent:
   """Create a data component description from a type declaration and entity.

   Args:
       item: The type declaration statement.
       entity_decl: The entity declaration string.
       description: The description of this component.

   Returns:
       A dictionary containing the data component description.
   """
   name, initial_value = get_name_and_initial_value(entity_decl)
   return {
       'name': name,
       'type': item.name,
       'kind': extract_kind(item.raw_selector),
       'len': extract_len(item.raw_selector),
       'description': description,
       'dimension': extract_dimension(entity_decl, item.attrspec),
       'initial_value': initial_value,
       'attributes': [attr.lower() for attr in item.attrspec if 'dimension' not in attr]
   }


def handle_other_type(item: Any, type_info: TypeDescription, description: str) -> None:
   """Handle any other type of item by doing nothing.

   Args:
       item: The item to handle.
       type_info: The type description dictionary.
       description: The description string.
   """
   pass


def get_formatted_description(comment_stack: List[Comment]) -> str:
   """Get a formatted description from the comment stack.

   Args:
       comment_stack: The stack of comments to format.

   Returns:
       The formatted description or an empty string if not a doc4for comment.
   """
   return format_comments(comment_stack) if is_doc4for_comment(comment_stack) else ''


def get_name_and_initial_value(entity_decl: str) -> Tuple[str, Optional[str]]:
   """Extract the name and initial value from an entity declaration.

   Args:
       entity_decl: The entity declaration string.

   Returns:
       A tuple containing the name and initial value (if any).
   
   """
#    TODO: Error handling - Handle regex match failures
   match = NAME_VALUE_PATTERN.search(entity_decl)
   if match:
       name: str = match.group(1)
       value: Optional[str] = match.group(2)
       return name.strip(), value.strip() if value else None
   words: List[str] = entity_decl.split()
   if ':' in words[-1]:
       return re.sub(r'\s*\(\s*(:(?:\s*,\s*:)*)\s*\)\s*$', '', words[-1]), None
   return words[-1].strip(), None


def extract_dimension(entity_decl: str, attributes: List[str]) -> Optional[Dimension_TEMP]:
   """Extract dimension information from an entity declaration and attributes.

   Args:
       entity_decl: The entity declaration string.
       attributes: The list of attributes.

   Returns:
       The extracted dimension information or None if not found.
   
   """
#    TODO: Error handling - Handle malformed dimension specifications
   if ':' in entity_decl:
       return dimension_from_declaration(entity_decl)
   for attr in attributes:
       if attr.startswith('dimension(') and attr.endswith(')'):
           dim_str = attr[len('dimension('):-1]
           dimensions = parse_dimension_string(dim_str)
           if dimensions is not None:
               return {'dimensions': dimensions}
   return None


def dimension_from_declaration(s: str) -> Optional[Dimension_TEMP]:
   """Extract dimension information from a declaration string.

   Args:
       s: The declaration string.

   Returns:
       The extracted dimension information or None if not found.
   
   """
#   TODO: Error handling - Handle malformed declaration strings
   match = DIMENSION_PATTERN.search(s)
   if match:
       dimensions: List[str] = match.group(1).split(',')
       return {'dimensions': [d.strip() if d.strip() != ':' else ':' for d in dimensions]}
   return None


def parse_dimension_string(dim_str: str) -> Optional[List[Union[int, str]]]:
   """Parse a dimension string into a list of dimensions.

   The string can be numbers or allocatable (':'). 

   Args:
       dim_str: The dimension string to parse.

   Returns:
       A list of parsed dimensions or None if invalid.
   
   """
#    TODO: Error handling - Handle malformed dimension strings
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
   """Extract the 'kind' value from a declaration.

   For example, integer(kind=int64), public :: big_int has a kind of int64.

   Args:
       type_spec: The type specification string.

   Returns:
       The extracted 'kind' value or None if not found.
   
   """
#    TODO: Replace with implementation in populate_data_models
#    TODO: Error handling - Handle malformed kind specifications
   match = KIND_PATTERN.search(type_spec)
   if match:
       return match.group(1)
   return None


def extract_len(type_spec: str) -> Optional[str]:
   """Extract the 'len' value from a declaration.

   'len' is used to declare the length of a Character array e.g. 
   character(len=20), public :: string_name
   'len' can be a number or ':' (allocatable).

   Args:
       type_spec: The type specification string.

   Returns:
       The extracted 'len' value or None if not found.
   
   """
#    TODO: Error handling - Handle malformed len specifications
   match = LEN_PATTERN.search(type_spec)
   if match:
       return match.group(1)
   return None