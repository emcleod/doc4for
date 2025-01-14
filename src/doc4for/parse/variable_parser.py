import re
from typing import List, Optional, Tuple
from fparser.one.typedecl_statements import TypeDeclarationStatement
from doc4for.models.variable_models import VariableDescription
from doc4for.parse.dimension_parser import (
    extract_dimension_from_attributes,
    extract_variable_dimension
)

# TODO with implied shape arrays, find the number of elements 
# in the array so the dimensions can be filled in
# TODO with defined type arrays, link to their type when generating
# the documents
def parse_variables(
    declaration: TypeDeclarationStatement,
    description: str,
    shared_attributes: List[str],
) -> List[VariableDescription]:
    
    base_type = str(declaration.name).lower()
    kind = extract_kind(declaration)

    # Extract dimension from attributes
    dimension_from_attr = extract_dimension_from_attributes(shared_attributes)
    
    # Remove 'dimension' from shared_attributes if present
    shared_attributes = [
        attr for attr in shared_attributes 
        if not attr.startswith("dimension(")
    ]
    
    variable_descriptions: List[VariableDescription] = []
    
    for entity in declaration.entity_decls:
        if "=" in entity:
            full_name, initial_value = [x.strip() for x in entity.split("=", 1)]
        else:
            full_name = entity.strip()
            initial_value = None

        # Try to get dimensions from name first
        dimension = extract_variable_dimension(full_name)
        
        # Use dimension from attributes if none found in name
        if not dimension and dimension_from_attr:
            dimension = dimension_from_attr

        # Remove any parenthetical expressions from the name
        name = re.sub(r"\(.*\)", "", full_name).strip()

        # Handle allocatable arrays
        if "allocatable" in shared_attributes and "(:)" in name:
            name = name.replace("(:)", "")

        # Handle character length if it's a character type
        length = get_character_length(
            base_type,
            shared_attributes, 
            declaration.selector,
            initial_value
        )
        if base_type == "character":
            # Filter out length-related attributes
            working_attributes = [
                attr for attr in shared_attributes 
                if not attr.startswith("len=") 
                and not (attr.isdigit() or (attr[0] == '-' and attr[1:].isdigit()))
            ]
        else:
            working_attributes = shared_attributes

        variable_description: VariableDescription = {
            "description": description,
            "type": base_type,
            "name": name,
            "dimension": dimension,
            "attributes": working_attributes.copy(),
            "kind": kind,
            "initial_value": initial_value,
            "length": length
        }

        variable_descriptions.append(variable_description)

    return variable_descriptions

def get_character_length(base_type: str, attributes: List[str], selector: Optional[Tuple] = None,
                         initial_value: Optional[str] = None) -> Optional[str]:
    """
    Determine character length from various possible sources:
    1. Explicit length specification in selector
    2. Length specification in attributes
    3. Initial value string length
    """
    # If it's not a character, return immediately
    if base_type != "character":
        return None
    
    # First check if length is in selector
    if selector and isinstance(selector, tuple):
        len_spec, old_style_len = selector
        if len_spec and 'selected_char_kind' in len_spec:
            pass # this is a kind spec, not a length spec
        elif len_spec == '*':  # assumed-length character: character(len=*)
            return '*'
        elif len_spec:  # new style: character(len=10)
            return len_spec
        if old_style_len:  # old style: character*20
            return old_style_len
                
    # Then check attributes (for character(len=10) form)
    for attr in attributes:
        if attr.startswith("len="):
            return attr[4:]  # remove "len=" prefix
        elif attr.isdigit() or (attr[0] == '-' and attr[1:].isdigit()):
            # This handles the case where length is just a number
            return attr
            
    # Finally, if we have an initial value, calculate length from that
    if initial_value and initial_value.startswith("'") and initial_value.endswith("'"):
        string_content = initial_value[1:-1]
        return str(len(string_content))
    
    return "1"

def extract_kind(declaration: TypeDeclarationStatement) -> Optional[str]:
    """Extract kind specification from a type declaration.

    Args:
        declaration: The type declaration statement

    Returns:
        Optional[str]: The kind specification if present, None otherwise
    """
    if hasattr(declaration, "selector") and isinstance(declaration.selector, Tuple):
        star_value, kind_value = declaration.selector
        return star_value or kind_value or None
    return None
