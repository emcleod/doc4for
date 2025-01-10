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

    # Handle character length if it's a character type
    length: Optional[str] = None
    if base_type == "character":
        length = extract_length_from_attributes(shared_attributes, declaration.selector)
        shared_attributes = [
            attr for attr in shared_attributes 
            if not attr.startswith("len=") 
            and not (attr.isdigit() or (attr[0] == '-' and attr[1:].isdigit()))
        ]
    
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

        variable_description: VariableDescription = {
            "description": description,
            "type": base_type,
            "name": name,
            "dimension": dimension,
            "attributes": shared_attributes.copy(),
            "kind": kind,
            "initial_value": initial_value,
            "length": length
        }

        variable_descriptions.append(variable_description)

    return variable_descriptions

def extract_length_from_attributes(attributes: List[str], selector: Optional[Tuple] = None) -> Optional[str]:
    """Extract length specification from character type attributes."""
    # First check if length is in selector (for character(10) form)
    if selector and isinstance(selector, tuple):
        star_value, _ = selector
        if star_value:
            return star_value
    
    # Then check attributes (for character(len=10) form)
    for attr in attributes:
        if attr.startswith("len="):
            return attr[4:]  # remove "len=" prefix
        elif attr.isdigit() or (attr[0] == '-' and attr[1:].isdigit()):
            # This handles the case where length is just a number
            return attr
    return None




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
