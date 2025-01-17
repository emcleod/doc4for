import re
from typing import List, Optional, Tuple
from fparser.one.typedecl_statements import TypeDeclarationStatement
from fparser.one.block_statements import Comment
from fparser.one.typedecl_statements import TypeDeclarationStatement
from doc4for.models.variable_models import ParameterDescription
from doc4for.parse.dimension_parser import extract_dimension_from_attributes, extract_variable_dimension
from doc4for.parse.array_utils import parse_initialization_value
from doc4for.utils.comment_utils import is_doc4for_comment, format_comments
from doc4for.parse.parsing_utils import get_attributes, extract_kind, get_character_length

def parse_parameter(
    declaration: TypeDeclarationStatement,
    comment_stack: List[Comment]
) -> List[ParameterDescription]:
    """Parse parameter declarations into parameter descriptions.

    Args:
        declaration: The type declaration statement to parse
        comment_stack: Stack of comments preceding the declaration

    Returns:
        List of parameter descriptions for both array and scalar parameters
    """
    description = format_comments(
        comment_stack) if is_doc4for_comment(comment_stack) else ""
    shared_attributes = get_attributes(declaration)

    try:
        return parse_parameters(declaration, description, shared_attributes)
    except Exception as e:
        raise ValueError(
            f"Error parsing parameter declaration: {str(e)}") from e

def parse_parameters(
    declaration: TypeDeclarationStatement,
    description: str,
    shared_attributes: List[str],
) -> List[ParameterDescription]:
    
    base_type = str(declaration.name).lower()
    kind = extract_kind(declaration)

    # Extract dimension from attributes
    dimension_from_attr = extract_dimension_from_attributes(shared_attributes)
    
    # Remove 'dimension' from shared_attributes if present
    shared_attributes = [
        attr for attr in shared_attributes 
        if not attr.startswith("dimension(")
    ]
    
    parameter_descriptions: List[ParameterDescription] = []
    
    for entity in declaration.entity_decls:
        full_name, value = parse_initialization_value(entity)
        
        # Parameters must have an initialization value
        if value is None:
            raise ValueError(f"Parameter {full_name} must have an initialization value")

        # Try to get dimensions from name first
        dimension = extract_variable_dimension(full_name)
        
        # Remove any parenthetical expressions from the name
        name = re.sub(r"\(.*\)", "", full_name).strip()

        # Use dimension from attributes if none found in name
        if not dimension and dimension_from_attr:
            dimension = dimension_from_attr

        # Initialize character-specific attributes
        length = None
        working_attributes = shared_attributes
        if base_type == "character":
            # Filter out length-related attributes
            working_attributes = [
                attr for attr in shared_attributes 
                if not attr.startswith("len=") 
                and not (attr.isdigit() or (attr[0] == '-' and attr[1:].isdigit()))
            ]
            length = get_character_length(
                base_type,
                declaration,
                shared_attributes, 
                declaration.selector,
                value
            )

        parameter_description: ParameterDescription = {
            "description": description,
            "type": base_type,
            "name": name,
            "dimension": dimension,
            "attributes": working_attributes,
            "kind": kind,
            "value": value,  # Note: using 'value' instead of 'initial_value'
            "length": length
        }

        parameter_descriptions.append(parameter_description)

    return parameter_descriptions

def is_parameter(declaration: TypeDeclarationStatement) -> bool:
    """Determines if a TypeDeclarationStatement is a parameter declaration.

    Args:
        declaration: The TypeDeclarationStatement to check

    Returns:
        bool: True if the declaration has the parameter attribute
    """
    # Check if 'parameter' is in the attributes
    return (
        any(attr.lower() == "parameter" for attr in declaration.attrspec)
        if declaration.attrspec
        else False
    )
