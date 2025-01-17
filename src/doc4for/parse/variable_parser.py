import re
from typing import List, Optional, Tuple
from fparser.one.typedecl_statements import TypeDeclarationStatement
from fparser.one.block_statements import Comment
from fparser.one.typedecl_statements import TypeDeclarationStatement
from doc4for.models.variable_models import VariableDescription
from doc4for.parse.dimension_parser import extract_dimension_from_attributes, extract_variable_dimension
from doc4for.parse.array_utils import parse_initialization_value
from doc4for.utils.comment_utils import is_doc4for_comment, format_comments
from doc4for.parse.parsing_utils import get_attributes, extract_kind, get_character_length

# TODO with defined type arrays, link to their type when generating
# the documents
def parse_variable(
    declaration: TypeDeclarationStatement,
    comment_stack: List[Comment]
) -> List[VariableDescription]:
    """Parse variable declarations into variable descriptions.

    Args:
        declaration: The type declaration statement to parse
        comment_stack: Stack of comments preceding the declaration

    Returns:
        List of variable descriptions for both array and scalar variables
    """
    description = format_comments(
        comment_stack) if is_doc4for_comment(comment_stack) else ""
    shared_attributes = get_attributes(declaration)

    try:
        return parse_variables(declaration, description, shared_attributes)
    except Exception as e:
        # TODO log this and continue
        raise ValueError(
            f"Error parsing variable declaration: {str(e)}") from e
    
def parse_variables(
    declaration: TypeDeclarationStatement,
    description: str,
    shared_attributes: List[str],
) -> List[VariableDescription]:
    
    # Extract dimension from attributes
    dimension_from_attr = extract_dimension_from_attributes(shared_attributes)
    
    # Remove 'dimension' from shared_attributes if present
    shared_attributes = [
        attr for attr in shared_attributes 
        if not attr.startswith("dimension(")
    ]
    
    variable_descriptions: List[VariableDescription] = []

    base_type = str(declaration.name).lower()    
    for entity in declaration.entity_decls:
        kind = extract_kind(declaration)

        full_name, initial_value = parse_initialization_value(entity)

        # Try to get dimensions from name first
        dimension = extract_variable_dimension(full_name)
        
        # Remove any parenthetical expressions from the name
        name = re.sub(r"\(.*\)", "", full_name).strip()

        # Use dimension from attributes if none found in name
        if not dimension and dimension_from_attr:
            dimension = dimension_from_attr

        # Handle coarrays - fparser does not remove [*] from name
        if "[" in name:
            name = name.split(("["))[0]
        
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
                initial_value
            )

        variable_description: VariableDescription = {
            "description": description,
            "type": base_type,
            "name": name,
            "dimension": dimension,
            "attributes": working_attributes,
            "kind": kind,
            "initial_value": initial_value,
            "length": length
        }

        variable_descriptions.append(variable_description)

    return variable_descriptions
            
