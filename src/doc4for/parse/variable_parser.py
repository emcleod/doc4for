import re
from typing import List
from fparser.one.typedecl_statements import TypeDeclarationStatement
from fparser.one.block_statements import Comment
from doc4for.models.variable_models import VariableDescription
from doc4for.models.common import BindingTypeEnum, BindingType
from doc4for.parse.dimension_parser import extract_dimension_from_attributes, extract_coarray_dimensions, extract_variable_dimension
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

    try:
        return parse_type_declaration_statement(declaration, description)
    except Exception as e:
        # TODO log this and continue
        raise ValueError(
            f"Error parsing variable declaration: {str(e)}") from e
    
def parse_type_declaration_statement(
    declaration: TypeDeclarationStatement,
    description: str
) -> List[VariableDescription]:
    
    shared_attributes = get_attributes(declaration)

    # Extract dimension from attributes
    dimension_from_attr = extract_dimension_from_attributes(shared_attributes)
    
    # Get binding type from attributes
    binding_type = extract_variable_binding(shared_attributes)

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

        # # Try to get dimensions from name first
        dimension = extract_variable_dimension(full_name)
                
        # Extract array and coarray specs before cleaning name
        is_array = re.search(r"\((.*?)\)", full_name)
        is_coarray = re.search(r"\[(.*?)\]", full_name)

        # Clean the name (remove both () and [] parts)
        name = re.split(r'[\(\[]', full_name)[0].strip()

        # Handle regular array dimensions
        if is_array:
            dimension = extract_variable_dimension(full_name)

        # Handle coarray dimensions
        if is_coarray:
            coarray_dims = extract_coarray_dimensions(is_coarray.group(0))
            if coarray_dims:
                if dimension:
                    dimension["dimensions"].extend(coarray_dims["dimensions"])
                else:
                    dimension = coarray_dims

        # Use dimension from attributes if still none found
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
            "length": length,
            "binding_type": binding_type
        }

        variable_descriptions.append(variable_description)

    return variable_descriptions
            
def extract_variable_binding(attributes) -> BindingType:
    binding_type: BindingType = {
        'type': BindingTypeEnum.DEFAULT,
        'name': None
    }
    
    if not attributes:
        return binding_type
    
    # Look for bind attribute in the attrspec list - need to handle weird spacing
    bind_attrs = []
    for attr in attributes:
        # Remove all spaces to normalize before checking
        normalized_attr = attr.replace(" ", "").lower()
        if normalized_attr.startswith('bind('):
            bind_attrs.append(attr)
    
    if bind_attrs:
        bind_attr = bind_attrs[0]  # Use the first bind attribute found
        
        # Look for 'c' binding type - be more tolerant of spacing
        if re.search(r'bind\s*\(\s*c\b', bind_attr.lower()):
            binding_type['type'] = BindingTypeEnum.BIND_C
            
            # Extract name if present
            name_match = re.search(r"name\s*=\s*['\"](.+?)['\"]", bind_attr, re.IGNORECASE)
            if name_match:
                binding_type['name'] = name_match.group(1)
    
    return binding_type