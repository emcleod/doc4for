import re
from typing import List, Optional
from fparser.two.Fortran2003 import (
    Type_Declaration_Stmt,
    Comment,
    Entity_Decl,
    Access_Spec,
    Attr_Spec,
    Dimension_Attr_Spec,
    Dimension_Stmt
)
from fparser.two.utils import walk
from doc4for.models.variable_models import VariableDescription
from doc4for.models.common import BindingTypeEnum, BindingType
from doc4for.parse.dimension_parser import extract_dimension_from_attributes, extract_coarray_dimensions, extract_variable_dimension
from doc4for.parse.array_utils import parse_initialization_value
from doc4for.utils.comment_utils import is_doc4for_comment, format_comments
from doc4for.parse.parsing_utils import get_attributes, extract_kind, get_character_length
from doc4for.parse.common_parser import _extract_type_info, _extract_entity_info, _extract_dimension_info

# TODO with defined type arrays, link to their type when generating
# the documents
def parse_variable(
    declaration: Type_Declaration_Stmt,
    comment_stack: List[Comment],
    dimension_stack: Optional[List[Dimension_Stmt]]=None
) -> List[VariableDescription]:

    description = format_comments(
        comment_stack) if is_doc4for_comment(comment_stack) else ""
    
    type_info = _extract_type_info(declaration)

    # Get attributes
    attributes = [attr.string for attr in walk(declaration, Attr_Spec)]
    attributes.extend([attr.string for attr in walk(declaration, Access_Spec)])
    
    # might have a dimension in the attributes e.g. real, dimension(10, 10) :: variable_name
    dimension = _extract_dimension_info(walk(declaration, Dimension_Attr_Spec))

    variable_descriptions: List[VariableDescription] = []

    for entity_decl in walk(declaration, Entity_Decl):
        variable_info = _extract_entity_info(entity_decl)
        if dimension_stack:
            # for F77-style declarations like
            # DIMENSION X(10)
            # REAL x
            dimension_specs = next((specs for dim_stmt in dimension_stack 
                            for name, specs in dim_stmt.items[0]
                            if name.string == variable_info["name"]), None)
            dimension = _extract_dimension_info(dimension_specs)
        
        variable_description: VariableDescription = {
            "description": description,
            "type": type_info.get("base_type"),
            "name": variable_info.get("name"),
            "dimension": dimension if dimension else variable_info.get("dimension"),
            "polymorphism_type": type_info.get("polymorphism_type"),
            "attributes": attributes,
            "kind": type_info.get("kind"),
            "initial_value": variable_info.get("value"),
            "length": type_info.get("length"),
            "binding_type": type_info.get("binding_type")
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