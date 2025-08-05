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
from doc4for.utils.comment_utils import is_doc4for_comment, format_comments
from doc4for.parse.common_parser import _extract_type_info, _extract_entity_info, _extract_dimension_info

# TODO with defined type arrays, link to their type when generating
# the documents
def parse_variable(
    declaration: Type_Declaration_Stmt,
    comment_stack: List[Comment],
    default_access: Optional[str],
    dimension_stack: Optional[List[Dimension_Stmt]]=None
) -> List[VariableDescription]:

    description = format_comments(comment_stack) if is_doc4for_comment(comment_stack) else ""
    
    type_info = _extract_type_info(declaration)

    # Get attributes
    attributes = [attr.string for attr in walk(declaration, Attr_Spec)]
    attributes.extend([attr.string for attr in walk(declaration, Access_Spec)])
    
    # Apply default access if no explicit access is specified. Need the null check in 
    # case we're inside a block data
    if default_access and "PUBLIC" not in attributes and "PRIVATE" not in attributes:
        attributes.append(default_access)

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
            "binding_type": type_info.get("binding_type"),
            "is_saved": False
        }        
        variable_descriptions.append(variable_description)

    return variable_descriptions
            
