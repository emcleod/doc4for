from typing import List, Optional
from fparser.two.Fortran2003 import (
    Type_Declaration_Stmt, 
    Comment, 
    Entity_Decl, 
    Attr_Spec,
    Access_Spec,
    Dimension_Attr_Spec,
    Dimension_Stmt
)
from fparser.two.utils import walk
from doc4for.models.variable_models import ParameterDescription
from doc4for.utils.comment_utils import is_doc4for_comment, format_comments
from doc4for.parse.common_parser import _extract_type_info, _extract_entity_info, _extract_dimension_info

def parse_parameter(
    declaration: Type_Declaration_Stmt,
    comment_stack: List[Comment],
    default_access: Optional[str],
    dimension_stack: Optional[List[Dimension_Stmt]]=None
) -> List[ParameterDescription]:
    
    description = format_comments(
        comment_stack) if is_doc4for_comment(comment_stack) else ""

    parameter_descriptions = []
    
    # Get type information
    type_info = _extract_type_info(declaration)
    
    # Get attributes
    attributes = [attr.string for attr in walk(declaration, Attr_Spec)]
    attributes.extend([attr.string for attr in walk(declaration, Access_Spec)])
    attributes.remove("PARAMETER") # we already know it's a parameter
    
    # Apply default access if no explicit access is specified
    if default_access and "PUBLIC" not in attributes and "PRIVATE" not in attributes:
        attributes.append(default_access)
        
    # might have a dimension in the attributes e.g. real, dimension(10, 10) :: variable_name
    dimension = _extract_dimension_info(walk(declaration, Dimension_Attr_Spec))

    # Get all entity declarations and extract their info
    for entity_decl in walk(declaration, Entity_Decl):
        param_info = _extract_entity_info(entity_decl)
        if dimension and param_info.get("dimension"):
            raise Exception("Can this happen?") #TODO

        parameter_description: ParameterDescription = {
            "description": description,
            "type": type_info.get("base_type"),
            "name": param_info.get("name"),
            "value": param_info.get("value"),
            "dimension": dimension if dimension else param_info.get("dimension"),
            "polymorphism_type": type_info.get("polymorphism_type"),
            "attributes": attributes,
            "kind": type_info.get("kind"),
            "length": type_info.get("length"),
            "binding_type": type_info.get("binding_type")
        }
        
        parameter_descriptions.append(parameter_description)
    
    return parameter_descriptions

