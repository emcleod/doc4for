from __future__ import annotations
import logging
from typing import Dict, TYPE_CHECKING, Tuple
from fparser.two.Fortran2003 import (
    Type_Declaration_Stmt,
    Dimension_Attr_Spec,
    Access_Spec,
    Attr_Spec,
    Entity_Decl,
    Intent_Spec,
    Assumed_Size_Spec
)
from fparser.two.utils import walk
from doc4for.models.procedure_models import Argument
from doc4for.models.dimension_models import ArrayBound, BoundType
if TYPE_CHECKING:
    from doc4for.models.dimension_models import Dimension
from doc4for.parse.common_parser import _extract_type_info, _extract_dimension_info, _extract_entity_info

logger: logging.Logger = logging.getLogger(__name__)

def parse_arguments(declaration: Type_Declaration_Stmt) -> Tuple[Dict[str, Argument], str]:
    # Extract type information using your existing function
    type_info = _extract_type_info(declaration)
    
    intent = None
    intent_spec = walk(declaration, Intent_Spec)
    if intent_spec:
        intent = intent_spec[0].string

    # Get attributes for this declaration
    attributes = [attr.string for attr in walk(declaration, Attr_Spec)]
    attributes.extend([attr.string for attr in walk(declaration, Access_Spec)])
    
    # Check for dimension in attributes
    dimension_attr = walk(declaration, Dimension_Attr_Spec)
    base_dimension = _extract_dimension_info(dimension_attr) if dimension_attr else None
    
    # if no dimension, it might be assumed size e.g. real, intent(in) :: arr(*)
    if not base_dimension:
        assumed_size_decl = walk(declaration, Assumed_Size_Spec)
        if assumed_size_decl:
            base_dimension = {}
            dimension_list = []
            for _ in range(len(assumed_size_decl)):
                dimension_list.append(ArrayBound(bound_type=BoundType.ASSUMED_SIZE))
            base_dimension["dimensions"] = dimension_list
    
    arguments = {}
    # Process each entity in this declaration
    for entity_decl in walk(declaration, Entity_Decl):
        entity_info = _extract_entity_info(entity_decl)
        arg_name = entity_info["name"]
        
        # Create the Argument object
        argument: Argument = {
            "type": type_info.get("base_type"),  
            "description": "",  # Will be filled later
            "dimension": entity_info.get("dimension") or base_dimension,
            "interface_name": None,  # Set if this is a procedure argument
            "enum_type": None,  # Set if this is an enum type
        }
        
        arguments[arg_name] = argument
    
    return arguments, intent