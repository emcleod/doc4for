from __future__ import annotations
import logging
from typing import Dict, Tuple
from fparser.two.Fortran2003 import (
    Type_Declaration_Stmt,
    Dimension_Attr_Spec,
    Access_Spec,
    Attr_Spec,
    Entity_Decl,
    Intent_Spec,
    Assumed_Size_Spec,
    Procedure_Declaration_Stmt,
    Proc_Decl_List,     # type: ignore[attr-defined]
    Initialization
)
from fparser.two.utils import walk
from doc4for.models.procedure_models import Argument
from doc4for.models.dimension_models import ArrayBound, BoundType
from doc4for.models.variable_models import PolymorphismType
from doc4for.parse.common_parser import _extract_type_info, _extract_dimension_info, _extract_entity_info

logger: logging.Logger = logging.getLogger(__name__)


def parse_procedure_argument(declaration: Procedure_Declaration_Stmt) -> Tuple[Dict[str, Argument], str]:
    type_info = "PROCEDURE"
    # procedures are intent in by default
    intent = "IN"

    # Get attributes for this declaration
    attributes = [attr.string for attr in walk(declaration, Attr_Spec)]
    attributes.extend([attr.string for attr in walk(declaration, Access_Spec)])

    arguments = {}
    interface_name = declaration.children[0].string
    for argument_name in walk(declaration, Proc_Decl_List)[0].children:
        arguments[argument_name.string] = {
            "type": type_info,  
            "kind": None,
            "length": None, 
            "description": "",  # Will be filled in later
            "dimension": None, # TODO can there be a dimension here? base_dimension,
            "attributes": attributes,
            "default_value": None,
            "interface_name": interface_name,  
            "enum_type": None,
            "polymorphism_type": PolymorphismType.NONE,
            "type_params": None  
        }
    return arguments, intent

def parse_arguments(declaration: Type_Declaration_Stmt) -> Tuple[Dict[str, Argument], str]:
    type_info = _extract_type_info(declaration)
    
    intent = None
    intent_spec = walk(declaration, Intent_Spec)
    if intent_spec:
        intent = intent_spec[0].string

    # Get attributes for this declaration
    attributes = [attr.string for attr in walk(declaration, Attr_Spec)]
    attributes.extend([attr.string for attr in walk(declaration, Access_Spec)])
    
    # Get the default value if it exists
    initializations = walk(declaration, Initialization)
    default_value = initializations[0].children[1].string if initializations else None

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
        
        arguments[arg_name] = {
            "type": type_info.get("base_type"),  
            "kind": type_info.get("kind"),
            "length": type_info.get("length"), 
            "description": "",  # Will be filled in later
            "dimension": entity_info.get("dimension") or base_dimension,
            "attributes": attributes,
            "default_value": default_value,
            "interface_name": None,  
            "enum_type": None, 
            "polymorphism_type": type_info.get("polymorphism_type"),
            "type_params": type_info.get("type_params")  # Changed from type_param to type_params
        } 
    return arguments, intent
