import logging
from typing import Any, List, Dict, TypeVar, Tuple
from fparser.two.Fortran2003 import (
    Derived_Type_Def,
    Type_Name,
    Component_Part,
    Data_Component_Def_Stmt,
    Component_Decl,
    Name,
    Comment,
    Type_Bound_Procedure_Part,
    Attr_Spec,
    Access_Spec,
    Specific_Binding,
    Type_Attr_Spec,
    Binding_Attr,
    Binding_PASS_Arg_Name,
    Final_Binding,
    Generic_Binding,
    Generic_Spec,
    Dtio_Generic_Spec,
    Component_Attr_Spec,
    Type_Param_Name_List,
    Type_Param_Def_Stmt,
    Type_Param_Attr_Spec,
    Type_Param_Decl_List,
    Type_Param_Decl,
    Derived_Type_Stmt,
    Type_Attr_Spec_List
)
from fparser.two.utils import walk
from doc4for.models.variable_models import DataComponent
from doc4for.models.type_models import TypeDescription, GenericInterface, TypeParameter
from doc4for.models.procedure_models import ProcedureDescription, PassType
from doc4for.utils.comment_utils import format_comments, is_doc4for_comment
from doc4for.parse.common_parser import (_extract_literal_value, _extract_type,
                                         _extract_dimension_info, _extract_type_info
                                        )
from doc4for.models.dimension_models import Dimension

logger: logging.Logger = logging.getLogger(__name__)

T = TypeVar("T")

def handle_type_definition(type_def: Derived_Type_Def, comment_stack: List[Comment]) -> TypeDescription:
    # Extract type name
    type_name = walk(type_def, Type_Name)[0].string
    type_description = format_comments(comment_stack) if is_doc4for_comment(comment_stack) else ""    
    attributes = [attr.string.upper() for attr in walk(type_def, Type_Attr_Spec)]
    
    # look for inline access statements
    attr_spec_lists = walk(type_def, Type_Attr_Spec_List)
    if attr_spec_lists:
        access_specs = walk(attr_spec_lists[0], Access_Spec)
        for access_spec in access_specs:
            attributes.append(access_spec.string.lower())  # 'public' or 'private'

    # if there's an extends attribute, find the parent type    
    parent_name: str = None
    if any("EXTENDS" in attribute for attribute in attributes):
        attr_spec = [spec for spec in walk(type_def, Type_Attr_Spec) if spec.children[0] == "EXTENDS"] 
        if attr_spec:
            parent_name = attr_spec[0].children[1].string
            attributes = [attribute for attribute in attributes if "EXTENDS" not in attribute]

    type_info: TypeDescription = {
        "type_name": type_name,
        "attributes": attributes,  
        "description": type_description,
        "data_components": {},
        "enums": {},
        "procedures": {},
        "generic_interfaces": {},
        "extends": parent_name,
        "type_parameters": {}  
    }

    # Check if this is a parameterized derived type
    derived_type_stmt = walk(type_def, Derived_Type_Stmt)[0]
    if walk(derived_type_stmt, Type_Param_Name_List):
        # Extract type parameters
        type_info["type_parameters"] = extract_type_parameters(type_def)

    # Process the nodes inside the type
    type_comment_stack = []    
    for node in type_def.children:
        if isinstance(node, Comment):
            type_comment_stack.append(node)
        elif isinstance(node, Component_Part):
            type_info["data_components"].update(handle_data_component(node, type_comment_stack))
            type_comment_stack.clear()
        elif isinstance(node, Type_Bound_Procedure_Part): 
            # is it a generic binding
            if walk(node, Generic_Binding):
                type_info["generic_interfaces"].update(handle_generic_interface(node))
            # still need to handle all specific bindings even if there's a generic binding in there
            type_info["procedures"].update(handle_type_bound_procedure(node))
        else:
            pass

    return type_info


def extract_type_parameters(type_def: Derived_Type_Def) -> Dict[str, TypeParameter]:
    """Extract type parameters from a parameterized derived type definition."""
    parameters = {}
    comment_stack = []
    
    for node in type_def.children:
        if isinstance(node, Comment):
            comment_stack.append(node)
        elif isinstance(node, Type_Param_Def_Stmt):
            # Get the parameter type (KIND or LEN)
            param_attr_spec = walk(node, Type_Param_Attr_Spec)[0]
            parameter_type = param_attr_spec.string.upper()
            
            # Process each parameter declaration
            param_decl_list = walk(node, Type_Param_Decl_List)[0]
            for param_item in param_decl_list.children:
                if isinstance(param_item, Name):
                    # Simple parameter without default
                    param_name = param_item.string
                    default = None
                elif isinstance(param_item, Type_Param_Decl):
                    # Parameter with default value
                    param_name = walk(param_item, Name)[0].string
                    # Extract the default value (everything after the '=')
                    default_node = param_item.children[2]
                    default = default_node.string if hasattr(default_node, 'string') else str(default_node)
                else:
                    # Skip commas and other separators
                    continue
                
                # Get description from preceding comments
                description = format_comments(comment_stack) if is_doc4for_comment(comment_stack) else ""
                
                # Create the parameter entry
                parameters[param_name] = {
                    "name": param_name,
                    "type": "INTEGER",  # PDT parameters are always integers
                    "parameter_type": parameter_type,
                    "default": default,
                    "description": description
                }
                
                # Clear comment stack after using it
                comment_stack.clear()
        else:
            # Clear comment stack if we hit something else
            if not isinstance(node, Derived_Type_Stmt):
                comment_stack.clear()
    
    return parameters

def handle_data_component(data_component: Component_Part, 
                          comment_stack: List[Comment]) -> Dict[str, DataComponent]:  
    components: Dict[str, DataComponent] = {}
    for def_stmt in walk(data_component, Data_Component_Def_Stmt):            
        #TODO not sure this is handling comments correctly 
        data_type, polymorphism_type, type_params = _extract_type(def_stmt)
        description: str = format_comments(comment_stack) if is_doc4for_comment(comment_stack) else ""
        dimension: Dimension = _extract_dimension_info(def_stmt)
        attributes: List[str] = [attr.string.upper() for attr in walk(def_stmt, (Attr_Spec, Access_Spec, Component_Attr_Spec))
                                 if not (hasattr(attr, 'string') and attr.string.upper().startswith('DIMENSION'))]
        type_info: Dict[str, str] = _extract_type_info(def_stmt)
        kind = type_info["kind"]
        length = type_info["length"]
        declarations = walk(def_stmt, Component_Decl)
        for declaration in declarations:
            name: str = walk(declaration, Name)[0].string
            initial_value: str = _extract_literal_value(declaration)
            component: DataComponent = {
                "name": name,
                "type": data_type,
                "kind": kind,
                "description": description,
                "dimension": dimension,
                "polymorphism_type": polymorphism_type,
                "len": length,
                "initial_value": initial_value,
                "attributes": attributes,
                "type_params": type_params 
            }
            components[name] = component
    return components

# def handle_data_component(data_component: Component_Part, 
#                           comment_stack: List[Comment]) -> Dict[str, DataComponent]:  
#     components: Dict[str, DataComponent] = {}
#     for def_stmt in walk(data_component, Data_Component_Def_Stmt):            
#         #TODO not sure this is handling comments correctly 
#         data_type, polymorphism_type = _extract_type(def_stmt)
#         description: str = format_comments(comment_stack) if is_doc4for_comment(comment_stack) else ""
#         dimension: Dimension = _extract_dimension_info(def_stmt)
#         attributes: List[str] = [attr.string.upper() for attr in walk(def_stmt, (Attr_Spec, Access_Spec, Component_Attr_Spec))
#                                  if not (hasattr(attr, 'string') and attr.string.upper().startswith('DIMENSION'))]
#         type_info: Dict[str, str] = _extract_type_info(def_stmt)
#         kind = type_info["kind"]
#         length = type_info["length"]
#         declarations = walk(def_stmt, Component_Decl)
#         for declaration in declarations:
#             name: str = walk(declaration, Name)[0].string
#             initial_value: str = _extract_literal_value(declaration)
#             component: DataComponent = {
#                 "name": name,
#                 "type": data_type,
#                 "kind": kind,
#                 "description": description,
#                 "dimension": dimension,
#                 "polymorphism_type": polymorphism_type,
#                 "len": length,
#                 "initial_value": initial_value,
#                 "attributes": attributes
#             }
#             components[name] = component
#     return components

def handle_type_bound_procedure(type_bound_statement: Type_Bound_Procedure_Part) -> Dict[str, ProcedureDescription]:
    procedures = {}
    comment_stack: List[Comment] = []
    for node in type_bound_statement.children:
        if isinstance(node, Comment):
            comment_stack.append(node)
        elif isinstance(node, Specific_Binding):  
            name, bound_to, implementation = extract_procedure_info(node)
            description: str = format_comments(comment_stack) if is_doc4for_comment(comment_stack) else ""
            attributes = [attr.string.upper() for attr in walk(node, (Attr_Spec, Access_Spec))]
            binding_types = [attr.string.upper() for attr in walk(node, Binding_Attr)]            
            binding_args = walk(node, Binding_PASS_Arg_Name)            
            if "DEFERRED" in binding_types:
                binding_types.remove("DEFERRED")
                attributes.append("DEFERRED")         
            pass_name = (walk(binding_args, Name)[0].string if binding_args 
                         else None)
            pass_type = (PassType.NONE if "NOPASS" in binding_types 
                         else PassType.NAMED if pass_name 
                         else PassType.DEFAULT)
            binding_type = None #TODO    
            procedure_description: ProcedureDescription = {
                "name": name,
                "description": description,
                "attributes": attributes,
                "is_final": False,
                "bound_to": bound_to,
                "pass_type": pass_type,
                "pass_name": pass_name,
                "implementation": implementation,
                "binding_type": binding_type
            }
            procedures[name] = procedure_description
            comment_stack.clear()
        elif isinstance(node, Final_Binding):
            binding_type = None
            description: str = format_comments(comment_stack) if is_doc4for_comment(comment_stack) else ""            
            for name in walk(node, Name):
                procedure_description: ProcedureDescription = {
                    "name": name.string,
                    "description": description,
                    "attributes": [],
                    "is_final": True,
                    "bound_to": None,
                    "pass_type": None,
                    "pass_name": None,
                    "implementation": None,
                    "binding_type": binding_type
                }
                procedures[name.string] = procedure_description
            comment_stack.clear()
        else:
            #ignoring everything else including generic interfaces
            comment_stack.clear()
    return procedures

def handle_generic_interface(generic_binding: Generic_Binding) -> Dict[str, GenericInterface]:
    generic_interfaces = {}
    comment_stack: List[Comment] = []
    
    for node in generic_binding.children:
        if isinstance(node, Comment):
            comment_stack.append(node)
        elif isinstance(node, Generic_Binding):
            generic_specs = walk(node, (Generic_Spec, Dtio_Generic_Spec))
            generic_spec, specific_procedures = None, []
            if generic_specs:
                # have an operator, assignment or I/O overload
                generic_spec = generic_specs[0].string
                specific_procedures = [name.string for name in walk(node, Name)]            
            else:
                names = walk(node, Name)
                # first name is generic name, rest are the specific procedures
                generic_spec = names[0].string
                specific_procedures = [name.string for name in names[1:]]
            description = format_comments(comment_stack) if is_doc4for_comment(comment_stack) else ""        
            attributes = [attr.string.upper() for attr in walk(node, (Access_Spec,))]            
            generic_interface = {
                "generic_spec": generic_spec,
                "description": description,
                "attributes": attributes,
                "specific_procedures": specific_procedures
            }            
            # If this generic spec already exists, merge the specific procedures
            if generic_spec in generic_interfaces:
                existing = generic_interfaces[generic_spec]
                existing["specific_procedures"].extend(specific_procedures)
                if description:
                    if existing["description"]:
                        existing["description"] += description
                    else:
                        existing["description"] = description            
            else:
                generic_interfaces[generic_spec] = generic_interface            
            comment_stack.clear()
        else:
            # ignore specific bindings and clear any accumulated comments from them
            comment_stack.clear()
            
    return generic_interfaces

def extract_procedure_info(specific_binding: Specific_Binding) -> Tuple[str, str, str]:
    names = [child for child in specific_binding.children if isinstance(child, Name)]
    has_arrow = "=>" in specific_binding.string
    
    # Default values
    procedure_name = None
    bound_to = None
    implementation = None
    
    if len(names) == 1:
        # Simple case: procedure :: name
        procedure_name = names[0].string
    elif len(names) == 2:
        if has_arrow:
            # Case: procedure :: name => implementation
            procedure_name = names[0].string
            implementation = names[1].string
        else:
            # Case: procedure(interface) :: name
            bound_to = names[0].string
            procedure_name = names[1].string
    
    return procedure_name, bound_to, implementation
    