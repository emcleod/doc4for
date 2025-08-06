import logging
from typing import Optional, List, Dict, TypeVar, Tuple, Type, Callable, Any
from dataclasses import dataclass
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
    Binding_Attr_List,  # type: ignore[attr-defined]
    Binding_Attr,
    Binding_PASS_Arg_Name,
    Final_Binding,
    Generic_Binding,
    Generic_Spec,
    Dtio_Generic_Spec,
    Component_Attr_Spec,
    Type_Param_Name_List, # type: ignore[attr-defined]
    Type_Param_Def_Stmt,
    Type_Param_Attr_Spec,
    Type_Param_Decl_List, # type: ignore[attr-defined]
    Type_Param_Decl,
    Derived_Type_Stmt,
    Type_Attr_Spec_List, # type: ignore[attr-defined]
    Proc_Decl,
    Proc_Component_Def_Stmt,
    Proc_Component_Attr_Spec_List, # type: ignore[attr-defined]
    Proc_Component_Attr_Spec,
    Component_Attr_Spec_List, # type: ignore[attr-defined]
    Binding_Name_List, # type: ignore[attr-defined]
    Private_Components_Stmt,
    Binding_Private_Stmt,
    Proc_Decl_List # type: ignore[attr-defined]
)
from fparser.two.utils import walk
from doc4for.models.common import BindingType, BindingTypeEnum
from doc4for.models.variable_models import DataComponent, PolymorphismType
from doc4for.models.type_models import TypeDescription, GenericInterface, TypeParameter
from doc4for.models.procedure_models import ProcedureDescription, PassType
from doc4for.utils.comment_utils import get_formatted_description
from doc4for.parse.common_parser import (_extract_literal_value, _extract_type,
                                         _extract_dimension_info, _extract_type_info
                                        )
from doc4for.models.dimension_models import Dimension


logger: logging.Logger = logging.getLogger(__name__)

T = TypeVar("T")

@dataclass
class TypeBoundContext:
    """Context object for handling type-bound procedures."""
    procedures: Dict[str, ProcedureDescription]
    generic_interfaces: Dict[str, GenericInterface]
    comment_stack: List[Comment]
    access: str = "PUBLIC"


class TypeBoundProcedureHandler:
    """Handler for processing type-bound procedures."""
    
    def __init__(self):
        self._handlers: Dict[Type, Callable[[Any, TypeBoundContext], None]] = {}
        
    def register_handler(self, node_type: Type, handler: Callable[[Any, TypeBoundContext], None]) -> None:
        """Register a handler for a specific node type."""
        self._handlers[node_type] = handler
        
    def get_handler(self, node_type: Type) -> Optional[Callable[[Any, TypeBoundContext], None]]:
        """Get the handler for a specific node type."""
        return self._handlers.get(node_type)
        
    def handle(self, node: Any, context: TypeBoundContext) -> None:
        """Handle a node using the appropriate handler."""
        handler = self.get_handler(type(node))
        if handler:
            handler(node, context)


# Handler instance
_type_bound_handler_instance: Optional[TypeBoundProcedureHandler] = None


def _get_type_bound_handler() -> TypeBoundProcedureHandler:
    """Get the singleton instance of TypeBoundProcedureHandler."""
    global _type_bound_handler_instance
    if _type_bound_handler_instance is None:
        handler = TypeBoundProcedureHandler()
        handler.register_handler(Comment, handle_type_bound_comment)
        handler.register_handler(Specific_Binding, handle_specific_binding)
        handler.register_handler(Final_Binding, handle_final_binding)
        handler.register_handler(Generic_Binding, handle_generic_binding)
        handler.register_handler(Binding_Private_Stmt, handle_binding_private_stmt)  
        _type_bound_handler_instance = handler
    return _type_bound_handler_instance


def handle_type_definition(type_def: Derived_Type_Def, comment_stack: List[Comment], default_access: Optional[str]) -> TypeDescription:
    # Extract type name
    type_name = walk(type_def, Type_Name)[0].string
    type_description = get_formatted_description(comment_stack)
    attributes = [attr.string.upper() for attr in walk(type_def, Type_Attr_Spec)]

    # Determine type's component default access
    component_default_access = "PRIVATE" if walk(type_def, Private_Components_Stmt) else "PUBLIC"

    # Extract binding type - check for BIND attribute
    binding_type: Optional[BindingType] = None
    for attr_spec in walk(type_def, Type_Attr_Spec):
        if attr_spec.children[0] == "BIND":
            # This is a bind attribute
            if len(attr_spec.children) > 1 and attr_spec.children[1] == "C":
                binding_type = {"type": BindingTypeEnum.BIND_C, "name": None}
                # TODO: Handle named bindings when fparser supports them
            break

    # look for inline access statements
    attr_spec_lists = walk(type_def, Type_Attr_Spec_List)
    if attr_spec_lists:
        access_specs = walk(attr_spec_lists[0], Access_Spec)
        for access_spec in access_specs:
            attributes.append(access_spec.string.upper())  # 'public' or 'private'

    # if there's an extends attribute, find the parent type    
    parent_name: Optional[str] = None
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
        "type_parameters": {},
        "binding_type": binding_type
    }

    # Check if this is a parameterized derived type
    derived_type_stmt = walk(type_def, Derived_Type_Stmt)[0]
    if walk(derived_type_stmt, Type_Param_Name_List):
        # Extract type parameters
        type_info["type_parameters"] = extract_type_parameters(type_def)

    # Process the nodes inside the type
    type_comment_stack: List[Comment] = []    
    for node in type_def.children:
        if isinstance(node, Comment):
            type_comment_stack.append(node)
        elif isinstance(node, Component_Part):
            # Check what kind of components are in this Component_Part
            if walk(node, Data_Component_Def_Stmt):
                type_info["data_components"].update(handle_data_component(node, type_comment_stack))
            if walk(node, Proc_Component_Def_Stmt):
                type_info["procedures"].update(handle_procedure_component(node, type_comment_stack))
            type_comment_stack.clear()
        elif isinstance(node, Type_Bound_Procedure_Part):
            # Handle all type-bound procedures using the handler pattern
            procedures, generic_interfaces = handle_type_bound_procedures(node)
            type_info["procedures"].update(procedures)
            type_info["generic_interfaces"].update(generic_interfaces)

    # Post-process to add access information to components without explicit access

    # for type, use module-level access and only add if there isn't already an explicit declaration
    if "PUBLIC" not in attributes and "PRIVATE" not in attributes:
        attributes.append(default_access)

    # declarations inside the type use they type's access, which is public by default
    for component in type_info["data_components"].values():
        if not any(attr in ["PUBLIC", "PRIVATE"] for attr in component["attributes"]):
            component["attributes"].append(component_default_access)
    for proc in type_info["procedures"].values():
        if not proc["is_final"]: # final procedures don't have access statements
            if not any(attr in ["PUBLIC", "PRIVATE"] for attr in proc["attributes"]):
                proc["attributes"].append(component_default_access)

    return type_info

def handle_binding_private_stmt(node: Any, context: TypeBoundContext) -> None:
    """Handle binding private statement - changes default access to PRIVATE."""
    context.access = "PRIVATE"

def handle_type_bound_procedures(type_bound_statement: Type_Bound_Procedure_Part) -> Tuple[Dict[str, ProcedureDescription], Dict[str, GenericInterface]]:
    """Handle all type-bound procedures using the handler pattern."""
    context = TypeBoundContext(
        procedures={},
        generic_interfaces={},
        comment_stack=[], 
        access="PUBLIC" # public by default, have to be explicitly marked as private
    )
    handler = _get_type_bound_handler()
    
    for node in type_bound_statement.children:
        handler.handle(node, context)
        # Clear comment stack after non-comment nodes
        if not isinstance(node, Comment):
            context.comment_stack.clear()
    
    return context.procedures, context.generic_interfaces


def handle_type_bound_comment(node: Comment, context: TypeBoundContext) -> None:
    """Handle comment nodes by adding them to the comment stack."""
    context.comment_stack.append(node)


def handle_specific_binding(node: Specific_Binding, context: TypeBoundContext) -> None:
    """Handle specific binding nodes."""
    name, bound_to, implementation = extract_procedure_info(node)
    # Handle the case where name is None
    if name is None:
        logger.error(f"Could not extract procedure name from specific binding: {node}")
        return
    description: str = get_formatted_description(context.comment_stack)
    attributes: List[str] = [attr.string.upper() for attr in walk(node, (Attr_Spec, Access_Spec, Component_Attr_Spec))
                                 if not (hasattr(attr, 'string') and attr.string.upper().startswith('DIMENSION'))]
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
    # If no explicit access specifier, use the current default
    if not any(attr in ["PUBLIC", "PRIVATE"] for attr in attributes):
        attributes.append(context.access)    
    procedure_description: ProcedureDescription = {
        "name": name,
        "description": description,
        "attributes": attributes,
        "is_final": False,
        "bound_to": bound_to,
        "pass_type": pass_type,
        "pass_name": pass_name,
        "implementation": implementation
    }
    context.procedures[name] = procedure_description


def handle_final_binding(node: Final_Binding, context: TypeBoundContext) -> None:
    """Handle final binding nodes."""
    description: str = get_formatted_description(context.comment_stack)       
    for name in walk(node, Name):
        procedure_description: ProcedureDescription = {
            "name": name.string,
            "description": description,
            "attributes": [],
            "is_final": True,
            "bound_to": None,
            "pass_type": None,
            "pass_name": None,
            "implementation": None
        }
        context.procedures[name.string] = procedure_description


def handle_generic_binding(node: Generic_Binding, context: TypeBoundContext) -> None:
    """Handle generic binding nodes."""
    generic_specs = walk(node, (Generic_Spec, Dtio_Generic_Spec))
    generic_spec: str  # Declare the type
    specific_procedures: List[str] = []
    
    if generic_specs:
        # have an operator, assignment or I/O overload
        generic_spec = generic_specs[0].string
        specific_procedures = [name.string for name in walk(node, Name)]            
    else:
        names = walk(node, Name)
        # first name is generic name, rest are the specific procedures
        if not names:  # Handle edge case
            logger.warning(f"No names found in generic binding: {node}")
            return
        generic_spec = names[0].string
        specific_procedures = [name.string for name in names[1:]]
    
    description = get_formatted_description(context.comment_stack)     
    attributes: List[str] = [attr.string.upper() for attr in walk(node, (Attr_Spec, Access_Spec, Component_Attr_Spec))
                                 if not (hasattr(attr, 'string') and attr.string.upper().startswith('DIMENSION'))]
    # If no explicit access specifier, use the current default
    if not any(attr in ["PUBLIC", "PRIVATE"] for attr in attributes):
        attributes.append(context.access)

    generic_interface: GenericInterface = {
        "generic_spec": generic_spec,
        "description": description,
        "attributes": attributes,
        "specific_procedures": specific_procedures
    }
    
    # If this generic spec already exists, merge the specific procedures
    if generic_spec in context.generic_interfaces:
        existing = context.generic_interfaces[generic_spec]
        existing["specific_procedures"].extend(specific_procedures)
        if description:
            if existing["description"]:
                existing["description"] += description
            else:
                existing["description"] = description            
    else:
        context.generic_interfaces[generic_spec] = generic_interface


def extract_type_parameters(type_def: Derived_Type_Def) -> Dict[str, TypeParameter]:
    """Extract type parameters from a parameterized derived type definition."""
    parameters: Dict[str, TypeParameter] = {}
    comment_stack: List[Comment] = []
    
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
                description = get_formatted_description(comment_stack)
                
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
        data_type: str
        polymorphism_type: PolymorphismType
        type_params: Optional[str]
        data_type, polymorphism_type, type_params = _extract_type(def_stmt)
        description: str = get_formatted_description(comment_stack)
        dimension: Optional[Dimension] = _extract_dimension_info(def_stmt)
        attributes: List[str] = [attr.string.upper() for attr in walk(def_stmt, (Attr_Spec, Access_Spec, Component_Attr_Spec))
                                 if not (hasattr(attr, 'string') and attr.string.upper().startswith('DIMENSION'))]
        type_info: Dict[str, str] = _extract_type_info(def_stmt)
        kind: str = type_info["kind"]
        length: str = type_info["length"]
        declarations = walk(def_stmt, Component_Decl)
        for declaration in declarations:
            name: str = walk(declaration, Name)[0].string
            initial_value: Optional[str] = _extract_literal_value(declaration)
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


def handle_procedure_component(component_part: Component_Part, 
                               comment_stack: List[Comment]) -> Dict[str, ProcedureDescription]:
    procedures: Dict[str, ProcedureDescription] = {}
    
    for proc_stmt in walk(component_part, Proc_Component_Def_Stmt):
        interface_name = proc_stmt.children[0].string
        description = get_formatted_description(comment_stack)
        
        # Extract attributes
        attributes = []
        pass_type = PassType.DEFAULT  # Default for procedure components
        pass_name = None
                
        attr_spec_list = walk(proc_stmt, Proc_Component_Attr_Spec_List)
        #TODO check for bind(c) in here when fparser supports it
        if attr_spec_list:
            for attr_spec in walk(attr_spec_list[0], Proc_Component_Attr_Spec):                
                attr_string = attr_spec.string.upper()
                if attr_string == "NOPASS":
                    pass_type = PassType.NONE
                elif attr_string == "PASS":
                    pass_type = PassType.DEFAULT
                    # Check if PASS has an argument like PASS(arg_name)
                    if hasattr(attr_spec, 'children') and len(attr_spec.children) > 1:
                        pass_type = PassType.NAMED
                        pass_name = attr_spec.children[1].string
                else:
                    attributes.append(attr_string.upper())
            
            # Look for access specifiers in the attribute list only
            for attr_spec in walk(attr_spec_list[0], Access_Spec):
                attributes.append(attr_spec.string.upper())        

        # Extract procedure declarations
        # Handle more complex declarations that produce Proc_Decl nodes
        proc_decls = walk(proc_stmt, Proc_Decl)
        for proc_decl in proc_decls:
            name = proc_decl.children[0].string
            
            procedure_info: ProcedureDescription = {
                "name": name,
                "description": description,
                "attributes": attributes,
                "is_final": False,  # Procedure components can't be FINAL
                "bound_to": interface_name,  # The interface it's bound to
                "pass_type": pass_type,
                "pass_name": pass_name,
                "implementation": None,  # No implementation for procedure pointer components
            }
            procedures[name] = procedure_info
    
        # Handle simple procedure pointers: procedure(interface), pointer :: name
        proc_decl_list = walk(proc_stmt, Proc_Decl_List)
        if proc_decl_list:
            for item in proc_decl_list[0].children:
                if isinstance(item, Name):
                    name = item.string
                elif isinstance(item, Proc_Decl):
                    name = item.children[0].string
                else:
                    continue  # Skip commas and other separators
                
                procedure_info: ProcedureDescription = {
                    "name": name,
                    "description": description,
                    "attributes": attributes,
                    "is_final": False,  # Procedure components can't be FINAL
                    "bound_to": interface_name,  # The interface it's bound to
                    "pass_type": pass_type,
                    "pass_name": pass_name,
                    "implementation": None,  # No implementation for procedure pointer components
                }
                procedures[name] = procedure_info
    return procedures


def extract_procedure_info(specific_binding: Specific_Binding) -> Tuple[Optional[str], Optional[str], Optional[str]]:
    names = [child for child in specific_binding.children if isinstance(child, Name)]
    binding_string = getattr(specific_binding, "string", "")
    has_arrow: bool = "=>" in binding_string
    
    # Default values
    procedure_name: Optional[str] = None
    bound_to: Optional[str] = None
    implementation: Optional[str] = None
    
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