from typing import List, Tuple, Dict, Any, Optional
from fparser.two.Fortran2003 import (
    Name,
    Interface_Block,
    Comment,
    Name,
    Type_Declaration_Stmt,
    Specification_Part,
    Implicit_Part,
    Procedure_Declaration_Stmt,
    Proc_Decl_List,  # type: ignore[attr-defined]
)
from fparser.two.utils import walk
from doc4for.models.variable_models import PolymorphismType
from doc4for.models.procedure_models import InterfaceDescription

def process_specification_part(
    spec_part: Specification_Part,
    default_access: str
) -> Tuple[List[InterfaceDescription], List[Type_Declaration_Stmt], List[Procedure_Declaration_Stmt]]:
    """Extract interfaces, type declarations, and procedure declarations from a specification part."""
    # avoid a circular dependency
    from doc4for.parse.interface_parser import parse_interface
    spec_comment_stack = []
    interface_blocks_in_order = []
    argument_decls = []
    procedure_decls = []
    
    for spec_child in spec_part.children:
        if isinstance(spec_child, Comment):
            spec_comment_stack.append(spec_child)
        elif isinstance(spec_child, Type_Declaration_Stmt):
            argument_decls.append(spec_child)
        elif isinstance(spec_child, Procedure_Declaration_Stmt):
            procedure_decls.append(spec_child)
        elif isinstance(spec_child, Implicit_Part):
            for implicit_child in spec_child.children:
                if isinstance(implicit_child, Comment):
                    spec_comment_stack.append(implicit_child)
        elif isinstance(spec_child, Interface_Block):
            _, nested_interface_desc = parse_interface(spec_child, spec_comment_stack, default_access)
            spec_comment_stack.clear()
            interface_blocks_in_order.append(nested_interface_desc)
    
    return interface_blocks_in_order, argument_decls, procedure_decls

def match_interfaces_to_procedure_arguments(
    interface_blocks: List[InterfaceDescription],
    dummy_arg_names: List[str],
    parsed_arguments: Dict[str, Any],
    procedure_decls: Optional[List[Procedure_Declaration_Stmt]] = None
) -> Tuple[Dict[str, InterfaceDescription], Dict[str, Dict[str, Any]]]:
    argument_interfaces = {}
    procedure_arguments = {}
    
    # First, handle explicitly declared procedure arguments
    interface_name_to_block = {}
    for interface_block in interface_blocks:
        for proc_name in interface_block["procedures"]:
            interface_name_to_block[proc_name] = interface_block
    
    # Process procedure declarations
    declared_procedure_args = set()
    if procedure_decls:
        for proc_decl in procedure_decls:
            interface_name = None
            arg_names = []
            
            # Extract interface name and argument names
            for child in proc_decl.children:
                if isinstance(child, Name):
                    interface_name = child.string
                elif isinstance(child, Proc_Decl_List):
                    arg_names = [name.string for name in walk(child, Name)]

            if interface_name and interface_name in interface_name_to_block:
                interface_block = interface_name_to_block[interface_name]
                for arg_name in arg_names:
                    declared_procedure_args.add(arg_name)
                    argument_interfaces[arg_name] = interface_block
                    procedure_arguments[arg_name] = {
                        "type": "PROCEDURE",
                        "description": "",
                        "dimension": None,
                        "interface_name": interface_name,
                        "enum_type": None,
                        "attributes": [],
                        "kind": None,
                        "length": None,
                        "polymorphism_type": PolymorphismType.NONE,
                        "default_value": None,
                        "type_params": None
                    }
    
    # Now handle implicit procedure arguments (old style)
    # Find which arguments are NOT covered by type declarations or procedure declarations
    procedure_arg_names = [name for name in dummy_arg_names 
                          if name not in parsed_arguments["intent_in"] 
                          and name not in declared_procedure_args]
    
    # Match remaining interface blocks to procedure arguments positionally
    remaining_interfaces = [block for block in interface_blocks 
                           if not any(arg in declared_procedure_args 
                                     for arg, intf in argument_interfaces.items() 
                                     if intf == block)]
    
    for i, interface_desc in enumerate(remaining_interfaces):
        if i < len(procedure_arg_names):
            arg_name = procedure_arg_names[i]
            interface_proc_name = list(interface_desc["procedures"].keys())[0]
            
            argument_interfaces[arg_name] = interface_desc
            procedure_arguments[arg_name] = {
                "type": "PROCEDURE",
                "description": "",
                "dimension": None,
                "interface_name": interface_proc_name,
                "enum_type": None,
                "attributes": [],
                "kind": None,
                "length": None,
                "polymorphism_type": PolymorphismType.NONE,
                "default_value": None,
                "type_params": None
            }
    
    return argument_interfaces, procedure_arguments
