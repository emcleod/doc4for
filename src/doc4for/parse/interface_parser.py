import logging
from typing import List, Tuple, Union, Dict, Any, Optional
from fparser.two.Fortran2003 import (
    Function_Stmt,
    Subroutine_Stmt,
    Name,
    Interface_Block,
    Comment,
    Prefix_Spec,
    Suffix,
    Name,
    Intrinsic_Type_Spec,
    Type_Declaration_Stmt,
    Language_Binding_Spec,
    Function_Body,
    Subroutine_Body,
    Interface_Stmt,
    Extended_Intrinsic_Op,
    Procedure_Stmt,
    Generic_Spec,
    Defined_Op,
    Specification_Part,
    Implicit_Part,
    Dummy_Arg_List,  # type: ignore[attr-defined]
    Procedure_Declaration_Stmt,
    Proc_Decl_List,  # type: ignore[attr-defined]
    Use_Stmt,
    Import_Stmt,
    External_Stmt
)
from fparser.two.utils import walk
from doc4for.models.procedure_models import InterfaceDescription, FunctionDescription, SubroutineDescription, Argument
from doc4for.models.variable_models import PolymorphismType
from doc4for.parse.procedure_parser import parse_procedure, update_arguments_with_comment_data
from doc4for.parse.common_parser import _extract_binding_type
from doc4for.utils.comment_utils import get_formatted_description
from doc4for.parse.uses_parser import parse_imports_list, parse_uses_list

logger: logging.Logger = logging.getLogger(__name__)


def parse_interface(
    interface: Interface_Block, comment_stack: List[Comment], default_access: Optional[str],
) -> Tuple[str, InterfaceDescription]:
    description = get_formatted_description(comment_stack)
    
    # Extract basic interface information
    interface_stmt = walk(interface, Interface_Stmt)[0]
    name, attributes = _extract_interface_name_and_attributes(interface_stmt)
    operator_symbol = _extract_operator_symbol(interface)
    
    module_procedures = {}
    procedures = {}
    body_comment_stack = []
    
    # Process interface body
    for node in interface.children:
        if isinstance(node, Comment):
            body_comment_stack.append(node)
            
        elif isinstance(node, Procedure_Stmt):
            # Handle module procedures
            proc_names = walk(node, Name)
            body_description = get_formatted_description(body_comment_stack)
            for proc_name in proc_names:
                module_procedures[proc_name.string] = {
                    "name": proc_name.string,
                    "description": body_description
                }
            body_comment_stack.clear()
            
        elif isinstance(node, Function_Body):
            # Process function body
            proc_info = _process_procedure_body(node, Function_Stmt, default_access)
            common = proc_info["common"]
            
            # Extract return information
            return_argument: Argument
            _, return_argument = _extract_return_info(common)
            
            # Extract binding type
            binding_type = _extract_binding_type(walk(common["procedure_declaration"], Language_Binding_Spec))
            
            # Build function description
            function_description: FunctionDescription = {
                "attributes": common["attributes"],
                "description": "",
                "arguments": common["arguments"],
                "in": common["intent_in"],
                "out": common["intent_out"],
                "return": return_argument,
                "argument_interfaces": proc_info["argument_interfaces"],
                "binding_type": binding_type,
                "uses": proc_info["uses"],
                "imports": proc_info["imports"],
                "external_procedures": common["external_procedures"]
            }
            
            procedures[common["procedure_name"]] = function_description
            update_arguments_with_comment_data(proc_info["procedure_comment_stack"], function_description)
            
        elif isinstance(node, Subroutine_Body):
            # Process subroutine body
            proc_info = _process_procedure_body(node, Subroutine_Stmt, default_access)
            common = proc_info["common"]
            
            # Extract binding type
            binding_type = _extract_binding_type(walk(common["procedure_declaration"], Language_Binding_Spec))
            
            # Build subroutine description
            subroutine_description: SubroutineDescription = {
                "attributes": common["attributes"],
                "description": "",
                "arguments": common["arguments"],
                "in": common["intent_in"],
                "out": common["intent_out"],
                "argument_interfaces": proc_info["argument_interfaces"],
                "binding_type": binding_type,
                "uses": proc_info["uses"],
                "imports": proc_info["imports"],
                "external_procedures": common["external_procedures"]
            }
            
            procedures[common["procedure_name"]] = subroutine_description
            update_arguments_with_comment_data(proc_info["procedure_comment_stack"], subroutine_description)
    
    # Apply default access if no explicit access is specified
    if "PUBLIC" not in attributes and "PRIVATE" not in attributes:
        attributes.append(default_access)

    interface_description: InterfaceDescription = {
        "description": description,
        "attributes": attributes,
        "procedures": procedures,
        "module_procedures": module_procedures,
        "name": name,
        "operator_symbol": operator_symbol,
        "uses": {} #TODO
    }
    
    return name, interface_description

def _extract_operator_symbol(interface: Interface_Block) -> Optional[str]:
    """Extract operator symbol from interface if it's an operator interface."""
    generic_specs = walk(interface, Generic_Spec)
    if not generic_specs:
        return None
        
    operators = walk(generic_specs, Extended_Intrinsic_Op)
    if operators:
        return operators[0].string
    
    defined_operators = walk(generic_specs, Defined_Op)
    if defined_operators:
        return defined_operators[0].string
    
    return generic_specs[0].children[1]


def _extract_interface_name_and_attributes(interface_stmt: Interface_Stmt) -> Tuple[str, List[str]]:
    """Extract interface name and attributes from interface statement."""
    name, attributes = "", []
    
    if interface_stmt.children[0] == "ABSTRACT":
        attributes.append("ABSTRACT")
    else:
        names = walk(interface_stmt, Name)
        if names:
            name = names[0].string
    
    return name, attributes

def _process_specification_part(
    spec_part: Specification_Part,
    default_access: str
) -> Tuple[List[InterfaceDescription], List[Type_Declaration_Stmt], List[Procedure_Declaration_Stmt]]:
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


def _extract_dummy_arg_names(proc_stmt: Union[Function_Stmt, Subroutine_Stmt]) -> List[str]:
    dummy_arg_names = []
    dummy_args = walk(proc_stmt, Dummy_Arg_List)
    if dummy_args:
        for dummy_arg in walk(dummy_args, Name):
            dummy_arg_names.append(dummy_arg.string)
    return dummy_arg_names

def _match_interfaces_to_procedure_arguments(
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

def _process_procedure_body(
    node: Union[Function_Body, Subroutine_Body],
    stmt_type: type,
    default_access: Optional[str]
) -> Dict[str, Any]:
    """Process function or subroutine body and extract all relevant information."""
    # Extract the procedure statement
    proc_stmt = walk(node, stmt_type)[0]
    dummy_arg_names = _extract_dummy_arg_names(proc_stmt)
    
    # Process body children sequentially
    procedure_comment_stack = []
    argument_decls = [] # don't walk because we pick up everything
    procedure_decls = []
    for child in node.children:
        if isinstance(child, Comment):
            procedure_comment_stack.append(child)
        elif isinstance(child, Type_Declaration_Stmt):
            argument_decls.append(child)
        elif isinstance(child, Specification_Part):
            interface_blocks, argument_decls, procedure_decls = _process_specification_part(child, default_access)
            break
    else:
        interface_blocks = []
    
    external_decls = walk(node, External_Stmt)
    # Parse the procedure - note that the external statements list is deliberately empty
    common: Optional[Dict[str, Any]] = parse_procedure(proc_stmt, stmt_type, argument_decls, 
                                             procedure_decls, external_decls, 
                                             procedure_comment_stack, default_access)
    if not common:
        #TODO log this or do something sensible
        return None
    
    # Match interfaces to arguments
    argument_interfaces, procedure_arguments = _match_interfaces_to_procedure_arguments(
        interface_blocks, dummy_arg_names, common, procedure_decls
    )
    
    # Update common with procedure arguments
    common["intent_in"].update(procedure_arguments)
    
    # Rebuild arguments list in correct order
    ordered_arguments = []
    for arg_name in dummy_arg_names:
        if arg_name in common["intent_in"] or arg_name in common["intent_out"] or arg_name in procedure_arguments:
            ordered_arguments.append(arg_name)
    common["arguments"] = ordered_arguments
    
    spec_parts = walk(node, Specification_Part)
    use_stmts = []
    import_stmts = []
    
    if spec_parts:
        use_stmts = walk(spec_parts[0], Use_Stmt)
        import_stmts = walk(spec_parts[0], Import_Stmt)
    
    # Add USE and IMPORT parsing
    uses = parse_uses_list(use_stmts)
    imports = parse_imports_list(import_stmts)  # New function

    return {
        "common": common,
        "argument_interfaces": argument_interfaces,
        "procedure_comment_stack": procedure_comment_stack,
        "imports": imports,
        "uses": uses
    }

def _extract_return_info(common: Dict[str, Any]) -> Tuple[str, Optional[Argument]]:
    return_type, return_variable = None, None
    
    if common["prefixes"]:
        for prefix_node in common["prefixes"][0].children:
            if not isinstance(prefix_node, Prefix_Spec):
                if return_type is not None:
                    logger.error(f"Found more than one return type for {common['procedure_name']}")
                    continue
                return_type = prefix_node.string.upper() if isinstance(prefix_node, Intrinsic_Type_Spec) else prefix_node.string
    
    suffixes = walk(common["procedure_declaration"], Suffix)
    if suffixes:
        return_variable = walk(suffixes, Name)[0].string
    else:
        return_variable = common["procedure_name"]
    
    # Handle return argument
    return_argument: Optional[Argument] = None
    if return_variable in common["all_parsed_arguments"]:
        return_argument = common["all_parsed_arguments"][return_variable]
    
    if not return_argument:
        return_argument = {
            "type": return_type,
            "description": "",
            "dimension": None,
            "enum_type": None,
            "interface_name": None,
            "attributes": [],
            "kind": None,
            "length": None,
            "default_value": None,
            "polymorphism_type": PolymorphismType.NONE,
            "type_params": None  
        }    
    return return_variable, return_argument

