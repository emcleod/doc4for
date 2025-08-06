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
from doc4for.parse.interface_helper import process_specification_part, match_interfaces_to_procedure_arguments

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
        "operator_symbol": operator_symbol
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

def _extract_dummy_arg_names(proc_stmt: Union[Function_Stmt, Subroutine_Stmt]) -> List[str]:
    dummy_arg_names = []
    dummy_args = walk(proc_stmt, Dummy_Arg_List)
    if dummy_args:
        for dummy_arg in walk(dummy_args, Name):
            dummy_arg_names.append(dummy_arg.string)
    return dummy_arg_names

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
            interface_blocks, argument_decls, procedure_decls = process_specification_part(child, default_access)
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
    argument_interfaces, procedure_arguments = match_interfaces_to_procedure_arguments(
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
    imports = parse_imports_list(import_stmts)  

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

