import logging
from typing import List, Tuple, Union, Dict, Any
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
    Procedure_Declaration_Stmt
)
from fparser.two.utils import walk
from doc4for.models.procedure_models import InterfaceDescription
from doc4for.utils.comment_utils import format_comments
from doc4for.parse.procedure_parser import parse_procedure, update_arguments_with_comment_data
from doc4for.parse.common_parser import _extract_binding_type
from doc4for.utils.comment_utils import format_comments, is_doc4for_comment

logger: logging.Logger = logging.getLogger(__name__)


def parse_interface(
    interface: Interface_Block, comment_stack: List[Comment]
) -> Tuple[str, InterfaceDescription]:
    """Parse an interface block and return interface name and description."""
    description = format_comments(comment_stack) if is_doc4for_comment(comment_stack) else ""
    
    # Extract basic interface information
    interface_stmt = walk(interface, Interface_Stmt)[0]
    name, attributes = _extract_interface_name_and_attributes(interface_stmt)
    operator_symbol = _extract_operator_symbol(interface)
    
    module_procedures = {}
    procedures = {}
    body_comment_stack = []
    procedure_decls = []
    
    # Process interface body
    for node in interface.children:
        if isinstance(node, Comment):
            body_comment_stack.append(node)
            
        elif isinstance(node, Procedure_Stmt):
            # Handle module procedures
            proc_names = walk(node, Name)
            body_description = format_comments(body_comment_stack) if is_doc4for_comment(body_comment_stack) else ""
            for proc_name in proc_names:
                module_procedures[proc_name.string] = {
                    "name": proc_name.string,
                    "description": body_description
                }
            body_comment_stack.clear()
            
        elif isinstance(node, Function_Body):
            # Process function body
            proc_info = _process_procedure_body(node, Function_Stmt, procedure_decls)
            common = proc_info["common"]
            
            # Extract return information
            _, return_argument = _extract_return_info(common)
            
            # Extract binding type
            suffixes = walk(common["procedure_declaration"], Suffix)
            binding_type = _extract_binding_type(walk(suffixes, Language_Binding_Spec)) if suffixes else None
            
            # Build function description
            function_description = {
                "attributes": common["attributes"],
                "description": "",
                "arguments": common["arguments"],
                "in": common["intent_in"],
                "out": common["intent_out"],
                "return": return_argument,
                "argument_interfaces": proc_info["argument_interfaces"],
                "binding_type": binding_type
            }
            
            procedures[common["procedure_name"]] = function_description
            update_arguments_with_comment_data(proc_info["procedure_comment_stack"], function_description)
            
        elif isinstance(node, Subroutine_Body):
            # Process subroutine body
            proc_info = _process_procedure_body(node, Subroutine_Stmt, procedure_decls)
            common = proc_info["common"]
            
            # Extract binding type
            suffixes = walk(common["procedure_declaration"], Suffix)
            binding_type = _extract_binding_type(walk(suffixes, Language_Binding_Spec)) if suffixes else None
            
            # Build subroutine description
            subroutine_description = {
                "attributes": common["attributes"],
                "description": "",
                "arguments": common["arguments"],
                "in": common["intent_in"],
                "out": common["intent_out"],
                "argument_interfaces": proc_info["argument_interfaces"],
                "binding_type": binding_type
            }
            
            procedures[common["procedure_name"]] = subroutine_description
            update_arguments_with_comment_data(proc_info["procedure_comment_stack"], subroutine_description)
    
    interface_description: InterfaceDescription = {
        "description": description,
        "attributes": attributes,
        "procedures": procedures,
        "module_procedures": module_procedures,
        "name": name,
        "operator_symbol": operator_symbol
    }
    
    return name, interface_description

def _extract_operator_symbol(interface: Interface_Block) -> str:
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
    argument_decls: List[Type_Declaration_Stmt]
) -> List[InterfaceDescription]:
    """Process a specification part to extract type declarations and interfaces."""
    spec_comment_stack = []
    interface_blocks_in_order = []
    
    for spec_child in spec_part.children:
        if isinstance(spec_child, Comment):
            spec_comment_stack.append(spec_child)
        elif isinstance(spec_child, Type_Declaration_Stmt):
            argument_decls.append(spec_child)
        elif isinstance(spec_child, Implicit_Part):
            for implicit_child in spec_child.children:
                if isinstance(implicit_child, Comment):
                    spec_comment_stack.append(implicit_child)
        elif isinstance(spec_child, Interface_Block):
            _, nested_interface_desc = parse_interface(spec_child, spec_comment_stack)
            spec_comment_stack.clear()
            interface_blocks_in_order.append(nested_interface_desc)
    
    return interface_blocks_in_order


def _extract_dummy_arg_names(proc_stmt: Union[Function_Stmt, Subroutine_Stmt]) -> List[str]:
    """Extract dummy argument names from procedure statement."""
    dummy_arg_names = []
    dummy_args = walk(proc_stmt, Dummy_Arg_List)
    if dummy_args:
        for dummy_arg in walk(dummy_args, Name):
            dummy_arg_names.append(dummy_arg.string)
    return dummy_arg_names


def _match_interfaces_to_procedure_arguments(
    interface_blocks: List[InterfaceDescription],
    dummy_arg_names: List[str],
    parsed_arguments: Dict[str, Any]
) -> Tuple[Dict[str, InterfaceDescription], Dict[str, Dict[str, Any]]]:
    """Match interface blocks to procedure arguments positionally."""
    argument_interfaces = {}
    procedure_arguments = {}
    
    # Find which arguments are NOT covered by type declarations
    procedure_arg_names = [name for name in dummy_arg_names if name not in parsed_arguments["intent_in"]]
    
    # Match interface blocks to procedure arguments positionally
    for i, interface_desc in enumerate(interface_blocks):
        if i < len(procedure_arg_names):
            arg_name = procedure_arg_names[i]
            interface_proc_name = list(interface_desc["procedures"].keys())[0]
            
            argument_interfaces[arg_name] = interface_desc
            procedure_arguments[arg_name] = {
                "type": "PROCEDURE",
                "description": "",
                "dimension": None,
                "interface_name": interface_proc_name,
                "enum_type": None
            }
    
    return argument_interfaces, procedure_arguments


def _process_procedure_body(
    node: Union[Function_Body, Subroutine_Body],
    stmt_type: type,
    procedure_decls: List[Procedure_Declaration_Stmt]
) -> Dict[str, Any]:
    """Process function or subroutine body and extract all relevant information."""
    # Extract the procedure statement
    proc_stmt = walk(node, stmt_type)[0]
    dummy_arg_names = _extract_dummy_arg_names(proc_stmt)
    
    # Process body children sequentially
    procedure_comment_stack = []
    argument_decls = []
    
    for child in node.children:
        if isinstance(child, Comment):
            procedure_comment_stack.append(child)
        elif isinstance(child, Type_Declaration_Stmt):
            argument_decls.append(child)
        elif isinstance(child, Specification_Part):
            interface_blocks = _process_specification_part(child, argument_decls)
            break
    else:
        interface_blocks = []
    
    # Parse the procedure
    common = parse_procedure(proc_stmt, stmt_type, argument_decls, procedure_decls, procedure_comment_stack)
    
    # Match interfaces to arguments
    argument_interfaces, procedure_arguments = _match_interfaces_to_procedure_arguments(
        interface_blocks, dummy_arg_names, common
    )
    
    # Update common with procedure arguments
    common["intent_in"].update(procedure_arguments)
    
    # Rebuild arguments list in correct order
    ordered_arguments = []
    for arg_name in dummy_arg_names:
        if arg_name in common["intent_in"] or arg_name in common["intent_out"] or arg_name in procedure_arguments:
            ordered_arguments.append(arg_name)
    common["arguments"] = ordered_arguments
    
    return {
        "common": common,
        "argument_interfaces": argument_interfaces,
        "procedure_comment_stack": procedure_comment_stack
    }


def _extract_return_info(common: Dict[str, Any]) -> Tuple[str, Dict[str, Any]]:
    """Extract return type and return variable information for functions."""
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
    return_argument = None
    if return_variable in common["all_parsed_arguments"]:
        return_argument = common["all_parsed_arguments"][return_variable]
    
    if not return_argument:
        return_argument = {
            "description": "",
            "dimension": None,
            "enum_type": None,
            "interface_name": None,
            "type": return_type
        }
    
    return return_variable, return_argument

# def parse_interface(
#     interface: Interface_Block, comment_stack: List[Comment]
# ) -> Tuple[str, InterfaceDescription]:
#     description = format_comments(comment_stack) if is_doc4for_comment(comment_stack) else ""

#     # this should be a method
#     interface_stmt = walk(interface, Interface_Stmt)[0]
#     name, operator_symbol, attributes = "", None, []
#     generic_specs = walk(interface, Generic_Spec)
#     if generic_specs:
#         operators = walk(generic_specs, Extended_Intrinsic_Op)
#         if operators:
#             operator_symbol = operators[0].string
#         else:
#             defined_operators = walk(generic_specs, Defined_Op)
#             if defined_operators:
#                 operator_symbol = defined_operators[0].string
#             else:
#                 operator_symbol = generic_specs[0].children[1]        
#     else:
#         # get the name and interface type
#         if interface_stmt.children[0] == "ABSTRACT":
#             attributes.append("ABSTRACT")
#         else:
#             names = walk(interface_stmt, Name)
#             if names:
#                 name = names[0].string

#     module_procedures = {}
#     procedures = {}
#     body_comment_stack = []
#     argument_decls: List[Type_Declaration_Stmt] = []
#     procedure_decls: List[Procedure_Declaration_Stmt] = []

#     body_comment_stack = []
#     for node in interface.children:
#         binding_type = None
#         if isinstance(node, Comment):
#             body_comment_stack.append(node)
#         elif isinstance(node, Procedure_Stmt):
#             proc_names = walk(node, Name)
#             body_description = format_comments(body_comment_stack) if is_doc4for_comment(body_comment_stack) else ""
#             for proc_name in proc_names:
#                 module_procedures[proc_name.string] = {"name": proc_name.string, "description": body_description}
#             body_comment_stack.clear()
#         elif isinstance(node, Function_Body):
#             function_stmt = walk(node, Function_Stmt)[0]
            
#             # Extract dummy arguments from function statement early
#             dummy_args = walk(function_stmt, Dummy_Arg_List)
#             dummy_arg_names = []
#             if dummy_args:
#                 for dummy_arg in walk(dummy_args, Name):
#                     dummy_arg_names.append(dummy_arg.string)
            
#             # Process function body children sequentially
#             procedure_comment_stack = []
#             argument_interfaces = {}
#             argument_decls = []  # Collect only direct child declarations
#             procedure_arguments = {}  # Track procedure arguments
            
#             for child in node.children:
#                 if isinstance(child, Comment):
#                     procedure_comment_stack.append(child)
#                 elif isinstance(child, Type_Declaration_Stmt):
#                     argument_decls.append(child)
#                 elif isinstance(child, Specification_Part):
#                     spec_comment_stack = []
#                     interface_blocks_in_order = []  # Track interfaces by position
                    
#                     for spec_child in child.children:
#                         if isinstance(spec_child, Comment):
#                             spec_comment_stack.append(spec_child)
#                         elif isinstance(spec_child, Type_Declaration_Stmt):
#                             argument_decls.append(spec_child)
#                         elif isinstance(spec_child, Implicit_Part):
#                             for implicit_child in spec_child.children:
#                                 if isinstance(implicit_child, Comment):
#                                     spec_comment_stack.append(implicit_child)
#                         elif isinstance(spec_child, Interface_Block):
#                             _, nested_interface_desc = parse_interface(spec_child, spec_comment_stack)
#                             spec_comment_stack.clear() 
#                             interface_blocks_in_order.append(nested_interface_desc)
            
#             # Parse the main function
#             common = parse_procedure(function_stmt, Function_Stmt, argument_decls, procedure_decls, procedure_comment_stack)
                        
#             # Find which arguments are NOT covered by type declarations (these are procedure arguments)
#             procedure_arg_names = [name for name in dummy_arg_names if name not in common["intent_in"]]

#             # Match interface blocks to procedure arguments positionally
#             for i, interface_desc in enumerate(interface_blocks_in_order):
#                 if i < len(procedure_arg_names):
#                     arg_name = procedure_arg_names[i]
#                     interface_proc_name = list(interface_desc["procedures"].keys())[0]  # Get the procedure name from interface
                    
#                     argument_interfaces[arg_name] = interface_desc
#                     procedure_arguments[arg_name] = {
#                         "type": "PROCEDURE",
#                         "description": "",
#                         "dimension": None,
#                         "interface_name": interface_proc_name,
#                         "enum_type": None
#                     }            
#             # Add procedure arguments to common
#             common["intent_in"].update(procedure_arguments)            
#             # Rebuild arguments list in the correct order using dummy_arg_names
#             ordered_arguments = []
#             for arg_name in dummy_arg_names:
#                 if arg_name in common["intent_in"] or arg_name in common["intent_out"] or arg_name in procedure_arguments:
#                     ordered_arguments.append(arg_name)

#             common["arguments"] = ordered_arguments                        
#             return_type, return_variable = None, None
#             if common["prefixes"]:
#                 for prefix_node in common["prefixes"][0].children:
#                     if not isinstance(prefix_node, Prefix_Spec):
#                         if return_type is not None:
#                             logger.error(f"Found more than one return type for {common['procedure_name']}")
#                             continue
#                         return_type = prefix_node.string.upper() if isinstance(prefix_node, Intrinsic_Type_Spec) else prefix_node.string

#             suffixes = walk(common["procedure_declaration"], Suffix)
#             if suffixes:
#                 binding_type = _extract_binding_type(walk(suffixes, Language_Binding_Spec))
#                 return_variable = walk(suffixes, Name)[0].string
#             else:
#                 return_variable = common["procedure_name"]
#             # Handle return argument - it's been found in the declarations or is the function name
#             return_argument = None
#             if return_variable in common["all_parsed_arguments"]:
#                 return_argument = common["all_parsed_arguments"][return_variable]
            
#             if not return_argument:
#                 return_argument = {
#                     "description": "",
#                     "dimension": None,
#                     "enum_type": None,
#                     "interface_name": None,
#                     "type": return_type
#                 }
#             function_description = {
#                 "attributes": common["attributes"],
#                 "description": "",
#                 "arguments": common["arguments"],
#                 "in": common["intent_in"],
#                 "out": common["intent_out"],
#                 "return": return_argument,
#                 "argument_interfaces": argument_interfaces,
#                 "binding_type": binding_type
#             }
#             procedures[common["procedure_name"]] = function_description
#             update_arguments_with_comment_data(procedure_comment_stack, function_description)
#         elif isinstance(node, Subroutine_Body):
#             subroutine_stmt = walk(node, Subroutine_Stmt)[0]
            
#             # Extract dummy arguments from subroutine statement early
#             dummy_args = walk(subroutine_stmt, Dummy_Arg_List)
#             dummy_arg_names = []
#             if dummy_args:
#                 for dummy_arg in walk(dummy_args, Name):
#                     dummy_arg_names.append(dummy_arg.string)
            
#             # Process subroutine body children sequentially
#             procedure_comment_stack = []
#             argument_interfaces = {}
#             argument_decls = []  # Collect only direct child declarations
#             procedure_arguments = {}  # Track procedure arguments
            
#             for child in node.children:
#                 if isinstance(child, Comment):
#                     procedure_comment_stack.append(child)
#                 elif isinstance(child, Type_Declaration_Stmt):
#                     argument_decls.append(child)
#                 elif isinstance(child, Specification_Part):
#                     spec_comment_stack = []
#                     interface_blocks_in_order = []  # Track interfaces by position
                    
#                     for spec_child in child.children:
#                         if isinstance(spec_child, Comment):
#                             spec_comment_stack.append(spec_child)
#                         elif isinstance(spec_child, Type_Declaration_Stmt):
#                             argument_decls.append(spec_child)
#                         elif isinstance(spec_child, Implicit_Part):
#                             for implicit_child in spec_child.children:
#                                 if isinstance(implicit_child, Comment):
#                                     spec_comment_stack.append(implicit_child)
#                         elif isinstance(spec_child, Interface_Block):
#                             _, nested_interface_desc = parse_interface(spec_child, spec_comment_stack)
#                             spec_comment_stack.clear() 
#                             interface_blocks_in_order.append(nested_interface_desc)
            
#             # Parse the main function
#             common = parse_procedure(subroutine_stmt, Subroutine_Stmt, argument_decls, procedure_decls, procedure_comment_stack)
            
#             # Find which arguments are NOT covered by type declarations (these are procedure arguments)
#             procedure_arg_names = [name for name in dummy_arg_names if name not in common["intent_in"]]

#             # Match interface blocks to procedure arguments positionally
#             for i, interface_desc in enumerate(interface_blocks_in_order):
#                 if i < len(procedure_arg_names):
#                     arg_name = procedure_arg_names[i]
#                     interface_proc_name = list(interface_desc["procedures"].keys())[0]  # Get the procedure name from interface
                    
#                     argument_interfaces[arg_name] = interface_desc
#                     procedure_arguments[arg_name] = {
#                         "type": "PROCEDURE",
#                         "description": "",
#                         "dimension": None,
#                         "interface_name": interface_proc_name,
#                         "enum_type": None
#                     }            
#             # Add procedure arguments to common
#             common["intent_in"].update(procedure_arguments)            
#             # Rebuild arguments list in the correct order using dummy_arg_names
#             ordered_arguments = []
#             for arg_name in dummy_arg_names:
#                 if arg_name in common["intent_in"] or arg_name in common["intent_out"] or arg_name in procedure_arguments:
#                     ordered_arguments.append(arg_name)

#             common["arguments"] = ordered_arguments                        

#             suffixes = walk(common["procedure_declaration"], Suffix)
#             if suffixes:
#                 binding_type = _extract_binding_type(walk(suffixes, Language_Binding_Spec))

#             subroutine_description = {
#                 "attributes": common["attributes"],
#                 "description": "",
#                 "arguments": common["arguments"],
#                 "in": common["intent_in"],
#                 "out": common["intent_out"],
#                 "argument_interfaces": argument_interfaces,
#                 "binding_type": binding_type
#             }
#             procedures[common["procedure_name"]] = subroutine_description
#             update_arguments_with_comment_data(procedure_comment_stack, subroutine_description)

#     interface_description: InterfaceDescription = {
#         "description": description,
#         "attributes": attributes,
#         "procedures": procedures,
#         "module_procedures": module_procedures,
#         "name": name,
#         "operator_symbol": operator_symbol
#     }
#     return name, interface_description