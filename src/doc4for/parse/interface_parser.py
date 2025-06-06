import re
import logging
from typing import List, Any, Tuple, Optional, Union, Dict
from collections import defaultdict
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
    Dummy_Arg_List,
    Procedure_Declaration_Stmt
)
from fparser.two.utils import walk
from doc4for.models.procedure_models import (
    FunctionDescription,
    SubroutineDescription,
    InterfaceDescription,
)
from doc4for.utils.comment_utils import format_comments
from doc4for.parse.procedure_parser import parse_procedure, update_arguments_with_comment_data
from doc4for.parse.common_parser import _extract_binding_type
from doc4for.utils.comment_utils import format_comments, is_doc4for_comment

logger: logging.Logger = logging.getLogger(__name__)


def parse_interface(
    interface: Interface_Block, comment_stack: List[Comment]
) -> Tuple[str, InterfaceDescription]:
    description = format_comments(comment_stack) if is_doc4for_comment(comment_stack) else ""

    # this should be a method
    interface_stmt = walk(interface, Interface_Stmt)[0]
    name, operator_symbol, attributes = "", None, []
    generic_specs = walk(interface, Generic_Spec)
    if generic_specs:
        operators = walk(generic_specs, Extended_Intrinsic_Op)
        if operators:
            operator_symbol = operators[0].string
        else:
            defined_operators = walk(generic_specs, Defined_Op)
            if defined_operators:
                operator_symbol = defined_operators[0].string
            else:
                operator_symbol = generic_specs[0].children[1]        
    else:
        # get the name and interface type
        if interface_stmt.children[0] == "ABSTRACT":
            attributes.append("ABSTRACT")
        else:
            names = walk(interface_stmt, Name)
            if names:
                name = names[0].string

    module_procedures = {}
    procedures = {}
    body_comment_stack = []
    argument_decls: List[Type_Declaration_Stmt] = []
    procedure_decls: List[Procedure_Declaration_Stmt] = []

    body_comment_stack = []
    for node in interface.children:
        binding_type = None
        if isinstance(node, Comment):
            body_comment_stack.append(node)
        elif isinstance(node, Procedure_Stmt):
            proc_names = walk(node, Name)
            body_description = format_comments(body_comment_stack) if is_doc4for_comment(body_comment_stack) else ""
            for proc_name in proc_names:
                module_procedures[proc_name.string] = {"name": proc_name.string, "description": body_description}
            body_comment_stack.clear()
        elif isinstance(node, Function_Body):
            function_stmt = walk(node, Function_Stmt)[0]
            
            # Extract dummy arguments from function statement early
            dummy_args = walk(function_stmt, Dummy_Arg_List)
            dummy_arg_names = []
            if dummy_args:
                for dummy_arg in walk(dummy_args, Name):
                    dummy_arg_names.append(dummy_arg.string)
            
            # Process function body children sequentially
            procedure_comment_stack = []
            argument_interfaces = {}
            argument_decls = []  # Collect only direct child declarations
            procedure_arguments = {}  # Track procedure arguments
            
            for child in node.children:
                if isinstance(child, Comment):
                    procedure_comment_stack.append(child)
                elif isinstance(child, Type_Declaration_Stmt):
                    argument_decls.append(child)
                elif isinstance(child, Specification_Part):
                    spec_comment_stack = []
                    interface_blocks_in_order = []  # Track interfaces by position
                    
                    for spec_child in child.children:
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

                    # spec_comment_stack = []
                    # for spec_child in child.children:
                    #     if isinstance(spec_child, Comment):
                    #         spec_comment_stack.append(spec_child)
                    #     elif isinstance(spec_child, Type_Declaration_Stmt):
                    #         argument_decls.append(spec_child)
                    #     elif isinstance(spec_child, Implicit_Part):
                    #         for implicit_child in spec_child.children:
                    #             if isinstance(implicit_child, Comment):
                    #                 spec_comment_stack.append(implicit_child)
                    #     elif isinstance(spec_child, Interface_Block):
                    #         _, nested_interface_desc = parse_interface(spec_child, spec_comment_stack)
                    #         spec_comment_stack.clear() 
                    #         interface_procedure_names = nested_interface_desc["procedures"].keys()
                    #         for interface_procedure_name in interface_procedure_names:
                    #             # Check against dummy_arg_names instead of common["arguments"]
                    #             if interface_procedure_name in dummy_arg_names:
                    #                 argument_interfaces[interface_procedure_name] = nested_interface_desc
                    #                 # Track this as a procedure argument
                    #                 procedure_arguments[interface_procedure_name] = {
                    #                     "type": "PROCEDURE",
                    #                     "description": "",  # Will be filled by update_arguments_with_comment_data
                    #                     "dimension": None, #TODO
                    #                     "interface_name": interface_procedure_name,
                    #                     "enum_type": None
                    #                 }
            
            # Parse the main function
            common = parse_procedure(function_stmt, Function_Stmt, argument_decls, procedure_decls, procedure_comment_stack)
                        
            # Find which arguments are NOT covered by type declarations (these are procedure arguments)
            procedure_arg_names = [name for name in dummy_arg_names if name not in common["intent_in"]]

            # Match interface blocks to procedure arguments positionally
            for i, interface_desc in enumerate(interface_blocks_in_order):
                if i < len(procedure_arg_names):
                    arg_name = procedure_arg_names[i]
                    interface_proc_name = list(interface_desc["procedures"].keys())[0]  # Get the procedure name from interface
                    
                    argument_interfaces[arg_name] = interface_desc
                    procedure_arguments[arg_name] = {
                        "type": "PROCEDURE",
                        "description": "",
                        "dimension": None,
                        "interface_name": interface_proc_name,
                        "enum_type": None
                    }            
            # Add procedure arguments to common
            common["intent_in"].update(procedure_arguments)            
            # Rebuild arguments list in the correct order using dummy_arg_names
            ordered_arguments = []
            for arg_name in dummy_arg_names:
                if arg_name in common["intent_in"] or arg_name in common["intent_out"] or arg_name in procedure_arguments:
                    ordered_arguments.append(arg_name)

            common["arguments"] = ordered_arguments                        
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
                binding_type = _extract_binding_type(walk(suffixes, Language_Binding_Spec))
                return_variable = walk(suffixes, Name)[0].string
            else:
                return_variable = common["procedure_name"]
            # Handle return argument - it's been found in the declarations or is the function name
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
            function_description = {
                "attributes": common["attributes"],
                "description": "",
                "arguments": common["arguments"],
                "in": common["intent_in"],
                "out": common["intent_out"],
                "return": return_argument,
                "argument_interfaces": argument_interfaces,
                "binding_type": binding_type
            }
            procedures[common["procedure_name"]] = function_description
            update_arguments_with_comment_data(procedure_comment_stack, function_description)
        elif isinstance(node, Subroutine_Body):
            subroutine_stmt = walk(node, Subroutine_Stmt)[0]
            
            # Extract dummy arguments from subroutine statement early
            dummy_args = walk(subroutine_stmt, Dummy_Arg_List)
            dummy_arg_names = []
            if dummy_args:
                for dummy_arg in walk(dummy_args, Name):
                    dummy_arg_names.append(dummy_arg.string)
            
            # Process subroutine body children sequentially
            procedure_comment_stack = []
            argument_interfaces = {}
            argument_decls = []  # Collect only direct child declarations
            procedure_arguments = {}  # Track procedure arguments
            
            for child in node.children:
                if isinstance(child, Comment):
                    procedure_comment_stack.append(child)
                elif isinstance(child, Type_Declaration_Stmt):
                    argument_decls.append(child)
                elif isinstance(child, Specification_Part):
                    spec_comment_stack = []
                    for spec_child in child.children:
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
                            interface_procedure_names = nested_interface_desc["procedures"].keys()
                            for interface_procedure_name in interface_procedure_names:
                                # Check against dummy_arg_names instead of common["arguments"]
                                if interface_procedure_name in dummy_arg_names:
                                    argument_interfaces[interface_procedure_name] = nested_interface_desc
                                    # Track this as a procedure argument
                                    procedure_arguments[interface_procedure_name] = {
                                        "type": "PROCEDURE",
                                        "description": "",  # Will be filled by update_arguments_with_comment_data
                                        "dimension": None, #TODO
                                        "interface_name": interface_procedure_name,
                                        "enum_type": None
                                    }
            
            # Parse the main function
            common = parse_procedure(subroutine_stmt, Subroutine_Stmt, argument_decls, procedure_decls, procedure_comment_stack)
            
            # Add procedure arguments to common
            common["intent_in"].update(procedure_arguments)            
            # Rebuild arguments list in the correct order using dummy_arg_names
            ordered_arguments = []
            for arg_name in dummy_arg_names:
                if arg_name in common["intent_in"] or arg_name in common["intent_out"] or arg_name in procedure_arguments:
                    ordered_arguments.append(arg_name)

            common["arguments"] = ordered_arguments                        

            suffixes = walk(common["procedure_declaration"], Suffix)
            if suffixes:
                binding_type = _extract_binding_type(walk(suffixes, Language_Binding_Spec))

            subroutine_description = {
                "attributes": common["attributes"],
                "description": "",
                "arguments": common["arguments"],
                "in": common["intent_in"],
                "out": common["intent_out"],
                "argument_interfaces": argument_interfaces,
                "binding_type": binding_type
            }
            procedures[common["procedure_name"]] = subroutine_description
            update_arguments_with_comment_data(procedure_comment_stack, subroutine_description)

    interface_description: InterfaceDescription = {
        "description": description,
        "attributes": attributes,
        "procedures": procedures,
        "module_procedures": module_procedures,
        "name": name,
        "operator_symbol": operator_symbol
    }
    return name, interface_description




# def parse_interface(
#         interface: Interface,
#         comment_stack: List[Comment]) -> InterfaceDescription:
#     description = format_comments(comment_stack) if is_doc4for_comment(comment_stack) else ""
#     attributes = ["abstract"] if interface.isabstract else []
#     name, operator = parse_interface_type(interface.name)
#     procedures = {}
#     procedure_comment_stack = []
#     module_procedures = {}

#     for item in interface.content:
#         # Check if we hit another Interface - this would indicate a parsing error where
#         # fparser has incorrectly nested two separate interfaces
#         if isinstance(item, Interface):
#             # We've reached the start of the next interface that was incorrectly nested
#             break

#         match item:
#             case Comment():
#                 if item.content:
#                     procedure_comment_stack.append(item)
#             case ModuleProcedure():
#                 procedure_names = item.items
#                 procedure_description = format_comments(procedure_comment_stack) if is_doc4for_comment(procedure_comment_stack) else ""
#                 for procedure_name in procedure_names:
#                     module_procedures[procedure_name] = { "name": procedure_name, "description": procedure_description}
#                 procedure_comment_stack.clear()
#             case Function():
#                 procedures[item.name] = parse_function(item, procedure_comment_stack)
#                 procedure_comment_stack.clear()
#             case Subroutine():
#                 procedures[item.name] = parse_subroutine(item, procedure_comment_stack)
#                 procedure_comment_stack.clear()
#             case _:
#                 pass

#     interface_description: InterfaceDescription = {
#         "description": description,
#         "attributes": attributes,
#         "procedures": procedures,
#         "module_procedures": module_procedures
#     }
#     if not interface.isabstract:
#         interface_description["name"] = name
#     if operator:
#         interface_description["operator_symbol"] = operator
#     return interface_description


# def parse_interface_type(name: str) -> Tuple[Optional[str], Optional[str]]:
#     """
#     Parse the interface name to determine if it's abstract (no name) or an operator or assignment interface.

#     Returns a tuple of (interface_type, operator_symbol)
#     where interface_type is 'operator', 'assignment', or None for regular interfaces,
#     and operator_symbol is the symbol for operator/assignment or None for regular interfaces.
#     """
#     if not name:
#         return None, None
#     if name.startswith('operator'):
#         match = re.match(r'operator\((.*?)\)', name)
#         if match:
#             return 'operator', match.group(1)
#     elif name.startswith('assignment'):
#         return 'assignment', '='
#     return name, None


