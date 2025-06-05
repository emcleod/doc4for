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
    Specification_Part
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
    argument_decls = []

    body_comment_stack = []
    for node in interface.children:
        binding_type = None
        if isinstance(node, Comment):
            body_comment_stack.append(node)
        elif isinstance(node, Procedure_Stmt):
            proc_names = walk(node, Name)
            body_description = format_comments(body_comment_stack) if is_doc4for_comment(body_comment_stack) else ""
            for proc_name in proc_names:
                module_procedures[proc_name.string] = {"name": proc_name.string,
                                             "description": body_description}
            body_comment_stack.clear()
        elif isinstance(node, Function_Body):
            # only go down one level so we're not picking up any comments for nested interfaces
            procedure_comment_stack = []
            for child in node.children:
                if isinstance(child, Comment):
                    procedure_comment_stack.append(child)
            function_stmt = walk(node, Function_Stmt)[0] 
            argument_decls = walk(node, Type_Declaration_Stmt)
            common = parse_procedure(function_stmt, Function_Stmt, argument_decls, procedure_comment_stack)
            # Handle return type and return variable
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
            argument_interfaces = {}
            nested_interfaces = walk(node, Interface_Block)
            for nested_interface in nested_interfaces:
                _, nested_interface_desc = parse_interface(nested_interface, procedure_comment_stack)
                # find the procedures in the common arguments and replace the type and interface name
                interface_function_names = nested_interface_desc["procedures"].keys()
                in_arguments = common["intent_in"]
                out_arguments = common["intent_out"]
                for interface_function_name in interface_function_names:
                    if interface_function_name in in_arguments:
                        in_arguments[interface_function_name]["type"] = "PROCEDURE"
                        in_arguments[interface_function_name]["interface_name"] = interface_function_name
                        argument_interfaces[interface_function_name] = nested_interface_desc
                    if interface_function_name in out_arguments:
                        del out_arguments[interface_function_name]
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
            procedure_comment_stack = walk(node, Comment)
            subroutine_stmt = walk(node, Subroutine_Stmt)[0] 
            argument_decls = walk(node, Type_Declaration_Stmt)
            common = parse_procedure(subroutine_stmt, Subroutine_Stmt, argument_decls, procedure_comment_stack)
            suffixes = walk(common["procedure_declaration"], Suffix)
            if suffixes:
                binding_type = _extract_binding_type(walk(suffixes, Language_Binding_Spec))
            
            subroutine_description = {
                "attributes": common["attributes"],
                "description": "",
                "arguments": common["arguments"],
                "in": common["intent_in"],
                "out": common["intent_out"],
                "argument_interfaces": None,
                "binding_type": binding_type
            }
            procedures[common["procedure_name"]] = subroutine_description
            update_arguments_with_comment_data(procedure_comment_stack, subroutine_description)
            procedure_comment_stack.clear()

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


