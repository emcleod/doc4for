import re
from typing import List, Tuple, Optional
from fparser.one.block_statements import (
    Comment,
    Function,
    Subroutine,
    Interface,
    ModuleProcedure
)
from doc4for.models.procedure_models import (
    FunctionDescription, 
    SubroutineDescription, 
    InterfaceDescription,
    ModuleProcedureDescription
)
from doc4for.parse.procedure_argument_parser import (
    update_arguments_with_comment_data,
    update_arguments_with_parsed_data,
)
from doc4for.utils.comment_utils import is_doc4for_comment, format_comments

def parse_function(
    function: Function, comment_stack: List[Comment]
) -> FunctionDescription:
    attributes: List[str] = [
        attr.strip().lower() for attr in function.prefix.split() if attr.strip()
    ]
    function_description: FunctionDescription = {
        "attributes": attributes,
        "description": "",
        "arguments": function.args,
        "in": {},
        "out": {},
        "return": {},
        "argument_interfaces": {},
    }
    update_arguments_with_parsed_data(function, function_description)
    # Get list of procedure-type arguments 
    function_args = [
        arg_name for arg_name, arg_info in function_description["in"].items()
        if arg_info["type"] == "procedure"
    ]
    
    # Process interfaces in function content
    interface_comment_stack = []
    function_arg_index = 0
    
    for item in function.content:
        match item:
            case Comment():
                if item.content:
                    interface_comment_stack.append(item)
            case Interface():
                if function_arg_index < len(function_args):
                    interface_description = parse_interface(item, interface_comment_stack)
                    argument_name = function_args[function_arg_index]
                    
                    # Store the full interface description
                    function_description["argument_interfaces"][argument_name] = interface_description
                    
                    # Add reference to the interface in the argument
                    function_description["in"][argument_name]["interface_name"] = argument_name                    
                    function_arg_index += 1
                    
                interface_comment_stack.clear()
            case _:
                pass

    if is_doc4for_comment(comment_stack):
        update_arguments_with_comment_data(comment_stack, function_description)
                
    return function_description 

def parse_subroutine(
    subroutine: Subroutine, comment_stack: List[Comment]
) -> SubroutineDescription:
    attributes: List[str] = [
        attr.strip().lower() for attr in subroutine.prefix.split() if attr.strip()
    ]
    subroutine_description: SubroutineDescription = {
        "attributes": attributes,
        "description": "",
        "arguments": subroutine.args,
        "in": {},
        "out": {},
        "argument_interfaces": {}
    }
    update_arguments_with_parsed_data(subroutine, subroutine_description)
    # Get list of procedure-type arguments 
    subroutine_args = [
        arg_name for arg_name, arg_info in subroutine_description["in"].items()
        if arg_info["type"] == "procedure"
    ]
    
    # Process interfaces in subroutine content
    interface_comment_stack = []
    subroutine_arg_index = 0
    
    for item in subroutine.content:
        match item:
            case Comment():
                if item.content:
                    interface_comment_stack.append(item)
            case Interface():
                if subroutine_arg_index < len(subroutine_args):
                    interface_description = parse_interface(item, interface_comment_stack)
                    argument_name = subroutine_args[subroutine_arg_index]
                    
                    # Store the full interface description
                    subroutine_description["argument_interfaces"][argument_name] = interface_description
                    
                    # Add reference to the interface in the argument
                    subroutine_description["in"][argument_name]["interface_name"] = argument_name                    
                    subroutine_arg_index += 1
                    
                interface_comment_stack.clear()
            case _:
                pass

    if is_doc4for_comment(comment_stack):
        update_arguments_with_comment_data(comment_stack, subroutine_description)
                
    return subroutine_description 


def parse_interface(
        interface: Interface,
        comment_stack: List[Comment]) -> InterfaceDescription:
    description = format_comments(comment_stack) if is_doc4for_comment(comment_stack) else ""
    attributes = ["abstract"] if interface.isabstract else []
    name, operator = parse_interface_type(interface.name)
    procedures = {}
    procedure_comment_stack = []

    module_procedures = {}
    for item in interface.content:
        match item:
            case Comment():
                if item.content:
                    procedure_comment_stack.append(item)
            case ModuleProcedure():
                procedure_names = item.items
                procedure_description = format_comments(procedure_comment_stack) if is_doc4for_comment(procedure_comment_stack) else ""
                for procedure_name in procedure_names:
                    module_procedures[procedure_name] = { "name": procedure_name, "description": procedure_description}                
                procedure_comment_stack.clear()
            case Function():
                procedures[item.name] = parse_function(item, procedure_comment_stack)
                procedure_comment_stack.clear()
            case Subroutine():
                procedures[item.name] = parse_subroutine(item, procedure_comment_stack)
                procedure_comment_stack.clear()
            case _:
                pass

    interface_description: InterfaceDescription = {
        "description": description,
        "attributes": attributes,
        "procedures": procedures,
        "module_procedures": module_procedures
    }   
    if not interface.isabstract:
        interface_description["name"] = name
    if operator:
        interface_description["operator_symbol"] = operator
    return interface_description

def parse_interface_type(name: str) -> Tuple[Optional[str], Optional[str]]:
    """
    Parse the interface name to determine if it's abstract (no name) or an operator or assignment interface.
    
    Returns a tuple of (interface_type, operator_symbol)
    where interface_type is 'operator', 'assignment', or None for regular interfaces,
    and operator_symbol is the symbol for operator/assignment or None for regular interfaces.
    """
    if not name:
        return None, None
    if name.startswith('operator'):
        match = re.match(r'operator\((.*?)\)', name)
        if match:
            return 'operator', match.group(1)
    elif name.startswith('assignment'):
        return 'assignment', '='
    return name, None

