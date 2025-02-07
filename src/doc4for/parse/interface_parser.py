import re
from typing import List, Tuple, Optional
from fparser.one.block_statements import Comment
from fparser.one.block_statements import Interface, Function, Subroutine
from doc4for.models.type_models import InterfaceDescription
from doc4for.utils.comment_utils import is_doc4for_comment, format_comments
from doc4for.parse.procedure_parser import parse_subroutine, parse_function


def parse_interface(
        interface: Interface,
        comment_stack: List[Comment]) -> InterfaceDescription:
    description = format_comments(comment_stack) if is_doc4for_comment(comment_stack) else ""
    attributes = ["abstract"] if interface.isabstract else []
    name, operator = parse_interface_type(interface.name)
    procedures = {}
    procedure_comment_stack = []

    for item in interface.content:
        match item:
            case Comment():
                procedure_comment_stack.append(item)
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
        "module_procedure_names": []
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
