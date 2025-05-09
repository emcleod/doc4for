import re
from typing import List, Tuple, Optional, Union
from fparser.one.block_statements import (
    Comment,
    Function,
    Subroutine,
    Interface,
    ModuleProcedure,
    EndInterface,
    SpecificBinding,
    Use,
    Import,
    External
)
from doc4for.models.procedure_models import (
    FunctionDescription, 
    SubroutineDescription, 
    InterfaceDescription,
)
from doc4for.models.common import (
    BindingType, 
    BindingTypeEnum
)
from doc4for.parse.procedure_argument_parser import (
    update_arguments_with_comment_data,
    update_arguments_with_parsed_data,
    handle_specific_binding
)
from doc4for.utils.comment_utils import is_doc4for_comment, format_comments

def parse_function(function: Function, comment_stack: List[Comment]) -> FunctionDescription:
    return parse_procedure(function, comment_stack, True)

def parse_subroutine(subroutine: Subroutine, comment_stack: List[Comment]) -> SubroutineDescription:
    return parse_procedure(subroutine, comment_stack, False)

def parse_procedure(
    procedure: Union[Function, Subroutine], 
    comment_stack: List[Comment],
    is_function: bool
) -> Union[FunctionDescription, SubroutineDescription]:
    """Generic procedure parser for both functions and subroutines."""
    attributes: List[str] = [
        attr.strip().lower() for attr in procedure.prefix.split() if attr.strip()
    ]
    
    # Initialize the description dictionary
    procedure_description = {
        "attributes": attributes,
        "description": "",
        "arguments": procedure.args,
        "in": {},
        "out": {},
        "argument_interfaces": {},
        "binding_type": None
    }
    
    # Add return field if this is a function
    if is_function:
        procedure_description["return"] = {}
    
    update_arguments_with_parsed_data(procedure, procedure_description)
    
    # Get list of procedure-type arguments 
    procedure_args = [
        arg_name for arg_name, arg_info in procedure_description["in"].items()
        if arg_info["type"] == "procedure"
    ]
    
    # Get any bindings
    procedure_description["binding_type"] = extract_binding_type(procedure.bind)
    
    # Process interfaces in procedure content
    interface_comment_stack = []
    interface_descriptions = {}  
    proc_arg_index = 0

    for item in procedure.content:
        match item:
            case Comment():
                if item.content:
                    interface_comment_stack.append(item)
            case Interface():
                interface_description = parse_interface(item, interface_comment_stack)
                
                # Check if this is an unnamed interface
                if interface_description["procedures"]:
                    # Use the first procedure name as the key
                    proc_name = next(iter(interface_description["procedures"]))
                    interface_descriptions[proc_name] = interface_description
                    
                    # If we have procedure arguments that haven't been processed yet
                    if proc_arg_index < len(procedure_args):
                        # For unnamed interfaces, we need to associate them with an argument
                        argument_name = procedure_args[proc_arg_index]
                        
                        # Set interface_name for the argument if not already set
                        if not procedure_description["in"][argument_name].get("interface_name"):
                            procedure_description["in"][argument_name]["interface_name"] = proc_name
                        
                        proc_arg_index += 1
                
                interface_comment_stack.clear()
            case SpecificBinding():
                handle_specific_binding(item, procedure_description, procedure_args)
            case Use():
                pass
            case Import():
                pass
            case External():
                pass
            case _:
                pass

    # Connect arguments to interfaces
    for arg_name, arg_info in procedure_description["in"].items():
        if arg_info.get("type") == "procedure" and arg_info.get("interface_name"):
            interface_name = arg_info["interface_name"]
            if interface_name in interface_descriptions:
                procedure_description["argument_interfaces"][arg_name] = interface_descriptions[interface_name]
            else:
                # Special case: if the interface name is the same as the argument name
                if interface_name == arg_name and arg_name in interface_descriptions:
                    procedure_description["argument_interfaces"][arg_name] = interface_descriptions[arg_name]   

    if is_doc4for_comment(comment_stack):
        update_arguments_with_comment_data(comment_stack, procedure_description)
                
    return procedure_description

def extract_binding_type(bind: Optional[List[str]]) -> Optional[BindingType]:
    """
    Convert fparser's binding attribute to our BindingType structure.
    
    Args:
        bind: The binding information from fparser, e.g., ['C', "NAME = 'c_square'"]
        
    Returns:
        A BindingType dict or None if no binding is specified
    """        
    binding_type = {
        'type': BindingTypeEnum.DEFAULT,
        'name': None
    }
    
    if not bind:
        return binding_type
    
    # Check for 'C' binding in any position in the list (case-insensitive)
    has_c_binding = any(param.upper() == 'C' for param in bind)
    
    if has_c_binding:
        binding_type['type'] = BindingTypeEnum.BIND_C
        
        # Look for name parameter in any position
        for param in bind:
            if 'NAME' in param.upper():
                # Extract the name string, handling different quote styles
                match = re.search(r"NAME\s*=\s*['\"](.+?)['\"]", param, re.IGNORECASE)
                if match:
                    binding_type['name'] = match.group(1)

    return binding_type

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
        # Check if we hit another Interface - this would indicate a parsing error where
        # fparser has incorrectly nested two separate interfaces
        if isinstance(item, Interface):
            # We've reached the start of the next interface that was incorrectly nested
            break
        
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

