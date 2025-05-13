import re
import logging
from typing import List, Any, Tuple, Optional, Union, Dict
from collections import defaultdict
from fparser.two.Fortran2003 import (
    Function_Stmt,
    Function_Subprogram,
    Name,
    Comment,
    Prefix_Spec,
    Prefix,
    Suffix,
    Name,
    Intrinsic_Type_Spec,
    Type_Declaration_Stmt,
    Dimension_Attr_Spec,
    Access_Spec,
    Attr_Spec,
    Entity_Decl,
    Dummy_Arg_List
)
from fparser.two.utils import walk
from doc4for.models.procedure_models import (
    FunctionDescription, 
    SubroutineDescription, 
    InterfaceDescription,
)
from doc4for.models.common import (
    BindingType, 
    BindingTypeEnum
)
from doc4for.models.common import ANNOTATION_PREFIX, IGNORE_PREFIX, IGNORE_SUFFIX, ARGUMENT_PATTERN
from doc4for.models.procedure_models import FunctionDescription, SubroutineDescription, is_function_description, Argument
from doc4for.utils.comment_utils import format_comments
from doc4for.parse.argument_parser import parse_arguments
from doc4for.parse.variable_parser import parse_variable
from doc4for.models.dimension_models import Dimension, format_dimension
from doc4for.models.common import UNKNOWN
from doc4for.parse.common_parser import _extract_type_info, _extract_dimension_info, _extract_entity_info
from doc4for.utils.comment_utils import is_doc4for_comment, format_comments

logger: logging.Logger = logging.getLogger(__name__)


def parse_function(function: Function_Subprogram, comment_stack: List[Comment]) -> Tuple[str, FunctionDescription]:
    # accumulate comment stack before declaration
    # don't use walk because there might be comments in the function body
    # that should be ignored
    for node in function.children:
        if isinstance(node, Comment):
            comment_stack.append(node)
        else:
            # only want those comments before the declaration
            break
    # only one declaration
    function_declaration = walk(function, Function_Stmt)[0]
    # only one name
    function_name = walk(function_declaration, Name)[0].string
    attributes, return_type, return_variable = [], None, None
    prefixes = walk(function_declaration, Prefix)
    if len(prefixes) > 1:
        logger.error(f"Have more than one Prefix in {prefixes}")
        return
    if prefixes:
        for node in prefixes[0].children:
            if isinstance(node, Prefix_Spec):
                attributes.append(node.string)
            else:
                if return_type is not None:
                    logger.error(f"Found more than one return type for {function_name}")
                    continue
                # for consistency with the way parameters and variables are upper-case
                return_type = node.string.upper() if isinstance(node, Intrinsic_Type_Spec) else node.string
    suffixes = walk(function_declaration, Suffix)
    if suffixes:
        # only have 1 in valid Fortran
        return_variable = walk(suffixes, Name)[0].string
    else:
        return_variable = function_name
    dummy_args = walk(function_declaration, Dummy_Arg_List)
    dummy_arg_names = []
    if dummy_args:
        for dummy_arg in walk(dummy_args, Name):
            dummy_arg_names.append(dummy_arg.string)
    declarations = walk(function, Type_Declaration_Stmt)

    arguments = []
    intent_in = {}
    intent_out = {}
    return_argument = None
    for decl in declarations:
        # this will grab everything that's declared, including things that aren't dummy arguments
        parsed_arguments, intent = parse_arguments(decl)
        # filter to keep only the actual arguments as the parser can't know if it's an
        # argument or an internal variable
        dummy_arguments = {name: var for name, var in parsed_arguments.items() if name in dummy_arg_names}
        if intent == 'IN':
            intent_in.update(dummy_arguments)
        if intent == 'OUT':
            intent_out.update(dummy_arguments)
        if intent == 'INOUT' or not intent:
            intent_in.update(dummy_arguments)
            intent_out.update(dummy_arguments) 
        arguments.extend(dummy_arguments.keys())
        if not return_argument and return_variable in parsed_arguments:
            return_argument = parsed_arguments.pop(return_variable)

    if not return_argument:
        return_argument = {
            "description": "",
            "dimension": None, 
            "enum_type": None,
            "interface_name": None,
            "type": return_type
        }
    function_description = {
        "attributes": attributes,
        "description": "",
        "arguments": arguments,
        "in": intent_in,
        "out": intent_out,
        "argument_interfaces": {},
        "binding_type": None,
        "return": return_argument
    }
    update_arguments_with_comment_data(comment_stack, function_description)
    return function_name, function_description    

def update_arguments_with_comment_data(comments: List[Comment], arg_info: Union[FunctionDescription, SubroutineDescription]) -> None:
    annotation_processors = defaultdict(
        lambda: lambda content, _: logging.warning("Unknown annotation type: %s", content.split()[0]), 
        {
        "@in": lambda content, info: update_with_argument_description(content, info, ["in"]),
        "@out": lambda content, info: update_with_argument_description(content, info, ["out"]),
        "@inout": lambda content, info: update_with_argument_description(content, info, ["in", "out"]),
        "@return": update_with_return_description
    })

    procedure_comment_stack = []
    for i, comment in enumerate(comments):
        content = comment.item.comment.strip()

        if content.startswith(ANNOTATION_PREFIX):
            if procedure_comment_stack:
                arg_info["description"] += format_comments(procedure_comment_stack)
                procedure_comment_stack.clear()

            full_content, i = collect_continuation_lines(comments, i)
            content = " ".join(full_content)
            annotation_type = content.split(maxsplit=1)[0].split(":")[0]
            annotation_processors[annotation_type](content, arg_info)
        else:
            procedure_comment_stack.append(comment)
    if procedure_comment_stack:
        arg_info["description"] += format_comments(procedure_comment_stack)

def update_with_argument_description(content: str,
                                   arg_info: Union[FunctionDescription, SubroutineDescription],
                                   annotation_types: List[str]) -> None:
    annotation_type = content.split()[0][1:]  # Remove the @ from @in/@out etc
    content_without_annotation = extract_content_without_annotation(content)

    argument_regex = re.compile(ARGUMENT_PATTERN, re.VERBOSE)
    match = argument_regex.match(content_without_annotation)
    if not match:
        #TODO need to distinguish between having no description e.g. '@in x' and the format being completely wrong
        logger.warning("Warning: Unexpected annotation format: %s", content)
        return

    var_name = match.group("var_name")
    description = match.group("description")

    # Check if the variable exists in any of the specified argument types
    if not any(var_name in arg_info[at] for at in annotation_types):
        logger.warning("Warning: '%s' annotation '%s' not found in arguments %s",
                      annotation_type, var_name, [list(arg_info[at].keys()) for at in annotation_types])
    else:
        # Update description for the variable in all relevant argument types
        for at in annotation_types:
            if var_name in arg_info[at]:
                arg_info[at][var_name]["description"] = description

def update_with_return_description(content: str, 
                                 arg_info: Union[FunctionDescription, SubroutineDescription]) -> None:
    if "return" not in arg_info:
        logger.warning("Warning: @return annotation found in a subroutine comment: %s", content)
        return
    
    content_without_annotation = extract_content_without_annotation(content)
    if not content_without_annotation:
        logger.warning("Warning: Not enough content in return annotation: %s", content)
        return

    # Just get the description (everything after @return)
    description = content_without_annotation.strip()
    
    # Update the description for the return value
    # (we know there"s only one return value in Fortran)
    return_name = next(iter(arg_info["return"]))
    arg_info["return"][return_name]["description"] = description

def collect_continuation_lines(comments: List[Comment], start_index: int) -> tuple[List[str], int]:
    full_content = [comments[start_index].content.strip()]
    for i in range(start_index + 1, len(comments)):
        next_content = comments[i].content.strip()
        if (not next_content.startswith(ANNOTATION_PREFIX) and
            not next_content.startswith(IGNORE_PREFIX) and
            not next_content.endswith(IGNORE_SUFFIX)):
            full_content.append(next_content)
        else:
            return full_content, i
    return full_content, len(comments)

def extract_content_without_annotation(content: str) -> str:
    return " ".join(content.split()[1:])

# def parse_subroutine(subroutine: Subroutine, comment_stack: List[Comment]) -> SubroutineDescription:
#     return parse_procedure(subroutine, comment_stack, False)

# def parse_procedure(
#     procedure: Union[Function, Subroutine], 
#     comment_stack: List[Comment],
#     is_function: bool
# ) -> Union[FunctionDescription, SubroutineDescription]:
#     function_stmt = walk(procedure, Function_Stmt)

#     attributes: List[str] = []
#     # attributes: List[str] = [
#     #     attr.strip().lower() for attr in procedure.prefix.split() if attr.strip()
#     # ]
    
#     # Initialize the description dictionary
#     procedure_description = {
#         "attributes": attributes,
#         "description": "",
#         "arguments": procedure.args,
#         "in": {},
#         "out": {},
#         "argument_interfaces": {},
#         "binding_type": None
#     }
    
#     # Add return field if this is a function
#     if is_function:
#         procedure_description["return"] = {}
    
#     update_arguments_with_parsed_data(procedure, procedure_description)
    
#     # Get list of procedure-type arguments 
#     procedure_args = [
#         arg_name for arg_name, arg_info in procedure_description["in"].items()
#         if arg_info["type"] == "procedure"
#     ]
    
#     # Get any bindings
#     procedure_description["binding_type"] = extract_binding_type(procedure.bind)
    
#     # Process interfaces in procedure content
#     interface_comment_stack = []
#     interface_descriptions = {}  
#     proc_arg_index = 0

#     for item in procedure.content:
#         match item:
#             case Comment():
#                 if item.content:
#                     interface_comment_stack.append(item)
#             case Interface():
#                 interface_description = parse_interface(item, interface_comment_stack)
                
#                 # Check if this is an unnamed interface
#                 if interface_description["procedures"]:
#                     # Use the first procedure name as the key
#                     proc_name = next(iter(interface_description["procedures"]))
#                     interface_descriptions[proc_name] = interface_description
                    
#                     # If we have procedure arguments that haven't been processed yet
#                     if proc_arg_index < len(procedure_args):
#                         # For unnamed interfaces, we need to associate them with an argument
#                         argument_name = procedure_args[proc_arg_index]
                        
#                         # Set interface_name for the argument if not already set
#                         if not procedure_description["in"][argument_name].get("interface_name"):
#                             procedure_description["in"][argument_name]["interface_name"] = proc_name
                        
#                         proc_arg_index += 1
                
#                 interface_comment_stack.clear()
#             case SpecificBinding():
#                 handle_specific_binding(item, procedure_description, procedure_args)
#             case Use():
#                 pass
#             case Import():
#                 pass
#             case External():
#                 pass
#             case _:
#                 pass

#     # Connect arguments to interfaces
#     for arg_name, arg_info in procedure_description["in"].items():
#         if arg_info.get("type") == "procedure" and arg_info.get("interface_name"):
#             interface_name = arg_info["interface_name"]
#             if interface_name in interface_descriptions:
#                 procedure_description["argument_interfaces"][arg_name] = interface_descriptions[interface_name]
#             else:
#                 # Special case: if the interface name is the same as the argument name
#                 if interface_name == arg_name and arg_name in interface_descriptions:
#                     procedure_description["argument_interfaces"][arg_name] = interface_descriptions[arg_name]   

#     if is_doc4for_comment(comment_stack):
#         update_arguments_with_comment_data(comment_stack, procedure_description)
                
#     return procedure_description

# def extract_binding_type(bind: Optional[List[str]]) -> Optional[BindingType]:
#     """
#     Convert fparser's binding attribute to our BindingType structure.
    
#     Args:
#         bind: The binding information from fparser, e.g., ['C', "NAME = 'c_square'"]
        
#     Returns:
#         A BindingType dict or None if no binding is specified
#     """        
#     binding_type = {
#         'type': BindingTypeEnum.DEFAULT,
#         'name': None
#     }
    
#     if not bind:
#         return binding_type
    
#     # Check for 'C' binding in any position in the list (case-insensitive)
#     has_c_binding = any(param.upper() == 'C' for param in bind)
    
#     if has_c_binding:
#         binding_type['type'] = BindingTypeEnum.BIND_C
        
#         # Look for name parameter in any position
#         for param in bind:
#             if 'NAME' in param.upper():
#                 # Extract the name string, handling different quote styles
#                 match = re.search(r"NAME\s*=\s*['\"](.+?)['\"]", param, re.IGNORECASE)
#                 if match:
#                     binding_type['name'] = match.group(1)

#     return binding_type

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

