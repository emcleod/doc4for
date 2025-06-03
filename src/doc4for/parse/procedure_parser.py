import re
import logging
from typing import List, Any, Tuple, Optional, Union, Dict
from collections import defaultdict
from fparser.two.Fortran2003 import (
    Function_Stmt,
    Function_Subprogram,
    Subroutine_Subprogram,
    Subroutine_Stmt,
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
    Dummy_Arg_List,
    Language_Binding_Spec
)
from fparser.two.utils import walk
from doc4for.models.procedure_models import (
    FunctionDescription, 
    SubroutineDescription, 
    InterfaceDescription,
)
from doc4for.models.common import ANNOTATION_PREFIX, IGNORE_PREFIX, IGNORE_SUFFIX
from doc4for.models.procedure_models import FunctionDescription, SubroutineDescription
from doc4for.utils.comment_utils import format_comments
from doc4for.parse.argument_parser import parse_arguments
from doc4for.models.common import UNKNOWN
from doc4for.parse.common_parser import _extract_binding_type
from doc4for.utils.comment_utils import format_comments

logger: logging.Logger = logging.getLogger(__name__)


def parse_function(function: Function_Subprogram, comment_stack: List[Comment]) -> Tuple[str, FunctionDescription]:
    common = _parse_procedure(function, Function_Stmt, comment_stack)
    if common is None:
        return None
    
    # Function-specific: handle return type and return variable
    return_type, return_variable = None, None
    if common["prefixes"]:
        for node in common["prefixes"][0].children:
            if not isinstance(node, Prefix_Spec):
                if return_type is not None:
                    logger.error(f"Found more than one return type for {common['procedure_name']}")
                    continue
                return_type = node.string.upper() if isinstance(node, Intrinsic_Type_Spec) else node.string
    
    # Function-specific: handle suffixes
    suffixes = walk(common["procedure_declaration"], Suffix)
    binding_type = None
    if suffixes:
        binding_type = _extract_binding_type(walk(suffixes, Language_Binding_Spec))
        return_variable = walk(suffixes, Name)[0].string
    else:
        return_variable = common["procedure_name"]
    
    # Function-specific: handle return argument
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
        "argument_interfaces": {},
        "binding_type": binding_type,
        "return": return_argument
    }
    
    _update_arguments_with_comment_data(comment_stack, function_description)
    return common["procedure_name"], function_description


def parse_subroutine(subroutine: Subroutine_Subprogram, comment_stack: List[Comment]) -> Tuple[str, SubroutineDescription]:
    common = _parse_procedure(subroutine, Subroutine_Stmt, comment_stack)
    if common is None:
        return None
    
    # Subroutine-specific: only need binding type, no return handling
    binding_type = _extract_binding_type(walk(common["procedure_declaration"], Language_Binding_Spec))
    
    subroutine_description = {
        "attributes": common["attributes"],
        "description": "",
        "arguments": common["arguments"],
        "in": common["intent_in"],
        "out": common["intent_out"],
        "argument_interfaces": {},
        "binding_type": binding_type
    }
    
    _update_arguments_with_comment_data(comment_stack, subroutine_description)
    return common["procedure_name"], subroutine_description

def _parse_procedure(procedure, stmt_type, comment_stack: List[Comment]) -> Dict:
    """
    Extract common parsing logic for functions and subroutines.
    Returns a dictionary with all the common elements.
    """
    # accumulate comment stack before declaration
    for node in procedure.children:
        if isinstance(node, Comment):
            comment_stack.append(node)
        else:
            break
    
    # only one declaration
    procedure_declaration = walk(procedure, stmt_type)[0]
    # only one name
    procedure_name = walk(procedure_declaration, Name)[0].string
    
    # process prefixes
    attributes = []
    prefixes = walk(procedure_declaration, Prefix)
    if len(prefixes) > 1:
        logger.error(f"Have more than one Prefix in {prefixes}")
        return None
    if prefixes:
        for node in prefixes[0].children:
            if isinstance(node, Prefix_Spec):
                attributes.append(node.string)
    
    # extract dummy argument names
    dummy_args = walk(procedure_declaration, Dummy_Arg_List)
    dummy_arg_names = []
    if dummy_args:
        for dummy_arg in walk(dummy_args, Name):
            dummy_arg_names.append(dummy_arg.string)
    
    # process declarations
    declarations = walk(procedure, Type_Declaration_Stmt)
    arguments = []
    intent_in = {}
    intent_out = {}
    all_parsed_arguments = {}
    
    for decl in declarations:
        parsed_arguments, intent = parse_arguments(decl)
        all_parsed_arguments.update(parsed_arguments)
        
        # filter to keep only the actual arguments
        dummy_arguments = {name: var for name, var in parsed_arguments.items() if name in dummy_arg_names}
        if intent == 'IN':
            intent_in.update(dummy_arguments)
        if intent == 'OUT':
            intent_out.update(dummy_arguments)
        if intent == 'INOUT' or not intent:
            intent_in.update(dummy_arguments)
            intent_out.update(dummy_arguments)
        arguments.extend(dummy_arguments.keys())
    
    return {
        "procedure_name": procedure_name,
        "procedure_declaration": procedure_declaration,
        "attributes": attributes,
        "arguments": arguments,
        "intent_in": intent_in,
        "intent_out": intent_out,
        "all_parsed_arguments": all_parsed_arguments,
        "prefixes": prefixes
    }


def _update_arguments_with_comment_data(comments: List[Comment], 
                                       arg_info: Union[FunctionDescription, SubroutineDescription]) -> None:
    annotation_processors = defaultdict(
        lambda: lambda content, _: logging.warning("Unknown annotation type: %s", content.split()[0]), 
        {
        "@in": lambda content, info: _update_with_argument_description(content, info, ["in"]),
        "@out": lambda content, info: _update_with_argument_description(content, info, ["out"]),
        "@inout": lambda content, info: _update_with_argument_description(content, info, ["in", "out"]),
        "@return": _update_with_return_description
    })

    procedure_comment_stack = []
    #TODO warning if there's a return annotation and it's a subroutine
    has_return_annotation = False
    for i, comment in enumerate(comments):
        content = comment.item.comment.strip()
        if content.startswith('!'):
            content = content[1:].strip()
        if content.startswith(ANNOTATION_PREFIX):
            if procedure_comment_stack:
                arg_info["description"] += format_comments(procedure_comment_stack)
                procedure_comment_stack.clear()
            # Track if we've seen a @return annotation
            if content.startswith("@return"):
                has_return_annotation = True
            content, i = _collect_continuation_lines(comments, i)
            annotation_type = content.split(maxsplit=1)[0].split(":")[0]
            annotation_processors[annotation_type](content, arg_info)
        else:
            procedure_comment_stack.append(comment)
    if procedure_comment_stack:
        arg_info["description"] += format_comments(procedure_comment_stack)
    # Check if we have a function without @return annotation
    if "return" in arg_info and not has_return_annotation:
        logger.warning("Warning: no annotation for return in function")

def _update_with_argument_description(content: str,
                                   arg_info: Union[FunctionDescription, SubroutineDescription],
                                   annotation_types: List[str]) -> None:
    annotation_type = content.split()[0][1:]  # Remove the @ from @in/@out etc
    content_without_annotation = extract_content_without_annotation(content)

    # Pattern for argument with description
    argument_with_desc_regex = re.compile(r'(?P<var_name>\w+)\s+(?P<description>.+)', re.VERBOSE)
    # Pattern for argument without description
    argument_only_regex = re.compile(r'^(?P<var_name>\w+)$', re.VERBOSE)
        
    match_with_desc = argument_with_desc_regex.match(content_without_annotation)
    match_only = argument_only_regex.match(content_without_annotation)

    if match_with_desc:
        var_name = match_with_desc.group("var_name")
        description = match_with_desc.group("description")
    elif match_only:
        var_name = match_only.group("var_name")
        description = ""
        # Don't warn about missing description yet - check if argument exists first
    else:
        logger.warning("Warning: Invalid annotation format: %s", content)
        return

    # Check if the variable exists FIRST
    if not any(var_name in arg_info[at] for at in annotation_types):
        logger.warning("Warning: '%s' annotation '%s' not found in arguments %s",
                    annotation_type, var_name, [list(arg_info[at].keys()) for at in annotation_types])
        return  # Don't continue processing if argument doesn't exist

    # Now warn about missing description if applicable
    if match_only:
        logger.warning("Warning: No description provided for argument '%s' in annotation: %s", 
                    var_name, content)

    # Update description for the variable in all relevant argument types
    for at in annotation_types:
        if var_name in arg_info[at]:
            arg_info[at][var_name]["description"] = description

def _update_with_return_description(content: str, 
                                 arg_info: Union[FunctionDescription, SubroutineDescription]) -> None:
    if "return" not in arg_info:
        logger.warning("Warning: @return annotation found in a subroutine comment: %s", content)
        return
    
    content_without_annotation = extract_content_without_annotation(content)
    if not content_without_annotation:
        logger.warning("Warning: No description provided for 'return' annotation")
        return

    # Just get the description (everything after @return)
    description = content_without_annotation.strip()
    arg_info["return"]["description"] = description

def _collect_continuation_lines(comments: List[Comment], start_index: int) -> tuple[str, int]:
    full_content = [clean_comment_content(comments[start_index])]
    for i in range(start_index + 1, len(comments)):
        next_content = clean_comment_content(comments[i])
        if (not next_content.startswith(ANNOTATION_PREFIX) and
            not next_content.startswith(IGNORE_PREFIX) and
            not next_content.endswith(IGNORE_SUFFIX)):
            full_content.append(next_content)
        else:
            return " ".join(full_content), i
    return " ".join(full_content), len(comments)

#TODO is this already in comment_utils - if not, move it
def clean_comment_content(comment: Comment) -> str:
    content = comment.item.comment.strip()
    if content.startswith('!'):
        content = content[1:].strip()
    return content

def extract_content_without_annotation(content: str) -> str:
    return " ".join(content.split()[1:])


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

