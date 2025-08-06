import re
import logging
from typing import List, Union, Dict, Optional, Any
from collections import defaultdict
from fparser.two.Fortran2003 import (
    Name,
    Comment,
    Prefix_Spec,
    Prefix,
    Name,
    Dummy_Arg_List, # type: ignore[attr-defined]
    Type_Declaration_Stmt,
    Procedure_Declaration_Stmt,
    Use_Stmt,
    Import_Stmt,
    External_Stmt, 
    Part_Ref,
    Section_Subscript_List, # type: ignore[attr-defined]
    Specification_Part,
    Interface_Block,
    Interface_Stmt
)
from fparser.two.utils import walk
from doc4for.models.procedure_models import (
    FunctionDescription, 
    SubroutineDescription, 
)
from doc4for.models.common import ANNOTATION_PREFIX, IGNORE_PREFIX, IGNORE_SUFFIX
from doc4for.models.procedure_models import FunctionDescription, SubroutineDescription
from doc4for.models.variable_models import PolymorphismType
from doc4for.utils.comment_utils import format_comments
from doc4for.parse.argument_parser import parse_arguments, parse_procedure_argument
from doc4for.parse.uses_parser import parse_uses_list, parse_imports_list
from doc4for.parse.interface_helper import match_interfaces_to_procedure_arguments

logger: logging.Logger = logging.getLogger(__name__)


#TODO look at differing types passed in from interface_parser and function_parser
def parse_procedure(procedure, 
                    stmt_type, 
                    type_decls: List[Type_Declaration_Stmt], 
                    procedure_decls: List[Procedure_Declaration_Stmt], 
                    external_decls: List[External_Stmt],
                    comment_stack: List[Comment],
                    default_access: Optional[str]) -> Optional[Dict[str, Any]]:
    # accumulate comment stack before declaration
    for node in procedure.children:
        if isinstance(node, Comment):
            comment_stack.append(node)
        else:
            break
    
    # only one declaration
    procedure_stmt = walk(procedure, stmt_type)[0]
    # only one name
    procedure_name = walk(procedure_stmt, Name)[0].string
    
    # process prefixes
    attributes = []
    prefixes = walk(procedure_stmt, Prefix)
    if len(prefixes) > 1:
        logger.error(f"Have more than one Prefix in {prefixes}")
        return None
    if prefixes:
        for node in prefixes[0].children:
            if isinstance(node, Prefix_Spec):
                attributes.append(node.string)
    
    # extract dummy argument names
    dummy_args = walk(procedure_stmt, Dummy_Arg_List)
    dummy_arg_names = []
    if dummy_args:
        for dummy_arg in walk(dummy_args, Name):
            dummy_arg_names.append(dummy_arg.string)
    
    # Parse interface blocks within the procedure
    interface_blocks = []
    spec_parts = walk(procedure, Specification_Part)
    if spec_parts:
        from doc4for.parse.interface_helper import process_specification_part
        interface_blocks, additional_type_decls, additional_proc_decls = process_specification_part(
            spec_parts[0], default_access
        )
        # Add any additional declarations found in the specification part
        if additional_type_decls:
            type_decls = type_decls + additional_type_decls if type_decls else additional_type_decls
        if additional_proc_decls:
            procedure_decls = procedure_decls + additional_proc_decls if procedure_decls else additional_proc_decls
    
    # process declarations
    intent_in = {}
    intent_out = {}
    all_parsed_arguments = {}
    
    # this will only have the interface names as keys - the interfaces will be 
    # filled in in a post-processing step when all modules have been processed
    argument_interfaces = {}
    external_procedures = {}
    
    if type_decls:        
        for decl in type_decls:
            parsed_arguments, intent = parse_arguments(decl)
            all_parsed_arguments.update(parsed_arguments)
            
            # filter to keep only the actual arguments
            dummy_arguments = {name: var for name, var in parsed_arguments.items() if name in dummy_arg_names}
            if intent == "IN":
                intent_in.update(dummy_arguments)
            if intent == "OUT":
                intent_out.update(dummy_arguments)
            if intent == "INOUT" or not intent:
                intent_in.update(dummy_arguments)
                intent_out.update(dummy_arguments)
        
        for decl in procedure_decls:
            parsed_arguments, intent = parse_procedure_argument(decl)
            dummy_arguments = {name: var for name, var in parsed_arguments.items() if name in dummy_arg_names}
            intent_in.update(dummy_arguments)
            for _, dummy_argument in dummy_arguments.items():
                argument_interfaces[dummy_argument["interface_name"]] = {}
    else:
        # there were no specific declarations of the type, so the only thing that we
        # can do is to assume intent inout and fill in the name
        #
        for dummy_argument in dummy_arg_names:
            #TODO need to check if implicit none has been used
            type = "INTEGER" if dummy_argument.upper()[0] in "IJKLMN" else "REAL"
            argument = {
                "type": type,  
                "kind": "",
                "length": "", 
                "description": "",  # Will be filled in later
                "dimension": "",
                "attributes": [],
                "default_value": "",
                "interface_name": None,  
                "enum_type": None, 
                "polymorphism_type": PolymorphismType.NONE
            } 
            all_parsed_arguments[dummy_argument] = argument
            intent_in[dummy_argument] = argument
            intent_out[dummy_argument] = argument

    # TODO I think this relies on ordering if the external procedure is passed into a procedure
    # i.e. the external declaration overwrites the information in the section above
    if external_decls:
        for decl in external_decls:
            external_names = walk(decl, Name)
            for external_name in external_names:
                name_str = external_name.string
                if name_str in dummy_arg_names:
                    # This is a parameter - add to intent_in as before
                    argument = {
                        "type": "PROCEDURE",  
                        "kind": "",
                        "length": "", 
                        "description": "",  # Will be filled in later
                        "dimension": "",
                        "attributes": attributes,
                        "default_value": "",
                        "interface_name": None,  
                        "enum_type": None, 
                        "polymorphism_type": PolymorphismType.NONE
                    } 
                    intent_in[name_str] = argument
                procedure_type = determine_procedure_type(procedure, name_str, all_parsed_arguments)
                external_procedures[name_str] = {
                    "name": name_str,
                    "procedure_type": procedure_type
                }

    # Match interface blocks to procedure arguments
    # This needs to happen after all arguments are parsed
    matched_interfaces, procedure_arguments = match_interfaces_to_procedure_arguments(
        interface_blocks, dummy_arg_names, {"intent_in": intent_in, "intent_out": intent_out}, procedure_decls
    )
    
    # Update argument_interfaces with matched interfaces
    argument_interfaces.update(matched_interfaces)
    
    # Update intent_in with any procedure arguments identified by interface matching
    intent_in.update(procedure_arguments)
    
    # Rebuild arguments list in correct order if new procedure arguments were added
    dummy_arg_names = []
    for arg_name in walk(procedure_stmt, Dummy_Arg_List)[0].children if dummy_args else []:
        if isinstance(arg_name, Name):
            dummy_arg_names.append(arg_name.string)

    uses = parse_uses_list(walk(procedure, Use_Stmt))
    imports = parse_imports_list(walk(procedure, Import_Stmt))
    
    # Apply default access if no explicit access is specified
    if default_access and "PUBLIC" not in attributes and "PRIVATE" not in attributes:
        attributes.append(default_access)
    
    return {
        "procedure_name": procedure_name,
        "procedure_declaration": procedure_stmt,
        "attributes": attributes,
        "arguments": dummy_arg_names, # keep the order of the arguments
        "intent_in": intent_in,
        "intent_out": intent_out,
        "all_parsed_arguments": all_parsed_arguments,
        "prefixes": prefixes,
        "argument_interfaces": argument_interfaces,
        "uses": uses,
        "imports": imports,
        "external_procedures": external_procedures
    }


def update_arguments_with_comment_data(comments: List[Comment], 
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

def determine_procedure_type(procedure, external_name, all_parsed_arguments):
    # First check if there's a type declaration for this external
    if external_name in all_parsed_arguments:
        arg_info = all_parsed_arguments[external_name]
        if arg_info.get("type") and arg_info["type"] != "PROCEDURE":
            # Has a type declaration (REAL, INTEGER, etc.) - it's a function
            return "FUNCTION"
    
    # Check if it's used as a function (appears in Part_Ref)
    for part_ref in walk(procedure, Part_Ref):
        names = walk(part_ref, Name)
        if names and names[0].string == external_name:
            # Check if it has a Section_Subscript_List (parentheses)
            if walk(part_ref, Section_Subscript_List):
                return "FUNCTION"
    
    # If it's not a function, it's a subroutine
    return "SUBROUTINE"

