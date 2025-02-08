import logging
import re
from collections import defaultdict
from typing import List, Any, Tuple, Optional, Union
from fparser.one.block_statements import (
    Comment,
    Interface,
    SpecificBinding
)
from fparser.one.typedecl_statements import TypeDeclarationStatement
from doc4for.models.common import ANNOTATION_PREFIX, IGNORE_PREFIX, IGNORE_SUFFIX, ARGUMENT_PATTERN
from doc4for.models.procedure_models import FunctionDescription, SubroutineDescription, is_function_description, Argument
from doc4for.utils.comment_utils import format_comments
from doc4for.parse.variable_parser import parse_type_declaration_statement
from doc4for.models.dimension_models import Dimension, format_dimension

logger: logging.Logger = logging.getLogger(__name__)


def extract_argument_type(item: Any) -> str:
    """Get the type of a dummy argument to a function from its `TypeDeclarationStatement`.

    Args:
        item: The argument object to get the type from.

    Returns:
        str: The name of the argument type if available, or an empty string if not.
    """
    return item.name if item.name else ''


def determine_argument_intent(item: Any) -> Tuple[bool, bool]:
    """Get the intent of a dummy argument to a function.

    This function determines whether a dummy argument is intended as input,
    output, or both, based on the presence of `intent` attributes in its
    attribute specification. The standard in Fortran is:
        - intent(in): intended for input only
        - intent(out): intended for output, so might be changed
        - intent(inout): intended for input and output
        - <no declaration>: defaults to intent(inout)

    Args:
        item: The `TypeDeclarationStatement` 

    Returns:
        Tuple[bool, bool]: A tuple containing two boolean values:
            - The first value indicates whether the argument is intended as input
              (`True`) or not (`False`).
            - The second value indicates whether the argument is intended as output
              (`True`) or not (`False`).

    Examples:
        >>> determine_argument_intent(obj_with_intent_inout)
        (True, True)
        >>> determine_argument_intent(obj_with_intent_in)
        (True, False)
        >>> determine_argument_intent(obj_with_intent_out)
        (False, True)
        >>> determine_argument_intent(obj_without_intent)
        (True, True)
    """
    if hasattr(item, 'attrspec'):
        if 'intent(inout)' in item.attrspec:
            intentin, intentout = True, True
        else:
            intentin = 'intent(in)' in item.attrspec
            intentout = 'intent(out)' in item.attrspec
    else:
        intentin, intentout = True, True
    return intentin, intentout


def extract_return_type(function: Any) -> str:
    """Get the return type of a function.

    This function extracts the return type of a function from its type
    declaration (`typedecl` attribute). If the function does not have a
    type declaration, the string 'Unknown' is returned.

    Args:
        function: An object representing a function, which should have a
            `typedecl` attribute containing information about its return type.

    Returns:
        str: The name of the function's return type, or 'Unknown' if the
            function does not have a type declaration.

    Example:
        >>> extract_return_type(my_function_with_type_decl)
        'integer'
        >>> extract_return_type(my_function_without_type_decl)
        'Unknown'
    """
    return function.typedecl.name if function.typedecl else 'Unknown'


def format_dimension_string(dims: List[int]) -> str:
    """Generate a string representing the dimensions of an array.

    This function takes a list of integers representing the dimensions of an
    array and generates a string that can be used to display or describe the
    array's shape.

    Args:
        dims (List[int]): A list of integers representing the dimensions of the
            array. An empty list returns an empty string. Any empty strings in
            the list are assumed to represent an allocatable dimension.

    Returns:
        str: A string representing the dimensions of the array, with allocatable
            dimensions represented by the string 'allocatable' and fixed
            dimensions represented by their integer values. The dimensions are
            separated by the string ' &times; ' (space, ampersand, times,
            semicolon, space).

    Examples:
        >>> format_dimension_string([])
        ''
        >>> format_dimension_string([3])
        '3'
        >>> format_dimension_string([''])
        'allocatable'
        >>> format_dimension_string([2, 3, 5])
        '2 &times; 3 &times; 5'
    """
    if not dims:
        return ''
    return format_dimension(dims)

def update_single_argument_1(decl: str, arg_type: str, intentin: bool, intentout: bool,
                             dummy_arg_info: Union[FunctionDescription, SubroutineDescription],
                             dims: Optional[List[int]] = None) -> None:
    """Populate the `Argument` type with information from the dummy argument
    declaration.

    Args:
        decl (str): The name of the argument as found by the parser.
        arg_type (str): The type of the argument as found by the parser, e.g.,
            'real', 'integer'.
        intentin (bool): True if the argument is an input to the function
            (`intent(in)` or `intent(inout)`).
        intentout (bool): True if the argument is an output of the function
            (`intent(out)` or `intent(inout)`), excluding the return type.
        dummy_arg_info (Union[FunctionDescription, SubroutineDescription]): A dictionary 
            containing information about a function or subroutines's arguments, 
            The 'in' and 'out' keys will be populated in this function.
        dims (Optional[List[int]]): An optional list of integers representing the
            dimensions of the argument. An empty list or string are
            interpreted as allocatable dimensions. Defaults to an empty list if
            not provided.
    """
    if dims is None:
        dims = []
    arg_info: Argument = {'type': arg_type,
                          'description': '',
                          'dimension': format_dimension_string(dims)}
    if intentin or not intentout:
        dummy_arg_info['in'][decl] = arg_info
    if intentout or not intentin:
        dummy_arg_info['out'][decl] = arg_info


def update_single_argument(decl: str, arg_type: str, intentin: bool, intentout: bool,
                           dummy_arg_info: Union[FunctionDescription, SubroutineDescription],
                           dims: Optional[Dimension] = None) -> None:
    """Populate the `Argument` type with information from the dummy argument
    declaration.

    Args:
        decl (str): The name of the argument as found by the parser.
        arg_type (str): The type of the argument as found by the parser, e.g.,
            'real', 'integer'.
        intentin (bool): True if the argument is an input to the function
            (`intent(in)` or `intent(inout)`).
        intentout (bool): True if the argument is an output of the function
            (`intent(out)` or `intent(inout)`), excluding the return type.
        dummy_arg_info (Union[FunctionDescription, SubroutineDescription]): A dictionary 
            containing information about a function or subroutines's arguments, 
            The 'in' and 'out' keys will be populated in this function.
        dims (Optional[List[int]]): An optional list of integers representing the
            dimensions of the argument. An empty list or string are
            interpreted as allocatable dimensions. Defaults to an empty list if
            not provided.
    """
    if dims is None:
        dims = []
    arg_info: Argument = {'type': arg_type,
                          'description': '',
                          'dimension': format_dimension_string(dims)}
    if intentin or not intentout:
        dummy_arg_info['in'][decl] = arg_info
    if intentout or not intentin:
        dummy_arg_info['out'][decl] = arg_info


def update_arguments_with_parsed_data(procedure: Any, arg_info: Union[FunctionDescription, SubroutineDescription]) -> None:
    args: List[str] = procedure.args.copy()  # Copy of all argument names
    remaining_args = args.copy()  # Will remove args as we find their types
    result: str = procedure.result

        # Handle return type for functions first (if not using result variable)
    if is_function_description(arg_info) and hasattr(procedure, 'typedecl') and procedure.typedecl:
        variables = parse_type_declaration_statement(procedure.typedecl, "")
        # There should be only one variable - the function name
        if variables and variables[0]['name'] == procedure.name:
            arg_info['return'][procedure.name] = {
                'type': variables[0]['type'],
                'description': '',
                'dimension': format_dimension_string(variables[0]['dimension'])
            }

    # First pass: handle regular type declarations
    for item in procedure.content:
        match item:
            case TypeDeclarationStatement():
                variables = parse_type_declaration_statement(item, "")
                intentin, intentout = determine_argument_intent(item)

                for var in variables:
                    if var['name'] in args:
                        update_single_argument(
                            var['name'],
                            var['type'],
                            intentin,
                            intentout,
                            arg_info,
                            var['dimension']
                        )
                        # Remove from remaining_args as we've handled it
                        if var['name'] in remaining_args:
                            remaining_args.remove(var['name'])
                    elif is_function_description(arg_info) and var['name'] == result:
                        arg_info['return'][var['name']] = {
                            'type': var['type'],
                            'description': '',
                            'dimension': format_dimension_string(var['dimension'])
                        }
            case SpecificBinding():
                if item.name in args:
                    update_single_argument(
                        item.name,
                        'procedure',
                        True,  # assume intent(in) for procedure arguments
                        False,
                        arg_info,
                        None
                    )
                    arg_info['in'][item.name]["interface_name"] = item.iname
                    if item.name in remaining_args:
                        remaining_args.remove(item.name)
            case _:
                pass

    # Second pass: handle interface blocks for remaining arguments
    interface_index = 0
    for item in procedure.content:
        if isinstance(item, Interface):
            if interface_index < len(remaining_args):
                arg_name = remaining_args[interface_index]
                update_single_argument(
                    arg_name,
                    'procedure',
                    True,  # assume intent(in) for function arguments
                    False,
                    arg_info,
                    None
                )
                interface_index += 1

        
def extract_content_without_annotation(content: str) -> str:
    return ' '.join(content.split()[1:])

def extract_type_and_description(content: str) -> Tuple[str, str]:
    match = re.match(r'(\w+(?:\s*\([^)]+\))?(?:\s*\[[^\]]+\])?)\s*(.*)', content)
    if match:
        return match.group(1), match.group(2).strip()
    return '', content

def get_base_type(type_str: str) -> str:
    return type_str.split('(')[0].split('[')[0].strip().lower()

def extract_and_validate_type(description: str, expected_type: str) -> Tuple[Optional[str], str]:
    potential_type, new_description = extract_type_and_description(description)
    if potential_type and get_base_type(potential_type) == expected_type.lower():
        return potential_type, new_description
    return None, description

def update_with_argument_description(content: str,
                                   arg_info: Union[FunctionDescription, SubroutineDescription],
                                   annotation_types: List[str]) -> None:
    annotation_type = content.split()[0][1:]  # Remove the @ from @in/@out etc
    content_without_annotation = extract_content_without_annotation(content)

    argument_regex = re.compile(ARGUMENT_PATTERN, re.VERBOSE)
    match = argument_regex.match(content_without_annotation)
    if not match:
        logger.warning('Warning: Unexpected annotation format: %s', content)
        return

    var_name = match.group('var_name')
    description = match.group('description')

    # Check if the variable exists in any of the specified argument types
    if not any(var_name in arg_info[at] for at in annotation_types):
        logger.warning('Warning: "%s" annotation "%s" not found in arguments %s',
                      annotation_type, var_name, [list(arg_info[at].keys()) for at in annotation_types])
    else:
        # Update description for the variable in all relevant argument types
        for at in annotation_types:
            if var_name in arg_info[at]:
                arg_info[at][var_name]['description'] = description

def update_with_return_description(content: str, 
                                 arg_info: Union[FunctionDescription, SubroutineDescription]) -> None:
    if "return" not in arg_info:
        logger.warning('Warning: @return annotation found in a subroutine comment: %s', content)
        return
    
    content_without_annotation = extract_content_without_annotation(content)
    if not content_without_annotation:
        logger.warning('Warning: Not enough content in return annotation: %s', content)
        return

    # Just get the description (everything after @return)
    description = content_without_annotation.strip()
    
    # Update the description for the return value
    # (we know there's only one return value in Fortran)
    return_name = next(iter(arg_info["return"]))
    arg_info["return"][return_name]['description'] = description

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

def update_arguments_with_comment_data(comments: List[Comment], arg_info: Union[FunctionDescription, SubroutineDescription]) -> None:
    annotation_processors = defaultdict(
        lambda: lambda content, _: logging.warning("Unknown annotation type: %s", content.split()[0]), 
        {
        '@in': lambda content, info: update_with_argument_description(content, info, ['in']),
        '@out': lambda content, info: update_with_argument_description(content, info, ['out']),
        '@inout': lambda content, info: update_with_argument_description(content, info, ['in', 'out']),
        '@return': update_with_return_description
    })

    procedure_comment_stack = []
    for i, comment in enumerate(comments):
        content = comment.content.strip()

        if content.startswith(ANNOTATION_PREFIX):
            if procedure_comment_stack:
                arg_info['description'] += format_comments(procedure_comment_stack)
                procedure_comment_stack.clear()

            full_content, i = collect_continuation_lines(comments, i)
            content = ' '.join(full_content)
            annotation_type = content.split(maxsplit=1)[0].split(':')[0]
            annotation_processors[annotation_type](content, arg_info)
        else:
            procedure_comment_stack.append(comment)
    if procedure_comment_stack:
        arg_info['description'] += format_comments(procedure_comment_stack)