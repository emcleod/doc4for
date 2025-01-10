from typing import List, Dict, Any, Tuple, Optional, Callable, Union
from fparser.one.block_statements import (
    Comment
)
from fparser.one.typedecl_statements import TypeDeclarationStatement
from doc4for.models.common import ANNOTATION_PREFIX, IGNORE_PREFIX, IGNORE_SUFFIX
from doc4for.models.procedure_models import FunctionDescription, SubroutineDescription, is_function_description, Argument 
from doc4for.utils.comment_utils import format_comment_for_html

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
    dimension_parts = ['allocatable' if not dim else str(dim) for dim in dims]
    return ' &times; '.join(dimension_parts)

def update_single_argument(decl: str, arg_type: str, intentin: bool, intentout: bool,
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

def update_arguments_with_parsed_data(procedure: Any, arg_info: Union[FunctionDescription, SubroutineDescription]) -> None:
    """Process the arguments and return type of a function.

    This function extracts information about the arguments and return type of a
    given function. It iterates through the content of the function and processes
    each `TypeDeclarationStatement` to determine the types, intent, and dimensions
    of the arguments. It also handles the return type of the function.

    Args:
        procedure: A procedure (function or subroutine) extracted by the parser.
        arg_info (Union[FunctionDescription, SubroutineDescription)]: A dictionary 
        containing information about the function or subroutine's arguments and
        return type for functions.
        It populates the following keys:
            - 'in': A dictionary to store input argument information.
            - 'out': A dictionary to store output argument information.
            - 'return': A dictionary to store the return type information if the input is a 
            FunctionDescription.

    Returns:
        None: The function modifies the `arg_info` dictionary in-place.

    TODO:
        - Handle assumed size arrays.
        - Properly handle the dimensions of arguments and return types.
    """
    args: List[str] = procedure.args
    result: str = procedure.result
    for item in procedure.content:
        if isinstance(item, TypeDeclarationStatement):
            arg_type = extract_argument_type(item)
            intentin, intentout = determine_argument_intent(item)
            for decl in item.entity_decls:
                # TODO handle assumed size arrays
                if not ':' in decl:  # it's a scalar or assumed size
                    if decl in args:
                        update_single_argument(decl, arg_type, intentin, intentout, arg_info)
                    elif is_function_description(arg_info) and decl == result:
                        # TODO sort out dimension
                        arg_info['return'][decl] = {'type': arg_type, 'description': '', 'dimension': ''}
                else:
                    name, dimensions = decl.split('(')
                    dim = dimensions[:-1].split(':')
                    if name in args:
                        update_single_argument(name, arg_type, intentin, intentout, arg_info, dim)
                    elif is_function_description(arg_info) and name == result:
                        # TODO sort out dimension
                        arg_info['return'][decl] = {'type': arg_type, 'description': '', 'dimension': ''}

    if is_function_description(arg_info) and not arg_info['return']:
        return_type = extract_return_type(procedure)
        # TODO sort out dimension
        arg_info['return'][result] = {'type': return_type, 'description': '', 'dimension': ''}

def update_with_annotation_description(parts: List[str], arg_info: Union[FunctionDescription, SubroutineDescription], 
                       annotation_types: List[str]) -> None:
    """Process an annotation comment and update the corresponding argument or return type description.

    This function processes an annotation comment and updates the 'description' field of the
    corresponding argument or return type in the `arg_info` dictionary.

    Args:
        parts (List[str]): A list of parts from the annotation comment.
        arg_info (Union[FunctionDescription, SubroutineDescription]): A dictionary containing 
        information about the function or subroutines's arguments and return type.
        annotation_types (List[str]): A list of annotation types (e.g., 'in', 'out', 'return') to
            process.

    Returns:
        None: The function modifies the `arg_info` dictionary in-place.
    """
    if isinstance(arg_info, dict) and 'return' in arg_info and parts[0] == '@return':
        if len(arg_info['return']) > 1:
            print(f'Warning: more than one @return annotation found: {parts[0]} {arg_info["return"]}')
        next(iter(arg_info['return'].values()))['description'] = ' '.join(parts[3:])
        return
    arg_name, annotation_type = parts[1].rstrip(':'), parts[2]
    comment_annotation_type = parts[0][1:]  # Remove '@' prefix
    if not any(arg_name in arg_info[annotation_type] for annotation_type in annotation_types):
        #TODO this error message is wrong
        print(f'Warning: "{comment_annotation_type}" annotation "{arg_name}" found that is not present in arguments {[arg_info[t].keys() for t in annotation_types]}')
    else:
        for annotation_type in annotation_types:
            if arg_name in arg_info[annotation_type]:
                if comment_annotation_type != annotation_type:
                    print(f'Warning: "{comment_annotation_type}" annotation "{arg_name}" type "{annotation_type}" does not match value in arguments "{arg_info[annotation_type][arg_name]}"')
                arg_desc = ' '.join(parts[3:])
                arg_info[annotation_type][arg_name]['description'] = arg_desc

def update_arguments_with_comment_data(comments: List[Comment], arg_info: Union[FunctionDescription, SubroutineDescription]) -> None:
    """Populate the description field of arguments and return type using annotations in comments.

    This function processes a list of comments and looks for annotations starting with
    '@' followed by 'in', 'out', 'inout', or 'return'. The annotations are used to populate
    the 'description' field of the corresponding arguments or return type in the `arg_info`
    dictionary.

    Args:
        comments (List[Comment]): A list of Comment objects containing the comments to be processed.
        arg_info (Union[FunctionDescription, SubroutineDescription]): A dictionary containing information 
        about the function or subroutines's arguments and return type if it is a function.

    Returns:
        None: The function modifies the `arg_info` dictionary in-place.

    """
    annotation_processors: Dict[str, Callable] = {
        '@in': lambda parts, info: update_with_annotation_description(parts, info, ['in']),
        '@out': lambda parts, info: update_with_annotation_description(parts, info, ['out']),
        '@inout': lambda parts, info: update_with_annotation_description(parts, info, ['in', 'out']),
        '@return': lambda parts, info: update_with_annotation_description(parts, info, ['return'])
    }

    for comment in comments:
        content = comment.content.strip()
        if content.startswith(ANNOTATION_PREFIX):
            parts = content.split()
            annotation_type = parts[0]
            if annotation_type in annotation_processors:
                annotation_processors[annotation_type](parts, arg_info)
        elif not content.startswith(IGNORE_PREFIX) and not content.endswith(IGNORE_SUFFIX):
            arg_info['description'] += format_comment_for_html(content)
