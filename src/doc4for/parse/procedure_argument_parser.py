from typing import List, Dict, Any, Tuple, Optional, Callable, Union
from fparser.one.block_statements import (
    Comment
)
from fparser.one.typedecl_statements import TypeDeclarationStatement
from doc4for.models.common import ANNOTATION_PREFIX, IGNORE_PREFIX, IGNORE_SUFFIX
from doc4for.models.procedure_models import FunctionDescription, SubroutineDescription, is_function_description, Argument 
from doc4for.utils.comment_utils import format_comment_for_html
from doc4for.parse.variable_parser import parse_variables
from doc4for.models.dimension_models import Dimension, format_dimension

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
    # dimension_parts = ['allocatable' if not dim else str(dim) for dim in dims]
    # return ' &times; '.join(dimension_parts)

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
    args: List[str] = procedure.args
    result: str = procedure.result
    for item in procedure.content:
        if isinstance(item, TypeDeclarationStatement):
            # Use parse_variables to get detailed variable information
            variables = parse_variables(item, "", [])
            
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
                elif is_function_description(arg_info) and var['name'] == result:
                    # Update return information
                    arg_info['return'][var['name']] = {
                        'type': var['type'], 
                        'description': '', 
                        'dimension': format_dimension_string(var['dimension'])
                    }

    # If return info still not set, use the old method as fallback
    if is_function_description(arg_info) and not arg_info['return']:
        return_type = extract_return_type(procedure)
        arg_info['return'][result] = {'type': return_type, 'description': '', 'dimension': ''}

def update_with_argument_description(parts: List[str], content: str, 
                                     arg_info: Union[FunctionDescription, SubroutineDescription], 
                                     annotation_types: List[str]) -> None:
    words = content.split()
    
    # Remove the annotation type
    annotation_type = words[0][1:]  # Remove '@' prefix
    words = words[1:]
    
    if not words:
        print(f'Warning: No content after annotation type in: {content}')
        return
    
    var_name, var_type, description = None, None, None
    
    if ":" in words[0]:
        if words[0].endswith(":"): # @in x: real description
            var_name = words[0][:-1]
            var_type = words[1]
            description = ' '.join(words[2:])
        else: # @in x:real description
            var_name, var_type = words[0].split(":")
            description = ' '.join(words[1:])
    elif words[1] == ":": # @in x : real description
        var_name = words[0]
        var_type = words[2]
        description = ' '.join(words[3:])
    elif words[1].startswith(":"): # @in x :real description
        var_name = words[0]
        var_type = words[1][1:]
        description = ' '.join(words[2:])
    else:
        print(f'Warning: Unexpected annotation format: {content}')
        return
    
    if not any(var_name in arg_info[annotation_type] for annotation_type in annotation_types):
        print(f'Warning: "{annotation_type}" annotation "{var_name}" not found in arguments {[arg_info[t].keys() for t in annotation_types]}')
    else:
        for annotation_type in annotation_types:
            if var_name in arg_info[annotation_type]:
                arg_info[annotation_type][var_name]['description'] = description

def update_with_return_description(parts: List[str], content: str, arg_info: Union[FunctionDescription, SubroutineDescription]) -> None:
    # Early exit for subroutines
    if "return" not in arg_info:
        print(f"Warning: @return annotation found in a subroutine comment: {content}")
        return
    
    if len(parts) < 2:  # We need at least @return and a description
        print(f'Warning: Not enough parts in return annotation: {" ".join(parts)}')
        return
    
    words = content.split()
    return_name, return_type, description = None, None, None
    if ":" in words[0]:
        if words[0].endswith(":"): # @return: real description 
            return_type = parts[1]
            description = ' '.join(parts[2:])
        else:
            _, return_type = words[0].split(":")
            description = ' '.join(parts[1:])
    elif words[1] == ":": # @return : real description
        return_type = parts[2]
        description = ' '.join(parts[3:])
    elif words[1].startswith(":"): # @return :real description
        return_type = parts[1][1:]
        description = ' '.join(parts[2:])
    elif len(parts) >= 5 and parts[2] == ":": # @return res : real description
        return_name = parts[1]
        return_type = parts[3]
        description = ' '.join(parts[4:])
    elif len(parts) >= 4: 
        if parts[1].endswith(":"): # @return res: real description
            return_name = parts[1][:-1]
            return_type = parts[2]
            description = ' '.join(parts[3:])
        elif parts[2].startswith(":"): # @return res :real description
            return_name = parts[1]
            return_type = parts[2][1:]
            description = ' '.join(parts[3:])
        elif ":" in parts[1]: # return res:real description
            return_name, return_type = parts[1].split(":")
            description = ' '.join(parts[2:])
        else: # @return description 
            description = ' '.join(parts[1:])
    else: # catch @return description if the description is short
        description = ' '.join(parts[1:])
        
    if return_name:
        if return_name not in arg_info["return"]:
            print(f"Return name in documentation does not match that in code")
    else: # didn't put in a return name, can only return one thing so add the description to the existing data
        return_name = next(iter(arg_info["return"]))
    arg_info["return"][return_name]['description'] = description

def update_arguments_with_comment_data(comments: List[Comment], arg_info: Union[FunctionDescription, SubroutineDescription]) -> None:
    annotation_processors: Dict[str, Callable] = {
        '@in': lambda parts, content, info: update_with_argument_description(parts, content, info, ['in']),
        '@out': lambda parts, content, info: update_with_argument_description(parts, content, info, ['out']),
        '@inout': lambda parts, content, info: update_with_argument_description(parts, content, info, ['in', 'out']),
        '@return': lambda parts, content, info: update_with_return_description(parts, content, info)
    }

    i = 0
    while i < len(comments):
        content = comments[i].content.strip()
        
        if content.startswith(ANNOTATION_PREFIX):
            # Collect all continuation lines
            full_content = [content]
            j = i + 1
            while j < len(comments):
                next_content = comments[j].content.strip()
                if (not next_content.startswith(ANNOTATION_PREFIX) and
                    not next_content.startswith(IGNORE_PREFIX) and 
                    not next_content.endswith(IGNORE_SUFFIX)):
                    full_content.append(next_content.strip()) 
                    j += 1
                else:
                    break
            i = j  # Skip processed comments
            
            # Join all parts into a single line
            content = ' '.join(full_content)
            
            # Process the annotation
            annotation_type, *rest = content.split(maxsplit=1)
            if ":" in annotation_type:
                annotation_type, _ = annotation_type.split(":")
            
            parts = [annotation_type] + (rest[0].split() if rest else [])
            
            if annotation_type in annotation_processors:
                annotation_processors[annotation_type](parts, content, arg_info)
        elif not content.startswith(IGNORE_PREFIX) and not content.endswith(IGNORE_SUFFIX):
            arg_info['description'] += format_comment_for_html(content)
            i += 1
        else:
            i += 1

