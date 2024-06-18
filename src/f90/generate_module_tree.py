""" Module for processing Fortran 90 modules and comments and generating a static webpage from their contents.

"""

import os
import html
import re
import shutil
import tempfile
import errno
import time
import random
from typing import List, Dict, Any, Tuple, Optional, TypedDict, Callable
from fparser.api import parse as fortran_parser  # type: ignore
from fparser.one.block_statements import (
    Module,
    Comment,
    Function,
)
from fparser.one.typedecl_statements import TypeDeclarationStatement
from jinja2 import Environment, FileSystemLoader

ANNOTATION_PREFIX = '@'
"""Prefix used to identify annotations in comments."""

"""Prefix used to mark the start of a section to be ignored."""
IGNORE_PREFIX = '!*'

IGNORE_SUFFIX = '*!'
"""Suffix used to mark the end of a section to be ignored."""

Argument = TypedDict('Argument', {
    'type': str,
    'description': str,
    'dimension': Optional[str]
})
"""
Represents an argument of a Fortran 90 function.

Fields:
    type (str): The data type of the argument e.g. real(kind=8)
    description (str): A description of the argument's purpose, read from the comments 
    dimension (Optional[str]): The dimension of a vector or matrix argument, if applicable.
"""

FunctionDescription = TypedDict('FunctionDescription', {
    'attributes': List[str],
    'description': str,
    'in': Dict[str, Argument],
    'out': Dict[str, Argument],
    'return': Dict[str, Argument]
})
"""
Describes a Fortran 90 function's attributes, arguments and return values, including any
description in the comments.

Fields:
    attributes (List[str]): A list of function attributes e.g. pure, elemental, etc.
    description (str): A description of the function's purpose.
    in (Dict[str, Argument]): The input (intent(in) or intent(inout) arguments to a function.
    out (Dict[str, Argument]): The output (intent(out) or intent(inout)) arguments to a function.
    return (Dict[str, Argument]): The return value ('result') of a function
"""

FunctionDetails = TypedDict('FunctionDetails', {
    'details': FunctionDescription
})
"""
Contains detailed information about a Fortran 90 function.

Fields:
    details (FunctionDescription): Detailed description of the function.
"""

ModuleData = TypedDict('ModuleData', {
    'module_name': str,
    'constants': Dict[str, Any],
    'functions': Dict[str, FunctionDetails],
    'subroutines': Dict[str, Any],
    'file_name': str,
    'module_description': str
})
"""
Represents data for a module.

Fields:
    module_name (str): The name of the module.
    constants (Dict[str, Any]): All public constants defined in the module.
    functions (Dict[str, FunctionDetails]): All public functions in the module.
    subroutines (Dict[str, Any]): All public subroutines in the module.
    file_name (str): The name of the file containing the module.
    module_description (str): A description of the module's purpose.
"""

def find_f90_files(directory: str) -> List[str]:
    """
    Finds all Fortran 90 files (*.f90) in a directory and its subdirectories.

    Args:
        directory (str): The path to the top-level directory
    Returns:
        A list of relative paths to any fortran files that have been found.

    """
    f90_files: List[str] = []
    for root, _, files in os.walk(directory):
        for file in files:
            if file.endswith('.f90'):
                relative_path = os.path.relpath(os.path.join(root, file), directory)
                f90_files.append(relative_path)
    return f90_files

def process_comment(comment: str) -> str:
    """Process a comment string to escape HTML entities and replace code blocks.

    Args:
        comment (str): The input comment string to be processed.

    Returns:
        str: The processed comment string with HTML entities escaped and code blocks
            wrapped in `<code>` tags.

    Examples:
        >>> process_comment("This is a comment with <code>print('Hello, World!')</code>")
        'This is a comment with &lt;code&gt;print(&#39;Hello, World!&#39;)&lt;/code&gt;'

        >>> process_comment("This is a {code} block with {nested {blocks}}")
        'This is a <code>code</code> block with <code>nested {blocks}</code>'
    """
    return re.sub(r'{(.*?)}', r'<code>\1</code>', html.escape(comment))

def get_dummy_arg_type(item: Any) -> str:
    """Get the type of a dummy argument to a function from its `TypeDeclarationStatement`.

    Args:
        item: The argument object to get the type from.

    Returns:
        str: The name of the argument type if available, or an empty string if not.
    """
    return item.name if item.name else ''

def get_dummy_arg_intent(item: Any) -> Tuple[bool, bool]:
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
        >>> get_dummy_arg_intent(obj_with_intent_inout)
        (True, True)
        >>> get_dummy_arg_intent(obj_with_intent_in)
        (True, False)
        >>> get_dummy_arg_intent(obj_with_intent_out)
        (False, True)
        >>> get_dummy_arg_intent(obj_without_intent)
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

def add_dimension_info(dims: List[int]) -> str:
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
        >>> add_dimension_info([])
        ''
        >>> add_dimension_info([3])
        '3'
        >>> add_dimension_info([''])
        'allocatable'
        >>> add_dimension_info([2, 3, 5])
        '2 &times; 3 &times; 5'
    """
    if not dims:
        return ''
    dimension_parts = ['allocatable' if not dim else str(dim) for dim in dims]
    return ' &times; '.join(dimension_parts)

def populate_argument_info(decl: str, arg_type: str, intentin: bool, intentout: bool,
                dummy_arg_info: FunctionDescription, 
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
        dummy_arg_info (FunctionDescription): A dictionary containing information
            about the function's arguments, which will be populated in this function.
            The 'in' and 'out' keys will be populated in this function.
        dims (Optional[List[int]]): An optional list of integers representing the
            dimensions of the argument. An empty list or string are
            interpreted as allocatable dimensions. Defaults to an empty list if
            not provided.
        Examples:
            >>> process_args()
    """
    if dims is None:
        dims = []
    arg_info: Argument = {'type': arg_type, 'description': '', 'dimension': add_dimension_info(dims)}
    if intentin or not intentout:
        dummy_arg_info['in'][decl] = arg_info
    if intentout or not intentin:
        dummy_arg_info['out'][decl] = arg_info

def get_return_type(function: Any) -> str:
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
        >>> get_return_type(my_function_with_type_decl)
        'integer'
        >>> get_return_type(my_function_without_type_decl)
        'Unknown'
    """
    return function.typedecl.name if function.typedecl else 'Unknown'

def populate_arguments(function: Any, arg_info: FunctionDescription) -> None:
    """Process the arguments and return type of a function.

    This function extracts information about the arguments and return type of a
    given function. It iterates through the content of the function and processes
    each `TypeDeclarationStatement` to determine the types, intent, and dimensions
    of the arguments. It also handles the return type of the function.

    Args:
        function: An `Function` extracted by the parser.
        arg_info (FunctionDescription): A dictionary containing information about
            the function's arguments and return type. It populates the following keys:
            - 'in': A dictionary to store input argument information.
            - 'out': A dictionary to store output argument information.
            - 'return': A dictionary to store the return type information.

    Returns:
        None: The function modifies the `arg_info` dictionary in-place.

    TODO:
        - Handle assumed size arrays.
        - Properly handle the dimensions of arguments and return types.
    """
    args: List[str] = function.args
    result: str = function.result
    for item in function.content:
        if isinstance(item, TypeDeclarationStatement):
            arg_type = get_dummy_arg_type(item)
            intentin, intentout = get_dummy_arg_intent(item)
            for decl in item.entity_decls:
                # TODO handle assumed size arrays
                if not ':' in decl:  # it's a scalar or assumed size
                    if decl in args:
                        populate_argument_info(decl, arg_type, intentin, intentout, arg_info)
                    elif decl == result:
                        # TODO sort out dimension
                        arg_info['return'][decl] = {'type': arg_type, 'description': '', 'dimension': ''}
                else:
                    name, dimensions = decl.split('(')
                    dim = dimensions[:-1].split(':')
                    if name in args:
                        populate_argument_info(name, arg_type, intentin, intentout, arg_info, dim)
                    elif name == result:
                        # TODO sort out dimension
                        arg_info['return'][decl] = {'type': arg_type, 'description': '', 'dimension': ''}

    if not arg_info['return']:
        return_type = get_return_type(function)
        # TODO sort out dimension
        arg_info['return'][result] = {'type': return_type, 'description': '', 'dimension': ''}

def populate_argument_description(comments: List[Comment], arg_info: FunctionDescription) -> None:
    """Populate the description field of arguments and return type using annotations in comments.

    This function processes a list of comments and looks for annotations starting with
    '@' followed by 'in', 'out', 'inout', or 'return'. The annotations are used to populate
    the 'description' field of the corresponding arguments or return type in the `arg_info`
    dictionary.

    Args:
        comments (List[Comment]): A list of Comment objects containing the comments to be processed.
        arg_info (FunctionDescription): A dictionary containing information about the function's
            arguments and return type.

    Returns:
        None: The function modifies the `arg_info` dictionary in-place.

    TODO:
        - Check that there isn't more than one return statement.
    """
    annotation_processors: Dict[str, Callable] = {
        '@in': lambda parts, info: process_annotation(parts, info, ['in']),
        '@out': lambda parts, info: process_annotation(parts, info, ['out']),
        '@inout': lambda parts, info: process_annotation(parts, info, ['in', 'out']),
        '@return': lambda parts, info: process_annotation(parts, info, ['return'])
    }

    for comment in comments:
        #TODO check that there isn't more than one return statement
        content = comment.content.strip()
        if content.startswith(ANNOTATION_PREFIX):
            parts = content.split()
            annotation_type = parts[0]
            if annotation_type in annotation_processors:
                annotation_processors[annotation_type](parts, arg_info)
        elif not content.startswith(IGNORE_PREFIX) and not content.endswith(IGNORE_SUFFIX):
            arg_info['description'] += process_comment(content)

def process_annotation(parts: List[str], arg_info: FunctionDescription, annotation_types: List[str]) -> None:
    """Process an annotation comment and update the corresponding argument or return type description.

    This function processes an annotation comment and updates the 'description' field of the
    corresponding argument or return type in the `arg_info` dictionary.

    Args:
        parts (List[str]): A list of parts from the annotation comment.
        arg_info (FunctionDescription): A dictionary containing information about the function's
            arguments and return type.
        annotation_types (List[str]): A list of annotation types (e.g., 'in', 'out', 'return') to
            process.

    Returns:
        None: The function modifies the `arg_info` dictionary in-place.
    """
    if parts[0] == '@return':
        if len(arg_info['return']) > 1:
            print(f'Warning: more than one @return annotation found: {parts[0]} {arg_info["return"]}')
        next(iter(arg_info['return'].values()))['description'] = ' '.join(parts[1:])
        return
    arg_name, annotation_type = parts[1].rstrip(':'), parts[2]
    comment_annotation_type = parts[0][1:]  # Remove '@' prefix
    if not any(arg_name in arg_info[annotation_type] for annotation_type in annotation_types):
        print(f'Warning: {comment_annotation_type} annotation {arg_name} found that is not present in arguments {[arg_info[t].keys() for t in annotation_types]}')
    else:
        for annotation_type in annotation_types:
            if arg_name in arg_info[annotation_type]:
                if comment_annotation_type != annotation_type:
                    print(f'Warning: "{comment_annotation_type}" annotation "{arg_name}" type "{annotation_type}" does not match value in arguments "{arg_info[annotation_type][arg_name]}"')
                arg_desc = ' '.join(parts[3:])
                arg_info[annotation_type][arg_name]['description'] = arg_desc

def process_modules(f90_files: List[str]) -> List[ModuleData]:
    modules: List[ModuleData] = []
    for f90_file in f90_files:
        comment_stack: List[Comment] = []
        tree: Any = fortran_parser(f90_file, ignore_comments=False)
        for child in tree.content:
            if isinstance(child, Comment):
                comment_stack.append(child)
            elif isinstance(child, Module):
                module_data: ModuleData = {
                    'module_name': child.name,
                    'constants': {},
                    'functions': {},
                    'subroutines': {},
                    'file_name': f90_file,
                    'module_description': ''
                }
                # collect module comments
                if (comment_stack and comment_stack[0].content.startswith('!*') and comment_stack[-1].content.endswith('*!')):
                    module_data['module_description'] = '\n'.join(
                        process_comment(comment.content) for comment in comment_stack[1:-1] if process_comment(comment.content)) + '\n'
                function_comments = []
                for item in child.content:
                    if isinstance(item, Comment):
                        function_comments.append(item)
                    elif isinstance(item, Function):
                        function_name = item.name
                        attributes: List[str] = [attr.strip().lower() for attr in item.prefix.split() if attr.strip()]
                        function_description: FunctionDescription = {
                            'attributes': attributes,
                            'description': '', 
                            'in': {}, 
                            'out': {}, 
                            'return': {}}
                        populate_arguments(item, function_description)

                        if function_comments:
                            populate_argument_description(function_comments, function_description)
                        module_data['functions'][function_name] = {
                            'details': function_description
                        }
                        function_comments = []  # Reset for next function
                    else:
                        function_comments = []  # Reset for next function

                modules.append(module_data)
                comment_stack = []  # Reset comment_stack for the next module
    return modules

def create_modules_directory(max_retries=5, base_delay=0.1):
    """
    Creates a 'modules' directory in the 'docs' directory if it doesn't exist.
    If it exists, clears its contents.

    The function performs the following actions:
    1. Attempts to create or clear the 'docs/modules' directory.
    2. If a race condition is detected, it retries the operation.

    Args:
        max_retries (int): Maximum number of retry attempts.
        base_delay (float): Base delay between retries, in seconds.

    Returns:
        None

    Raises:
        PermissionError: If the program doesn't have write permissions.
        OSError: If there's an issue creating the directory or removing its contents after all retries.
    """
    modules_dir = os.path.join('docs', 'modules')

    if not check_write_permissions(os.path.dirname(modules_dir)):
        raise PermissionError("No write permissions in the docs directory.")

    for attempt in range(max_retries):
        try:
            if not os.path.exists(modules_dir):
                os.makedirs(modules_dir)
            else:
                clear_directory(modules_dir)
            return  # Success, exit the function
        except FileExistsError:
            # Directory was created by another process after we checked
            continue
        except FileNotFoundError:
            # Directory was deleted by another process after we checked
            continue
        except OSError as e:
            if e.errno != errno.EEXIST:
                raise
        
        # If we get here, we need to retry
        time.sleep(base_delay * (2 ** attempt) * (random.random() + 0.5))

    # If we've exhausted all retries, make one last attempt and let any exceptions propagate
    if not os.path.exists(modules_dir):
        os.makedirs(modules_dir)
    else:
        clear_directory(modules_dir)

def clear_directory(directory):
    """
    Clears the contents of the given directory.

    Args:
        directory (str): Path to the directory to clear.

    Raises:
        OSError: If there's an issue removing the contents.
    """
    for item in os.listdir(directory):
        item_path = os.path.join(directory, item)
        if os.path.islink(item_path) or os.path.isfile(item_path):
            os.unlink(item_path)
        elif os.path.isdir(item_path):
            shutil.rmtree(item_path)

def check_write_permissions(path):
    """
    Check if the program has write permissions in the specified path.

    Args:
        path (str): The path to check for write permissions.

    Returns:
        bool: True if write permissions are available, False otherwise.
    """
    try:
        testfile = tempfile.TemporaryFile(dir=path)
        testfile.close()
    except (IOError, OSError):
        return False
    return True

def generate_module_pages(modules: List[ModuleData]):
    create_modules_directory()  # Assuming this function already exists

    env = Environment(loader=FileSystemLoader('templates'))
    template = env.get_template('module_template.html')
    module_names = [module['module_name'] for module in modules]

    # Generate module_index.html
    output = template.render(
        module_names=module_names,
        module_data={},
        content_data='Welcome to your modules!',
        is_index=True,
        relative_path=''
    )
    with open(os.path.join('docs', 'modules', 'module_index.html'), 'w', encoding='utf-8') as file:
        file.write(output)

    # Generate individual module pages
    for module in modules:
        output = template.render(
            module_names=module_names,
            module_data=module,
            content_data='',
            is_index=False,
            relative_path='../'
        )
        with open(os.path.join('docs', 'modules', f'{module["module_name"]}.html'), 'w', encoding='utf-8') as file:
            file.write(output)


# def generate_home_html(modules: List[ModuleData]):
#     template = env.get_template('module_template.html')

#     module_names = list(map(lambda module: module['module_name'], modules))
#     output = template.render(
#         module_names=module_names,
#         module_data=[],
#         content_data='Welcome to your modules!',
#     )

#     with open(os.path.join('docs', 'modules', 'module_index.html'), 'w', encoding='utf-8', ) as file:
#         file.write(output)


# def generate_module_html(modules: List[ModuleData]):
#     template = env.get_template('module_template.html')

#     module_names = list(map(lambda m: m['module_name'], modules))
#     for module in modules:
#         output = template.render(
#             module_names=module_names, module_data=module, content_data=''
#         )
#         with open(
#             os.path.join('docs', 'modules', f'{module["module_name"]}.html'), 'w', encoding='utf-8', 
#         ) as file:
#             file.write(output)


current_directory = os.getcwd()
fortran_files = find_f90_files(current_directory)
modules = process_modules(fortran_files)

create_modules_directory()
generate_module_pages(modules)
# generate_home_html(modules)
# generate_module_html(modules)
