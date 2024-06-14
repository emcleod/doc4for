""" Module for processing Fortran 90 modules and comments and generating a static webpage from their contents.

"""

import os
from fparser.api import parse as fortran_parser  # type: ignore
from fparser.one.block_statements import (
    Module,
    Comment,
    Function,
)
from fparser.one.typedecl_statements import TypeDeclarationStatement
from jinja2 import Environment, FileSystemLoader
import html
import re
from typing import List, Dict, Any, Tuple, Optional, TypedDict, Callable

from typing import TypedDict, List, Dict, Optional, Any

"""Prefix used to identify annotations in comments."""
ANNOTATION_PREFIX = '@'

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

# TODO: this works for inline code, but will need to handle multi-line
# code blocks and surround them with <pre><code>
def process_comment(comment: str) -> str:
    return re.sub(r'{(.*?)}', r'<code>\1</code>', html.escape(comment))

def get_arg_type(item: Any) -> str:
    return item.name if item.name else ''

def get_arg_intent(item: Any) -> Tuple[bool, bool]:
    #TODO if no attrspec it defaults to inout
    if hasattr(item, 'attrspec'):
        intentin = 'intent(in)' in item.attrspec
        intentout = 'intent(out)' in item.attrspec
    else:
        intentin, intentout = False, False
    return intentin, intentout

def add_dimension_info(decl: str, dims: List[int]) -> str:
    if not dims:
        return ''
    dimension_parts = ['allocatable' if not dim else str(dim) for dim in dims]
    return ' &times; '.join(dimension_parts)

def process_arg(decl: str, arg_type: str, intentin: bool, intentout: bool,
                inputs: Dict[str, Argument], outputs: Dict[str, Argument], dims: List[int] = []) -> None:
    arg_info: Argument = {'type': arg_type, 'description': '', 'dimension': add_dimension_info(decl, dims)}
    if intentin or not intentout:
        inputs[decl] = arg_info.copy()
    if intentout or not intentin:
        outputs[decl] = arg_info.copy()

def get_return_type(function: Any) -> str:
    return function.typedecl.name if function.typedecl else 'Unknown'

def extract_arg_info(function: Any, arg_info: FunctionDescription) -> None:
    args: List[str] = function.args
    result: str = function.result
    for item in function.content:
        if isinstance(item, TypeDeclarationStatement):
            arg_type = get_arg_type(item)
            intentin, intentout = get_arg_intent(item)
            for decl in item.entity_decls:
                # TODO handle assumed size arrays
                if not ':' in decl:  # it's a scalar or assumed size
                    if decl in args:
                        process_arg(decl, arg_type, intentin, intentout, arg_info['in'], arg_info['out'])
                    elif decl == result:
                        # TODO sort out dimension
                        arg_info['return'][decl] = {'type': arg_type, 'description': '', 'dimension': ''}
                else:
                    name, dimensions = decl.split('(')
                    dim = dimensions[:-1].split(':')
                    if name in args:
                        process_arg(name, arg_type, intentin, intentout, arg_info['in'], arg_info['out'], dim)
                    elif name == result:
                        # TODO sort out dimension
                        arg_info['return'][decl] = {'type': arg_type, 'description': '', 'dimension': ''}

    if not arg_info['return']:
        return_type = get_return_type(function)
        # TODO sort out dimension
        arg_info['return'][result] = {'type': return_type, 'description': '', 'dimension': ''}

def process_function_comments(comments: List[Comment], arg_info: FunctionDescription) -> None:
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
    if parts[0] == '@return':
        if len(arg_info['return']) > 1:
            print(f'Warning: more than one @return annotation found')
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

def process_modules(f90_files: List[str]) -> List[Any]:
    modules = []
    for f90_file in f90_files:
        module_data = {}
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
                        extract_arg_info(item, function_description)

                        if function_comments:
                            process_function_comments(function_comments, function_description)
                        module_data['functions'][function_name] = {
                            'details': function_description
                        }
                        function_comments = []  # Reset for next function
                    else:
                        function_comments = []  # Reset if next item is not a function

                modules.append(module_data)
                module_data = {}  # Reset module_data for the next module
                comment_stack = []  # Reset comment_stack for the next module
    return modules


env = Environment(loader=FileSystemLoader('templates'))


def create_modules_directory():
    modules_dir = os.path.join('docs', 'modules')
    os.makedirs(modules_dir, exist_ok=True)

    for file in os.listdir(modules_dir):
        file_path = os.path.join(modules_dir, file)
        if os.path.isfile(file_path):
            os.remove(file_path)


def generate_home_html(modules):
    template = env.get_template('module_template.html')

    module_names = list(map(lambda module: module['module_name'], modules))
    output = template.render(
        module_names=module_names,
        module_data=[],
        content_data='Welcome to your modules!',
    )

    with open(os.path.join('docs', 'modules', 'module_index.html'), 'w') as file:
        file.write(output)


def generate_module_html(modules: List[Dict[str, Any]]):
    template = env.get_template('module_template.html')

    module_names = list(map(lambda m: m['module_name'], modules))
    for module in modules:
        output = template.render(
            module_names=module_names, module_data=module, content_data=''
        )
        with open(
            os.path.join('docs', 'modules', f'{module["module_name"]}.html'), 'w'
        ) as file:
            file.write(output)


# current_directory = os.getcwd()
# fortran_files = find_f90_files(current_directory)
# modules = process_modules(fortran_files)

# create_modules_directory()
# generate_home_html(modules)
# generate_module_html(modules)
