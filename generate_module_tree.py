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
from re import Match
from typing import List, Dict, Any, Tuple, Optional, TypedDict

from typing import TypedDict, Optional

class ArgumentInfo(TypedDict):
    type: str
    description: str
    dimension: Optional[str]

class ArgumentsDict(TypedDict):
    in_: Dict[str, ArgumentInfo]
    out_: Dict[str, ArgumentInfo]
    return_: Dict[str, ArgumentInfo]

def find_f90_files(directory: str) -> List[str]:
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
    if hasattr(item, 'attrspec'):
        intentin_ = 'intent(in)' in item.attrspec
        intentout_ = 'intent(out)' in item.attrspec
    else:
        intentin_, intentout_ = False, False
    return intentin_, intentout_

def add_dimension_info(decl: str, dims: List[int]) -> str:
    if not dims:
        return ''
    dimension_parts = ['allocatable' if not dim else str(dim) for dim in dims]
    return ' &times; '.join(dimension_parts)

def process_arg(decl: str, arg_type: str, intentin_: bool, intentout_: bool,
               inputs: Dict[str, ArgumentInfo], outputs: Dict[str, ArgumentInfo], dims: List[int] = []) -> None:
    if intentin_:
        inputs[decl] = {'type': arg_type, 'description': '', 'dimension': add_dimension_info(decl, dims) }
    elif intentout_:
        outputs[decl] = {'type': arg_type, 'description': '', 'dimension': add_dimension_info(decl, dims) }
    else:
        inputs[decl] = {'type': arg_type, 'description': '', 'dimension': add_dimension_info(decl, dims) }
        outputs[decl] = {'type': arg_type, 'description': '', 'dimension': add_dimension_info(decl, dims) }

def get_return_type(function: Any) -> str:
    return function.typedecl.name if function.typedecl else 'Unknown'

def extract_arg_info(function: Any) -> ArgumentsDict:
    arg_info: ArgumentsDict = {'in_': {}, 'out_': {}, 'return_': {}}
    args: List[str] = function.args
    result: str = function.result
    for item in function.content:
        if isinstance(item, TypeDeclarationStatement):
            arg_type = get_arg_type(item)
            intentin_, intentout_ = get_arg_intent(item)
            for decl in item.entity_decls:
                # TODO handle assumed size arrays
                if not ':' in decl:  # it's a scalar or assumed size
                    if decl in args:
                        process_arg(decl, arg_type, intentin_, intentout_, arg_info['in_'], arg_info['out_'])
                    elif decl == result:
                        # TODO sort out dimension
                        arg_info['return_'][decl] = {'type': arg_type, 'description': '', 'dimension': ''}
                else:
                    name, dimensions = decl.split('(')
                    dim = dimensions[:-1].split(':')
                    if name in args:
                        process_arg(name, arg_type, intentin_, intentout_, arg_info['in_'], arg_info['out_'], dim)
                    elif name == result:
                        # TODO sort out dimension
                        arg_info['return_'][decl] = {'type': arg_type, 'description': '', 'dimension': ''}

    if not arg_info['return_']:
        return_type = get_return_type(function)
        # TODO sort out dimension
        arg_info['return_'][result] = {'type': return_type, 'description': '', 'dimension': ''}
    return arg_info

# TODO pass in data and add description to that
def process_function_comments(comments: List[Comment]) -> Tuple[Dict[str, Any], Dict[str, Any], str]:
    arg_info: Dict[str, Any] = {}
    returnin_fo: Dict[str, Any] = {}
    description = ''
    for comment in comments:
        content = comment.content.strip()
        if content.startswith('@in') or content.startswith('@out') or content.startswith('@inout'):
            parts = content.split()
            if len(parts) >= 3:
                arg_name, arg_type = parts[1].rstrip(':'), parts[2]
                arg_desc = ' '.join(parts[3:])
                arg_info[arg_name] = {
                    'name': arg_name,
                    'type': arg_type,
                    'description': arg_desc,
                }
        elif content.startswith('@return'):
            parts = content.split()
            if len(parts) >= 3:
                return_name, return_type = parts[1].rstrip(':'), parts[2]
                return_desc = ' '.join(parts[3:])
                returnin_fo = {
                    'name': return_name,
                    'type': return_type,
                    'description': return_desc,
                }
        elif not content.startswith('!*') and not content.endswith('*!'):
            description += process_comment(content) + '\n'
    return arg_info, returnin_fo, description.strip()


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
                module_data['module_name'] = child.name
                module_data['constants'] = {}
                module_data['functions'] = {}
                module_data['subroutines'] = {}
                module_data['file_name'] = f90_file

                # collect module comments
                if (
                    comment_stack
                    and comment_stack[0].content.startswith('!*')
                    and comment_stack[-1].content.endswith('*!')
                ):
                    module_data['module_description'] = ''
                    for comment in comment_stack[1:-1]:
                        content = process_comment(comment.content)
                        if content:
                            module_data['module_description'] += f'{content}\n'

                function_comments = []
                for item in child.content:
                    if isinstance(item, Comment):
                        function_comments.append(item)
                    elif isinstance(item, Function):
                        function_name = item.name
                        module_data['functions'][function_name] = {
                            'description': '',
                            'details': {},
                        }
                        attributes = [attr.strip().lower() for attr in item.prefix.split() if attr.strip()]
                        module_data['functions'][function_name]['details'] = {
                            'attributes': attributes,
                            'arguments': {},
                        }
                        arg_info: ArgumentsDict = extract_arg_info(item)

                        if function_comments:
                            comment_arg_info, commentreturn_in_fo, func_description = (
                                process_function_comments(function_comments)
                            )

                            for arg_name, arg_data in comment_arg_info.items():
                                if arg_name in arg_info['in_']:
                                    if arg_data['type'] != arg_info['in_'][arg_name]['type']:
                                        print(
                                            f'Warning: Mismatched type for input argument {arg_name} in function {function_name}'
                                        )
                                    arg_info['in_'][arg_name].update(arg_data)
                                elif arg_name in arg_info['out_']:
                                    if arg_data['type'] != arg_info['out_'][arg_name]['type']:
                                        print(
                                            f'Warning: Mismatched type for output argument {arg_name} in function {function_name}'
                                        )
                                    arg_info['out_'][arg_name].update(arg_data)
                                else:
                                    print(
                                        f'Warning: Argument {arg_name} in comment not found in function {function_name}'
                                    )

                            # Update result info
                            if commentreturn_in_fo:
                                result_name = list(arg_info['return_'].keys())[0] if arg_info['return_'] else None
                                if (
                                    result_name
                                    and commentreturn_in_fo['name'] != result_name
                                ):
                                    print(
                                        f'Warning: Mismatched result name in comment for function {function_name}'
                                    )
                                if (result_name and commentreturn_in_fo['type'] != arg_info['return_'][result_name]['type']
                                ):
                                    print(
                                        f'Warning: Mismatched result type in comment for function {function_name}'
                                    )
                                arg_info['return_'].update(commentreturn_in_fo)

                            # Update function description
                            module_data['functions'][function_name]['description'] = func_description

                        # Update function details with input, output, and result info
                        module_data['functions'][function_name]['details'].update(arg_info)

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

    with open(os.path.join('docs', 'modules', 'modulein_dex.html'), 'w') as file:
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
