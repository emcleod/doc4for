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
from typing import List, Dict, Any, Tuple, Optional

# TODO
# class ModuleDict(TypedDict):
#     module_name: str
#     file_name: str
#     module_description: str
#     constants: Dict[str, Any]
#     functions: Dict[str, Any]
#     subroutines: Dict[str, Any]


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
        intent_in = 'intent(in)' in item.attrspec
        intent_out = 'intent(out)' in item.attrspec
    else:
        intent_in, intent_out = False, False
    return intent_in, intent_out

def add_dimension_info(decl: str, info: Dict[str, Any], dims: List[int]) -> None:
    if not dims:
        return
    dimension_parts = ['allocatable' if not dim else str(dim) for dim in dims]
    info[decl]['dimension'] = ' &times; '.join(dimension_parts)

def process_arg(decl: str, arg_type: str, intent_in: bool, intent_out: bool,
               inputs: Dict[str, Any], outputs: Dict[str, Any], dims: List[int] = []) -> None:
    if intent_in:
        inputs[decl] = {'type': arg_type}
        add_dimension_info(decl, inputs, dims)
    elif intent_out:
        outputs[decl] = {'type': arg_type}
        add_dimension_info(decl, outputs, dims)
    else:
        inputs[decl] = {'type': arg_type}
        outputs[decl] = {'type': arg_type}
        add_dimension_info(decl, outputs, dims)
        add_dimension_info(decl, inputs, dims)

def get_return_type(function: Any) -> Optional[str]:
    return function.typedecl.name if function.typedecl else None

def extract_arg_info(function: Any) -> Tuple[Dict[str, Any], Dict[str, Any], Dict[str, Any]]:
    inputs: Dict[str, Any] = {}
    outputs: Dict[str, Any] = {}
    results: Dict[str, Any] = {}
    args: List[str] = function.args
    result: str = function.result
    for item in function.content:
        if isinstance(item, TypeDeclarationStatement):
            arg_type = get_arg_type(item)
            intent_in, intent_out = get_arg_intent(item)
            for decl in item.entity_decls:
                # TODO handle assumed size arrays
                if not ':' in decl: # it's a scalar or assumed size
                    if decl in args:
                        process_arg(decl, arg_type, intent_in, intent_out, inputs, outputs)
                    elif decl == result:
                        results[decl] = {'type': arg_type}
                else:
                    name, dimensions = decl.split('(')
                    dim = dimensions[:-1].split(':')
                    if name in args:
                        process_arg(name, arg_type, intent_in, intent_out, inputs, outputs, dim)
                    elif name == result:
                        results[decl] = {'type': arg_type}

    if not results:
        return_type = get_return_type(function)
        results[result] = {'type': return_type}
    return inputs, outputs, results

def process_function_comments(
    comments: List[Comment],
) -> Tuple[Dict[str, Any], Dict[str, Any], str]:
    arg_info: Dict[str, Any] = {}
    return_info: Dict[str, Any] = {}
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
                return_info = {
                    'name': return_name,
                    'type': return_type,
                    'description': return_desc,
                }
        elif not content.startswith('!*') and not content.endswith('*!'):
            description += process_comment(content) + '\n'
    return arg_info, return_info, description.strip()


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

                        # Extract function details
                        attributes = [
                            attr.strip().lower()
                            for attr in item.prefix.split()
                            if attr.strip()
                        ]
                        module_data['functions'][function_name]['details'] = {
                            'attributes': attributes,
                            'arguments': {},
                        }

                        inputs, outputs, result = extract_arg_info(item)

                        if function_comments:
                            # Process function comments
                            comment_arg_info, comment_return_info, func_description = (
                                process_function_comments(function_comments)
                            )

                            # Compare and update input argument info
                            for arg_name, arg_data in comment_arg_info.items():
                                if arg_name in inputs:
                                    if arg_data['type'] != inputs[arg_name]['type']:
                                        print(
                                            f'Warning: Mismatched type for input argument {arg_name} in function {function_name}'
                                        )
                                    inputs[arg_name].update(arg_data)
                                elif arg_name in outputs:
                                    if arg_data['type'] != outputs[arg_name]['type']:
                                        print(
                                            f'Warning: Mismatched type for output argument {arg_name} in function {function_name}'
                                        )
                                    outputs[arg_name].update(arg_data)
                                else:
                                    print(
                                        f'Warning: Argument {arg_name} in comment not found in function {function_name}'
                                    )

                            # Update result info
                            if comment_return_info:
                                result_name = list(result.keys())[0] if result else None
                                if (
                                    result_name
                                    and comment_return_info['name'] != result_name
                                ):
                                    print(
                                        f'Warning: Mismatched result name in comment for function {function_name}'
                                    )
                                if (
                                    result_name
                                    and comment_return_info['type']
                                    != result[result_name]['type']
                                ):
                                    print(
                                        f'Warning: Mismatched result type in comment for function {function_name}'
                                    )
                                result.update(comment_return_info)

                            # Update function description
                            module_data['functions'][function_name][
                                'description'
                            ] = func_description

                        # Update function details with input, output, and result info
                        module_data['functions'][function_name]['details'][
                            'inputs'
                        ] = inputs
                        module_data['functions'][function_name]['details'][
                            'outputs'
                        ] = outputs
                        module_data['functions'][function_name]['details'][
                            'return'
                        ] = result

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
