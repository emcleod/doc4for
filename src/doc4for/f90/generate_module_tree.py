""" Module for processing Fortran 90 modules and comments and generating a static webpage from their contents.

"""
import os
import shutil
import errno
import time
import random
import re
from typing import List, Any
from fparser.api import parse as fortran_parser  # type: ignore
from fparser.one.block_statements import (
    Module,
    Comment,
    Function,
    Subroutine,
    Type,
    Public
)
from fparser.one.typedecl_statements import TypeDeclarationStatement
from jinja2 import Environment, FileSystemLoader
from pathlib import Path
from doc4for.file_utils import check_write_permissions
from doc4for.data_models import (
    FunctionDescription,
    SubroutineDescription,
    ParameterDescription,
    TypeDescription,
    ModuleData,
)
from doc4for.comment_utils import is_doc4for_comment, format_comments
from doc4for.argument_utils import update_arguments_with_comment_data, update_arguments_with_parsed_data
from doc4for.type_utils import update_type_with_parsed_data

def extract_module_data(f90_files: List[Path]) -> List[ModuleData]:
    modules: List[ModuleData] = []
    for f90_file in f90_files:
        comment_stack: List[Comment] = []
        f90_file_str = os.fspath(f90_file)
        tree: Any = fortran_parser(f90_file_str, ignore_comments=False)
        for child in tree.content:
            if isinstance(child, Comment) and child.content:
                comment_stack.append(child)
            elif isinstance(child, Module):
                module_data: ModuleData = {
                    'module_name': child.name,
                    'parameters': {},
                    'functions': {},
                    'subroutines': {},
                    'types': {},
                    'file_name': f90_file_str,
                    'module_description': ''
                }
                if is_doc4for_comment(comment_stack):
                    module_data['module_description'] = format_comments(comment_stack)
                comment_stack = []  
                public_declarations = []
                for item in child.content:
                    if isinstance(item, Comment) and item.content:
                        comment_stack.append(item)
                    elif isinstance(item, Public):
                        public_declarations.extend(item.items)
                    elif isinstance(item, Function):
                        function_name: str = item.name
                        attributes: List[str] = [attr.strip().lower() for attr in item.prefix.split() if attr.strip()]
                        function_description: FunctionDescription = {
                            'attributes': attributes,
                            'description': '',
                            'arguments': item.args,
                            'in': {},
                            'out': {},
                            'return': {},
                            'binding_type': '',
                            'interface': ''
                        }
                        update_arguments_with_parsed_data(item, function_description)
                        if comment_stack:
                            update_arguments_with_comment_data(comment_stack, function_description)
                        module_data['functions'][function_name] = function_description
                        comment_stack = []  
                    elif isinstance(item, Subroutine):
                        subroutine_name: str = item.name
                        attributes: List[str] = [attr.strip().lower() for attr in item.prefix.split() if attr.strip()]
                        subroutine_description: SubroutineDescription = {
                            'attributes': attributes,
                            'description': '',
                            'arguments': item.args,
                            'in': {},
                            'out': {},
                            'binding_type': '',
                            'interface': ''
                        }
                        update_arguments_with_parsed_data(item, subroutine_description)
                        if comment_stack:
                            update_arguments_with_comment_data(comment_stack, subroutine_description)
                        module_data['subroutines'][subroutine_name] = subroutine_description
                        comment_stack = []  
                    elif isinstance(item, Type):
                        type_name: str = item.name
                        type_description: TypeDescription = {
                            'type_name': type_name,
                            'attributes': [],
                            'description': '',
                            'data_components': {},
                            'procedures': {},
                            'generic_interfaces': {},
                            'extends': None
                        }
                        type_description['attributes'].extend(item.specs)
                        if type_name in public_declarations:
                            type_description['attributes'].append('public')
                        update_type_with_parsed_data(item, type_description)
                        if comment_stack:
                            type_description['description'] = format_comments(comment_stack)
                        module_data['types'][type_name] = type_description
                        comment_stack = []
                    elif isinstance(item, TypeDeclarationStatement) and 'parameter' in item.attrspec:
                        param_info = extract_parameter_info(item)
                        if is_doc4for_comment(comment_stack):
                            param_info['description'] = format_comments(comment_stack)
                        module_data['parameters'][param_info['name']] = param_info                        
                        comment_stack = []  
                modules.append(module_data)
                comment_stack = []  
    return modules

def extract_parameter_info(decl: TypeDeclarationStatement) -> ParameterDescription:
    item_str = decl.item.line
    name_match = re.search(r'::\s*(\w+)\s*=', item_str)
    if name_match:
        name = name_match.group(1)
        value = item_str.split('=', 1)[1].strip()
    else:
        # Handle cases where the parameter declaration format is not recognized
        name = ''
        value = ''
    return {
        'description': '',  
        'type': str(decl.name),
        'name': name.strip(),
        'value': value.strip(),
        'dimension': ''
    }

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

def generate_module_pages(modules: List[ModuleData]):
    create_modules_directory()
    env = Environment(loader=FileSystemLoader('templates'))
    template = env.get_template('module_template.html')
    module_names = [module['module_name'] for module in modules]
    output = template.render(
        module_names=module_names,
        module_data={},
        content_data='Welcome to your modules!',
        is_index=True,
        relative_path=''
    )
    with open(os.path.join('docs', 'modules', 'module_index.html'), 'w', encoding='utf-8') as file:
        file.write(output)
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
