""" Module for processing Fortran 90 modules and comments and generating a static webpage from their contents.

"""
import os
import shutil
import errno
import time
import random
from typing import List, Dict, Any
from fparser.api import parse as fortran_parser  # type: ignore
from fparser.one.block_statements import (
    Module,
    Comment,
    Function,
    Subroutine
)
from jinja2 import Environment, FileSystemLoader
from pathlib import Path
from doc4for.file_utils import check_write_permissions
from doc4for.data_models import (
    FunctionDescription,
    SubroutineDescription,
    ModuleData,
)
from doc4for.html_comment_utils import format_comment_for_html
from doc4for.arguments import update_arguments_with_comment_data, update_arguments_with_parsed_data

def process_modules(f90_files: List[Path]) -> List[ModuleData]:
    modules: List[ModuleData] = []
    for f90_file in f90_files:
        comment_stack: List[Comment] = []
        f90_file_str = os.fspath(f90_file)
        tree: Any = fortran_parser(f90_file_str, ignore_comments=False)
        for child in tree.content:
            if isinstance(child, Comment):
                comment_stack.append(child)
            elif isinstance(child, Module):
                module_data: ModuleData = {
                    'module_name': child.name,
                    'constants': {},
                    'functions': {},
                    'subroutines': {},
                    'file_name': f90_file_str,
                    'module_description': ''
                }
                # collect module comments
                if (comment_stack and comment_stack[0].content.startswith('!*') and comment_stack[-1].content.endswith('*!')):
                    module_data['module_description'] = '\n'.join(
                        format_comment_for_html(comment.content) for comment in comment_stack[1:-1] if format_comment_for_html(comment.content)) + '\n'
                comment_stack = []  # Reset comment_stack for the next entity
                for item in child.content:
                    if isinstance(item, Comment):
                        comment_stack.append(item)
                    elif isinstance(item, Function):
                        function_name: str = item.name
                        attributes: List[str] = [attr.strip().lower() for attr in item.prefix.split() if attr.strip()]
                        function_description: FunctionDescription = {
                            'attributes': attributes,
                            'description': '',
                            'in': {},
                            'out': {},
                            'return': {}
                        }
                        update_arguments_with_parsed_data(item, function_description)
                        if comment_stack:
                            update_arguments_with_comment_data(comment_stack, function_description)
                        module_data['functions'][function_name] = {
                            'details': function_description
                        }
                        comment_stack = []  # Reset comment_stack for the next entity
                    elif isinstance(item, Subroutine):
                        subroutine_name: str = item.name
                        attributes: List[str] = [attr.strip().lower() for attr in item.prefix.split() if attr.strip()]
                        subroutine_description: SubroutineDescription = {
                            'attributes': attributes,
                            'description': '',
                            'in': {},
                            'out': {},
                        }
                        update_arguments_with_parsed_data(item, subroutine_description)
                        if comment_stack:
                            update_arguments_with_comment_data(comment_stack, subroutine_description)
                        module_data['subroutines'][subroutine_name] = {
                            'details': subroutine_description
                        }
                        comment_stack = []  # Reset comment_stack for the next entity
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

# 1. Subroutine vs. Function:
#    - You can differentiate between subroutines and functions. Subroutines do not have a return value, while functions do.
#    - This information can be obtained from the `typedecl` attribute of the `Function` object. If `typedecl` is present, it indicates a function; otherwise, it's a subroutine.

# 2. Module Procedures:
#    - You can identify module procedures, which are functions or subroutines defined within a module.
#    - Module procedures have a `parent` attribute that refers to the module in which they are defined.

# 3. Attributes:
#    - In addition to `intent`, you can extract other attributes of variables and arguments, such as `allocatable`, `dimension`, `pointer`, `target`, etc.
#    - These attributes can be found in the `attrspec` attribute of the `TypeDeclarationStatement` object.

# 4. Assumed-Shape Arrays:
#    - Assumed-shape arrays are declared with dimensions specified using colons (e.g., `array(:)`).
#    - You can identify assumed-shape arrays by checking for the presence of colons in the dimension specifier.

# 5. Allocatable Arrays:
#    - Allocatable arrays are declared with the `allocatable` attribute.
#    - You can check for the presence of the `allocatable` attribute in the `attrspec` attribute of the `TypeDeclarationStatement` object.

# 6. Assumed-Size Arrays:
#    - Assumed-size arrays are declared with an asterisk as the last dimension (e.g., `array(10,*)`).
#    - You can identify assumed-size arrays by checking for the presence of an asterisk in the last dimension specifier.

# 7. Optional Arguments:
#    - Optional arguments are declared with the `optional` attribute.
#    - You can check for the presence of the `optional` attribute in the `attrspec` attribute of the `TypeDeclarationStatement` object.

# 8. Derived Types:
#    - Derived types are user-defined types in Fortran.
#    - You can identify derived type declarations by checking for the presence of a `Type` object in the parsed code.

# 9. Interfaces and Generic Procedures:
#    - Interfaces and generic procedures provide a way to overload procedures with different argument types or define a set of procedures with a common name.
#    - You can identify interfaces and generic procedures by checking for the presence of an `Interface` object in the parsed code.

# 10. Use Statements and External Procedures:
#     - Use statements indicate modules that are being used within a program unit.
#     - External procedures are procedures that are defined outside the current program unit and are referenced using the `external` attribute.
#     - You can identify use statements and external procedures by checking for the presence of `Use` and `External` objects in the parsed code.