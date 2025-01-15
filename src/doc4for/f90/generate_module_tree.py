import os
import shutil
import errno
import time
import random
from enum import Enum, auto
from typing import List, Dict, Any, Tuple, Optional
from fparser.api import parse as fortran_parser  # type: ignore
from fparser.one.block_statements import (
    Module,
    Comment,
    Function,
    Subroutine,
    Type,
    Public,
    Private,
    Protected
)
from fparser.one.typedecl_statements import TypeDeclarationStatement
from jinja2 import Environment, FileSystemLoader, Template
from pathlib import Path
from doc4for.utils.file_utils import check_write_permissions
from doc4for.models.module_models import ModuleDescription
from doc4for.models.type_models import TypeDescription
from doc4for.parse.parameter_parser import parse_parameter, is_parameter
from doc4for.parse.procedure_parser import parse_subroutine, parse_function
from doc4for.f90.populate_data_models import (
    parse_module,
    parse_type, 
    parse_variable
)

class Visibility(Enum):
    PUBLIC = auto()
    PROTECTED = auto()
    PRIVATE = auto()

def extract_module_data(f90_files: List[Path]) -> List[ModuleDescription]:
    visibility: Dict[str, Visibility] = {}
    modules: List[ModuleDescription] = []
    for f90_file in f90_files:
        comment_stack: List[Comment] = []
        f90_file_path: str = os.fspath(f90_file)
        tree: Any = fortran_parser(f90_file_path, ignore_comments=False)
        for child in tree.content:
            if isinstance(child, Comment) and child.content:
                comment_stack.append(child)
            elif isinstance(child, Module):
                module_data: ModuleDescription = parse_module(child, comment_stack, f90_file_path)
                comment_stack.clear()
                parse_module_content(comment_stack, child, module_data, visibility)
                modules.append(module_data)
                comment_stack.clear() 
    return modules

#TODO recursive modules
def parse_module_content(comment_stack: List[Comment], module: Any, module_data: ModuleDescription, visibility: Dict[str, Visibility], 
                         is_public: bool = True) -> None:
    public_declarations: List[str] = []
    for item in module.content:
        if isinstance(item, Comment) and item.content:
            comment_stack.append(item)
        elif isinstance(item, Public):
             is_public = True
             public_declarations.extend(item.items)
        elif isinstance(item, Private):
            is_public = False
        else:
            match item:
                case Function():
                    module_data['functions'][item.name] = parse_function(item, comment_stack)
                case Subroutine():
                    module_data['subroutines'][item.name] = parse_subroutine(item, comment_stack)
                case Type():
                    type_description: TypeDescription = parse_type(item, comment_stack, public_declarations)
                    module_data['types'][type_description['type_name']] = type_description
                case TypeDeclarationStatement():
                    if is_parameter(item):
                        parameter_descriptions = parse_parameter(item, comment_stack)
                        for param in parameter_descriptions:
                            module_data['parameters'][param['name']] = param
                    else:
                        variable_descriptions = parse_variable(item, comment_stack)
                        for var in variable_descriptions:
                            module_data['variables'][var['name']] = var                
                case _:
                    pass
            comment_stack.clear()

def create_modules_directory(output_dir: str, max_retries: int = 5, base_delay: float = 0.1) -> None:
    """
    Creates a 'modules' directory in the documentation directory if it doesn't exist.
    If it exists, clears its contents.

    The function performs the following actions:
    1. Attempts to create or clear the '{documentation}/modules' directory.
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
    modules_dir: str = os.path.join(output_dir, 'modules')
    if not check_write_permissions(os.path.dirname(modules_dir)):
        raise PermissionError("No write permissions in the documentation directory.")
    for attempt in range(max_retries):
        try:
            if not os.path.exists(modules_dir):
                os.makedirs(modules_dir)
            else:
                clear_directory(modules_dir)
            return
        except (FileExistsError, FileNotFoundError):
            continue
        except OSError as e:
            if e.errno != errno.EEXIST:
                raise
        time.sleep(base_delay * (2 ** attempt) * (random.random() + 0.5))
    if not os.path.exists(modules_dir):
        os.makedirs(modules_dir)
    else:
        clear_directory(modules_dir)

def clear_directory(directory: str) -> None:
    """
    Clears the contents of the given directory.

    Args:
        directory (str): Path to the directory to clear.

    Raises:
        OSError: If there's an issue removing the contents.
    """
    for item in os.listdir(directory):
        item_path: str = os.path.join(directory, item)
        if os.path.islink(item_path) or os.path.isfile(item_path):
            os.unlink(item_path)
        elif os.path.isdir(item_path):
            shutil.rmtree(item_path)

def generate_module_pages(modules: List[ModuleDescription], 
                          template_dir: str, 
                          module_template: str, 
                          output_dir: str) -> None:
    create_modules_directory(output_dir)
    env: Environment = Environment(loader=FileSystemLoader(template_dir))
    template: Template = env.get_template(module_template)
    module_names: List[str] = [module['module_name'] for module in modules]
    
    # Generate index page
    output: str = template.render(
        module_names=module_names,
        module_data={},
        content_data='Welcome to your modules!',
        is_index=True,
        relative_path=''
    )
    with open(os.path.join(output_dir, 'modules', 'module_index.html'), 'w', encoding='utf-8') as file:
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
        with open(os.path.join(output_dir, 'modules', f'{module["module_name"]}.html'), 'w', encoding='utf-8') as file:
            file.write(output)

    
