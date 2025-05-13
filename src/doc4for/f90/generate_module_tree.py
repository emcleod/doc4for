import os
import shutil
import errno
import time
import random
import logging
import sys
from enum import Enum, auto
from typing import List
from fparser.two.parser import ParserFactory
from fparser.common.readfortran import FortranFileReader
from fparser.two.Fortran2003 import (
    Module, 
    Comment,
    Dimension_Stmt
)
from jinja2 import Environment, FileSystemLoader, Template
from pathlib import Path
from doc4for.utils.file_utils import check_write_permissions
from doc4for.models.module_models import ModuleDescription
from doc4for.f90.populate_data_models import initialise_module_description
from doc4for.parse.module_parser import parse_module_content

logger: logging.Logger = logging.getLogger(__name__)

class Visibility(Enum):
    PUBLIC = auto()
    PROTECTED = auto()
    PRIVATE = auto()

#
# TODO can't create the parser
#
# The issue is in ParserFactory.create() method
# It tries to access Fortran2008.__name__ before the module is fully imported
# The fix would be to use the string 'fparser.two.Fortran2008' directly instead of Fortran2008.__name__
#
# The error is in
# f2008_cls_members = inspect.getmembers(
#    sys.modules[Fortran2008.__name__], inspect.isclass  # Bug: should use 'fparser.two.Fortran2008'
#)
#
# bug report:
# The error: KeyError: 'fparser.two.Fortran2008' when calling ParserFactory().create(std="f2008")

# The location: ParserFactory.create() method in /fparser/two/parser.py, line ~154

# The cause: The code tries to access sys.modules[Fortran2008.__name__] before the module is properly loaded into sys.modules

# The proposed fix: Change the line to use the string 'fparser.two.Fortran2008' directly instead of Fortran2008.__name__

try:
    from fparser.two import Fortran2008
    sys.modules['fparser.two.Fortran2008'] = Fortran2008
except ImportError:
    pass

def extract_module_data(f90_files: List[Path]) -> List[ModuleDescription]:
    modules: List[ModuleDescription] = []
    
    # Create a parser
    try:
        parser = ParserFactory().create(std="f2008")
    except:
        parser = ParserFactory().create(std="f2003")
    
    for f90_file in f90_files:
        comment_stack: List[Comment] = []
        f90_file_path: str = os.fspath(f90_file)
        
        reader = FortranFileReader(f90_file_path, ignore_comments=False)
        tree = parser(reader)
        
        # Walk through the parse tree  
        for child in tree.content:     
            # comments before the module
            if isinstance(child, Comment) and child.item.comment:
                comment_stack.append(child)
            elif isinstance(child, Module):
                module_data: ModuleDescription = initialise_module_description(child, comment_stack, f90_file_path)
                comment_stack.clear()
                parse_module_content(child, module_data, comment_stack)
                modules.append(module_data)
                comment_stack.clear()
                    
            
    return modules

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
    module_names: List[str] = [module["module_name"] for module in modules]
    
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

    
