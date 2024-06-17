import os
import shutil
from pathlib import Path, PureWindowsPath
from typing import List, Dict, Any
from jinja2 import Environment, FileSystemLoader
from typing import Dict, List, Union, Optional, Iterator

class DirectoryTree:
    def __init__(self, name: str, parent: Optional['DirectoryTree'] = None):
        self.name = name
        self.parent = parent
        self.children: List[Union[str, 'DirectoryTree']] = []

    def add_path(self, path: Path):
        parts = path.parts
        current = self
        for part in parts[:-1]:
            child = next((c for c in current.children if isinstance(c, DirectoryTree) and c.name == part), None)
            if child is None:
                child = DirectoryTree(part, current)
                current.children.append(child)
            current = child
        current.children.append(str(path))

    def walk(self) -> Iterator[Union[str, 'DirectoryTree']]:
        yield self.name
        for child in self.children:
            if isinstance(child, DirectoryTree):
                yield from child.walk()
            else:
                yield child


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
            # Get the relative path from the current directory to the file
            if file.endswith(".f90"):
                relative_path = os.path.relpath(os.path.join(root, file), directory)
                f90_files.append(relative_path)
    return f90_files


# TODO
# invalid file paths - add error handling and validation checks
# special characters / whitespace
def build_directory_tree(files: List[str]) -> DirectoryTree:
    directory_tree = DirectoryTree('')

    for file in files:
        # Convert Windows-style paths to Unix-style
        if '\\' in file:
            file = PureWindowsPath(file).as_posix()

        # Convert the file path to a Path object
        path = Path(file)

        # Add the path to the directory tree
        directory_tree.add_path(path)

    return directory_tree

def create_docs_directory():
    docs_directory = 'docs'
    if not os.path.exists(docs_directory):
        os.makedirs(docs_directory)
    else:
        # Clear the docs directory (except for the static directory)
        for item in os.listdir(docs_directory):
            item_path = os.path.join(docs_directory, item)
            if item == 'static':
                continue
            if os.path.isfile(item_path):
                os.remove(item_path)
            elif os.path.isdir(item_path):
                shutil.rmtree(item_path)

env = Environment(loader=FileSystemLoader('templates'))

def generate_index_html(directory_tree: Dict[str, Any]):
    # Load the template
    template = env.get_template('file_template.html')
    # Render the template with the data
    output = template.render(sidebar_data = directory_tree,
                             content_data = 'Welcome to your code!',
                             path = '',
                             current_path = '',
                             is_index = True,
                             separator = os.sep)
    # Save the rendered output to a file
    with open(os.path.join('docs', 'index.html'), 'w', encoding='utf-8') as file:
        file.write(output)

def generate_html_files(directory_tree: Dict[str, Any]):
    template = env.get_template('file_template.html')

    def generate_html_recursively(tree: Dict[str, Any], current_path: str):
        for key, value in tree.items():
            if isinstance(value, str):
                file_path = value
                file_name = os.path.basename(file_path)
                target_directory = os.path.join('docs', os.path.dirname(file_path))
                os.makedirs(target_directory, exist_ok=True)
                # Include 'docs' in the current_path
                current_path_with_docs = os.path.join('docs', current_path)
                # Read the file
                with open(file_path, 'r', encoding='utf-8', errors='replace') as file:
                    code = file.read()
                # Render the template
                output = template.render(sidebar_data = directory_tree,
                                         content_data = '',
                                         code = code,
                                         path = current_path,
                                         file = file_name,
                                         current_path = current_path_with_docs,
                                         is_index = False,
                                         separator = os.sep)
                with open(
                    os.path.join(target_directory, f'{file_name[:-4]}.html'),
                    'w', 
                    encoding='utf-8'
                ) as file:
                    file.write(output)
            else:
                generate_html_recursively(value, os.path.join(current_path, key))

    generate_html_recursively(directory_tree, '')

# Find .f90 files starting from the current directory
current_directory = os.getcwd()
fortran_files = find_f90_files(current_directory)

# Build the directory tree
directory_tree = build_directory_tree(fortran_files)

# Manage the docs directory
create_docs_directory()

# Generate the HTML content
# generate_index_html(directory_tree)

# generate_html_files(directory_tree)
