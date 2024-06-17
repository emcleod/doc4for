import os
import shutil
from pathlib import Path, PureWindowsPath
from typing import List, Dict, Any
from jinja2 import Environment, FileSystemLoader
from typing import Dict, List, Union, TypeAlias

DirectoryTree: TypeAlias = Dict[str, Union[str, "DirectoryTree"]]
"""
A type representing a directory tree structure.

DirectoryTree is a recursive type alias that represents a nested directory structure.
It's a dictionary where:
- Keys are strings, representing directory or file names.
- Values are either:
  - Strings, representing full file paths for files, or
  - Nested DirectoryTree instances, representing subdirectories.

Examples:
    A simple directory with one file:
    {
        "file.txt": "/path/to/file.txt"
    }

    A directory with a subdirectory:
    {
        "subdir": {
            "file.txt": "/path/to/subdir/file.txt"
        },
        "another_file.txt": "/path/to/another_file.txt"
    }

Note:
    This type allows for arbitrary nesting of directories, accurately representing
    complex file system structures in a dictionary format.
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
            # Get the relative path from the current directory to the file
            if file.endswith(".f90"):
                relative_path = os.path.relpath(os.path.join(root, file), directory)
                f90_files.append(relative_path)
    return f90_files

# TODO
# 1. Handling of invalid file paths:
#    - The function currently assumes that all provided file paths are valid. It doesn't have any explicit error handling for invalid or non-existent file paths.
#    - If an invalid file path is passed, it will still be processed by the function, and the resulting directory tree may include unexpected entries.
#    - To improve this, you could add validation checks to ensure that the provided file paths exist and are valid before processing them.

# 2. Handling of edge cases:
#    - The function doesn't have explicit handling for edge cases like file paths with leading or trailing whitespace, or file paths containing special characters.
#    - While the function may still work correctly in most cases, it's a good practice to handle these edge cases explicitly to ensure consistent behavior.
#    - You could add code to trim leading/trailing whitespace from file paths and handle any special characters appropriately.

# 3. Handling of very large directory structures:
#    - If the function is used to process a very large number of files or deeply nested directory structures, it could potentially consume a significant amount of memory.
#    - The recursive nature of the function means that it will hold all the directory and file information in memory until the processing is complete.
#    - If memory usage is a concern, you may want to consider alternative approaches or optimizations, such as using generators or streaming the results instead of holding the entire structure in memory.
def build_directory_tree(files: List[str]) -> DirectoryTree:
    """
    Builds a directory tree structure from a list of file paths.

    Args:
        files (List[str]): A list of file paths, where each path represents a .f90 file.

    Returns:
        Dict[str, Any]: A dictionary representing the directory tree structure.
                        The keys are directory names, and the values are either nested
                        dictionaries for subdirectories or the original file path for files.

    Example:
        Given the following list of file paths:
        [
            "dir1/subdir1/file1.f90",
            "dir1/subdir2/file2.f90",
            "dir2/file3.f90"
        ]

        The resulting directory tree will be:
        {
            "dir1": {
                "subdir1": {
                    "file1.f90": "dir1/subdir1/file1.f90"
                },
                "subdir2": {
                    "file2.f90": "dir1/subdir2/file2.f90"
                }
            },
            "dir2": {
                "file3.f90": "dir2/file3.f90"
            }
        }

    Note:
        - The function assumes that the file paths are valid and contain only directories and .f90 files.
        - The resulting dictionary represents the directory structure, with directories as nested dictionaries
          and files as key-value pairs, where the key is the file name and the value is the original file path.
    """
    #TODO stop ignoring the types
    directory_tree: DirectoryTree = {}

    for file in files:
        # Convert Windows-style paths to Unix-style
        if '\\' in file:
            file = PureWindowsPath(file).as_posix()

        # Convert the file path to a Path object
        path = Path(file)

        # Determine if the path is absolute
        if path.is_absolute():
            # For absolute paths, start with the root
            current_level = directory_tree.setdefault('/', {})
            parts = path.parts[1:]  # Skip the root
        else:
            # For relative paths, start at the top level
            current_level = directory_tree
            parts = path.parts

        # Build the directory tree
        for part in parts[:-1]:  # All parts except the last (filename)
            if part not in current_level:
                current_level[part] = {} # type: ignore
            current_level = current_level[part] # type: ignore
        
        # Add the file to the tree
        current_level[path.name] = str(path) # type: ignore

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
generate_index_html(directory_tree)

generate_html_files(directory_tree)
