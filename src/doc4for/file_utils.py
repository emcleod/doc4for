import os
import tempfile
from typing import List

import os
from pathlib import Path
from typing import List, Union, Set

def find_files_by_extensions(directory: Union[str, Path], extensions: Set[str] = {'f90'}) -> List[Path]:
    """
    Finds all files with the specified extensions in a directory and its subdirectories.

    Args:
        directory (Union[str, Path]): The path to the top-level directory
        extensions (Set[str]): The file extensions to search for (e.g., {'f90', 'f95'})
    Returns:
        A list of relative paths to any files that have been found.
    """
    directory = Path(directory)
    files = [file_path.relative_to(directory) 
             for extension in extensions 
             for file_path in directory.rglob(f'*.{extension.lstrip(".")}')]
    return files

def check_write_permissions(path: Union[str, Path]) -> bool:
    """
    Check if the program has write permissions in the specified path.

    Args:
        path (Union[str, Path]): The path to check for write permissions.

    Returns:
        bool: True if write permissions are available, False otherwise.
    """
    path = Path(path)
    return os.access(path, os.W_OK)

#TODO use configuration files (e.g., YAML or JSON) to define project-specific settings 
# like file extensions, directories to search, etc.