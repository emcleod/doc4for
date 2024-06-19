import os
from typing import List

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
            if file.endswith('.f90'):
                relative_path = os.path.relpath(os.path.join(root, file), directory)
                f90_files.append(relative_path)
    return f90_files

