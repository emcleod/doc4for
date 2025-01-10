import os
import shutil
import errno
import random
import time
from pathlib import Path
from typing import List, Union, Set
import logging

logger = logging.getLogger(__name__)

def find_files_by_extensions(directory: Union[str, Path], 
                            extensions: Set[str] = {'f90'},
                            exclude_dirs: Set[str] = set()) -> List[Path]:
    """
    Finds all files with the specified extensions in a directory and its subdirectories,
    excluding specified directories.

    Args:
        directory (Union[str, Path]): The path to the top-level directory
        extensions (Set[str]): The file extensions to search for (e.g., {'f90', 'f95'})
        exclude_dirs (Set[str]): Directories to exclude from the search
    Returns:
        A list of relative paths to any files that have been found.
    """
    directory = Path(directory)
    # Convert all extensions to lowercase for case-insensitive matching
    lowercase_extensions = {ext.lower().lstrip('.') for ext in extensions}
    # Convert exclude_dirs to absolute paths for comparison
    exclude_paths = {directory / exclude_dir for exclude_dir in exclude_dirs}
    
    files = []
    for file_path in directory.rglob('*'):
        # Check if file is in an excluded directory
        if any(exclude_path in file_path.parents for exclude_path in exclude_paths):
            continue
        
        if (file_path.is_file() and 
            file_path.suffix.lower().lstrip('.') in lowercase_extensions):
            files.append(file_path.relative_to(directory))
    
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

# Get the project root directory
PROJECT_ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), '..', '..'))
DOCS_DIR = 'docs'

def create_docs_directory(docs_dir=DOCS_DIR, max_retries=5, base_delay=0.1) -> bool:
    """
    Creates a 'docs' directory in the project root if it doesn't exist.
    If it exists, clears its contents except for the 'static' subdirectory.

    Args:
        docs_dir (str): Name of the docs directory (default: 'docs')
        max_retries (int): Maximum number of retry attempts.
        base_delay (float): Base delay between retries, in seconds.

    Returns:
        bool: True if directories were created or cleared successfully, False otherwise.

    Raises:
        PermissionError: If the program doesn't have write permissions.
        OSError: If there's an issue creating the directory or removing its contents after all retries.
    """
    def get_directory_contents(directory):
        """
        Recursively get the contents of the given directory and its subdirectories,
        excluding the 'static' directory and its contents.

        Args:
            directory (str): Path to the directory.

        Returns:
            list: List of directory and file paths.
        """
        contents = []
        for root, dirs, files in os.walk(directory):
            if 'static' in dirs:
                dirs.remove('static')  # don't visit 'static' directory
            for dir in dirs:
                contents.append(os.path.join(root, dir))
            for file in files:
                contents.append(os.path.join(root, file))
        return contents

    docs_directory = os.path.join(PROJECT_ROOT, docs_dir)
    if not os.access(PROJECT_ROOT, os.W_OK):
        raise PermissionError(f"No write permissions in the project root directory: {PROJECT_ROOT}")
    if not os.path.exists(docs_directory):
        print(f"Creating the 'docs' directory. The following directory will be created:")
        print(docs_directory)
        response = input("Is this OK (y/n)? ")
        if response.lower() != 'y':
            print("Operation cancelled.")
            return False
        for attempt in range(max_retries):
            try:
                os.makedirs(docs_directory)
                os.makedirs(os.path.join(docs_directory, 'static'))
                logger.info("Directories created successfully.")
                return True
            except FileExistsError:
                logger.info(f"Directory was created by another process after we checked: {docs_directory}")
                continue
            except OSError as e:
                if e.errno != errno.EEXIST:
                    logger.error(f"Error creating directory: {e}")
                    if attempt == max_retries - 1:
                        raise
                retry_delay = base_delay * (2 ** attempt) * (random.random() + 0.5)
                logger.info(f"Retrying after {retry_delay:.2f} seconds...")
                time.sleep(retry_delay)
        logger.info("Final attempt after exhausting all retries")
        os.makedirs(docs_directory)
        os.makedirs(os.path.join(docs_directory, 'static'))
        logger.info("Directories created successfully.")
        return True
    else:
        contents = get_directory_contents(docs_directory)
        if not contents:
            print("The 'docs' directory is already empty (excluding the 'static' subdirectory).")
            return True
        print(f"Found an existing 'docs' directory. The contents of the following directories will be cleared:")
        for item in contents:
            print(item)
        response = input("Is this OK (y/n)? ")
        if response.lower() != 'y':
            print("Operation cancelled.")
            return False
        for attempt in range(max_retries):
            try:
                for item in contents:
                    if os.path.islink(item) or os.path.isfile(item):
                        os.unlink(item)
                    elif os.path.isdir(item):
                        shutil.rmtree(item)
                logger.info("Directories cleared successfully.")
                return True
            except FileNotFoundError:
                logger.info(f"Directory was deleted by another process after we checked: {docs_directory}")
                continue
            except OSError as e:
                if e.errno != errno.EEXIST:
                    raise
            retry_delay = base_delay * (2 ** attempt) * (random.random() + 0.5)
            logger.info(f"Retrying after {retry_delay:.2f} seconds...")
            time.sleep(retry_delay)
        logger.info("Final attempt after exhausting all retries")
        for item in contents:
            if os.path.islink(item) or os.path.isfile(item):
                os.unlink(item)
            elif os.path.isdir(item):
                shutil.rmtree(item)
        logger.info("Directories cleared successfully.")
        return True
    
#TODO use configuration files (e.g., YAML or JSON) to define project-specific settings 
# like file extensions, directories to search, etc.
#TODO log to files and rotate