import os
import shutil
import tempfile
import errno
import time
import random
from typing import List, Union, Optional, Iterator
from pathlib import Path, PureWindowsPath
from jinja2 import Environment, FileSystemLoader

class DirectoryTree:
    """Represents a directory tree structure.

    This class provides methods to build and traverse a directory tree structure.
    The tree is composed of `DirectoryTree` objects representing directories and
    strings representing file paths.

    Attributes:
        name (str): The name of the directory or file.
        parent (Optional[DirectoryTree]): The parent directory of this directory or file.
        children (List[Union[str, DirectoryTree]]): A list of child directories and files.

    Methods:
        add_path(path: Path):
            Adds a file path to the directory tree.

        walk() -> Iterator[Union[str, DirectoryTree]]:
            Yields the names of directories and files in the tree by traversing it recursively.
    """

    def __init__(self, name: str, parent: Optional['DirectoryTree'] = None):
        """Initializes a new DirectoryTree instance.

        Args:
            name (str): The name of the directory or file.
            parent (Optional[DirectoryTree]): The parent directory of this directory or file.
        """
        self.name = name
        self.parent = parent
        self.children: List[Union[str, 'DirectoryTree']] = []

    def add_path(self, path: Path):
        """Adds a file path to the directory tree.

        Args:
            path (Path): The file path to be added to the tree.

        The method recursively traverses the directory tree and creates new `DirectoryTree`
        instances for any missing directories in the path. The file name is added as a string
        to the final directory in the path.
        """
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
        """Yields the names of directories and files in the tree by traversing it recursively.

        Yields:
            Union[str, DirectoryTree]: The name of a directory or file in the tree.

        The method yields the name of the current directory, then recursively yields the names
        of its child directories and files.
        """
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
            if file.endswith('.f90'):
                relative_path = os.path.relpath(os.path.join(root, file), directory)
                f90_files.append(relative_path)
    return f90_files


def build_directory_tree(files: List[str]) -> DirectoryTree:
    """Builds a directory tree from a list of file paths.

    Args:
        files: A list of file paths as strings.

    Returns:
        A `DirectoryTree` object representing the directory structure.

    Raises:
        MemoryError: If the file list is too large and causes a memory error.
        Exception: If an unexpected error occurs during the building process.

    The function takes a list of file paths and constructs a directory tree based on the paths.
    It handles both Unix-style and Windows-style paths. If a Windows-style path is encountered,
    it is converted to a Unix-style path using `PureWindowsPath`.

    If an invalid Windows path is encountered, a warning message is printed, and the path is skipped.
    If an error occurs while adding a path to the directory tree, a warning message is printed,
    and the path is skipped.

    The function returns a `DirectoryTree` object representing the directory structure.

    If a `MemoryError` occurs due to a large file list, an error message is printed,
    and the exception is re-raised.

    If any other unexpected exception occurs, an error message is printed, and the exception
    is re-raised.

    Example:
        files = [
            '/path/to/file1.txt',
            '/path/to/file2.txt',
            '/another/path/file3.txt',
            'C:\\Windows\\path\\file4.txt'
        ]
        directory_tree = build_directory_tree(files)
        for item in directory_tree.walk():
            print(item)
    """
    try:
        directory_tree = DirectoryTree('')
        for file in files:
            # Convert Windows-style paths to Unix-style
            if '\\' in file:
                try:
                    file = PureWindowsPath(file).as_posix()
                except ValueError:
                    print(f'Invalid Windows path: {file}. Skipping.')
                    continue
            path = Path(file)
            try:
                directory_tree.add_path(path)
            except Exception:
                print(f'Error adding path: {path}. Skipping.')
                continue
        return directory_tree
    except MemoryError as e:
        print(f'Memory error occurred. The file list might be too large: {e}.')
        raise
    except Exception as e:
        print(f'An unexpected error occurred: {e}')
        raise

# #TODO what about large directories
def create_docs_directory(max_retries=5, base_delay=0.1):
    """
    Creates a 'docs' directory in the current working directory if it doesn't exist.
    If it exists, clears its contents except for the 'static' subdirectory.

    The function performs the following actions:
    1. Attempts to create or clear the 'docs' directory.
    2. If the 'docs' directory doesn't exist, creates a 'static' subdirectory within it.
    3. If a race condition is detected, it retries the operation.

    Args:
        max_retries (int): Maximum number of retry attempts.
        base_delay (float): Base delay between retries, in seconds.

    Returns:
        None

    Raises:
        PermissionError: If the program doesn't have write permissions.
        OSError: If there's an issue creating the directory or removing its contents after all retries.
    """
    docs_directory = 'docs'

    if not check_write_permissions(os.getcwd()):
        raise PermissionError("No write permissions in the current directory.")
    for attempt in range(max_retries):
        try:
            if not os.path.exists(docs_directory):
                os.makedirs(docs_directory)
                os.makedirs(os.path.join(docs_directory, 'static'))
            else:
                clear_directory(docs_directory)
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
    if not os.path.exists(docs_directory):
        os.makedirs(docs_directory)
        os.makedirs(os.path.join(docs_directory, 'static'))
    else:
        clear_directory(docs_directory)

def clear_directory(directory):
    """
    Clears the contents of the given directory, except for the 'static' subdirectory.

    Args:
        directory (str): Path to the directory to clear.

    Raises:
        OSError: If there's an issue removing the contents.
    """
    for item in os.listdir(directory):
        item_path = os.path.join(directory, item)
        if item == 'static':
            continue
        if os.path.islink(item_path) or os.path.isfile(item_path):
            os.unlink(item_path)
        elif os.path.isdir(item_path):
            shutil.rmtree(item_path)

def check_write_permissions(path):
    """
    Check if the program has write permissions in the specified path.

    Args:
        path (str): The path to check for write permissions.

    Returns:
        bool: True if write permissions are available, False otherwise.
    """
    try:
        testfile = tempfile.TemporaryFile(dir=path)
        testfile.close()
    except (IOError, OSError):
        return False
    return True

def generate_file_pages(directory_tree: DirectoryTree, template_dir: str = 'templates', base_dir: str = ''):
    """
    Generates HTML files for a given directory tree structure.

    This function creates HTML files for each file in the directory tree,
    maintaining the directory structure in the output. It also generates
    an index page for the root of the documentation.

    Args:
        directory_tree (DirectoryTree): The directory tree structure to generate HTML files for.
        template_dir (str, optional): The directory containing HTML templates. Defaults to 'templates'.
        base_dir (str, optional): The base directory of the source files. Defaults to an empty string.

    Returns:
        None

    Raises:
        jinja2.exceptions.TemplateNotFound: If the template file is not found in the template directory.
        OSError: If there are issues reading source files or writing output files.

    Note:
        - The function uses Jinja2 for templating.
        - Output HTML files are created in a 'docs' directory, mirroring the original directory structure.
        - An index.html file is created at the root of the 'docs' directory.
    """
    env = Environment(loader=FileSystemLoader(template_dir))
    template = env.get_template('file_template.html')

    def generate_html_recursively(node: Union[DirectoryTree, str], current_path: Path):
        if isinstance(node, str):
            # This is a file
            file_path = Path(os.path.join(base_dir, node))
            file_name = file_path.name
            target_directory = Path('docs') / file_path.parent.relative_to(base_dir) if base_dir else Path('docs') / file_path.parent
            target_directory.mkdir(parents=True, exist_ok=True)
            # Calculate relative path
            relative_path = '../' * len(current_path.parts)
            # Read the file
            with open(file_path, 'r', encoding='utf-8', errors='replace') as file:
                code = file.read()
            # Render the template
            output = template.render(
                sidebar_data=directory_tree,
                code=code,
                file=file_name,
                relative_path=relative_path,
                is_index=False,
                content_data=''
            )
            # Write the output
            output_path = target_directory / f'{file_path.stem}.html'
            with open(output_path, 'w', encoding='utf-8') as file:
                file.write(output)
        else:
            # This is a directory
            for child in node.children:
                generate_html_recursively(child, current_path / node.name)

    # Generate regular file pages
    generate_html_recursively(directory_tree, Path())

    # Generate index page
    index_output = template.render(
        sidebar_data=directory_tree,
        content_data='Welcome to your code!',
        relative_path='',
        is_index=True,
        file='',
        code=''
    )
    with open('docs/index.html', 'w', encoding='utf-8') as file:
        file.write(index_output)

# 1. **Error handling**: Some functions, like `build_directory_tree`, have extensive error handling and printing,
# while others, like `generate_html_files`, don't have any error handling or logging.
# Consistent error handling and logging across the codebase would improve maintainability and debugging.

# 2. **Configuration options**: Some values, like the template directory (`templates`)
# and the base directory for source files (`base_dir`), are hard-coded. Allowing these values
# to be configured through command-line arguments or a configuration file would make the script more flexible and reusable.

# 3. **Separation of concerns**: The `generate_html_files` function is responsible for both
# generating the HTML files and rendering the Jinja2 template. Separating these concerns into
# different functions or classes could improve the code's modularity and readability.

# 4. **Docstring formatting**: The docstrings follow the Google style guide, but some of them
# could be formatted more consistently, especially for the description and the examples.

# 5. **Potential performance issues**: The script might face performance issues when dealing with
# large directories or files, as it reads the entire file content into memory.
# Implementing a streaming approach or using a more efficient way of reading and writing files could improve performance.
