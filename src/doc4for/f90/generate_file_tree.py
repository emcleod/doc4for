import os
import logging
from pathlib import Path
from typing import List, Union, Optional, Iterator, Any, Dict
from jinja2 import Environment, FileSystemLoader
from fparser.api import parse as fortran_parser  # type: ignore
from fparser.one.block_statements import (
    Module,
    Comment,
    Function,
    Subroutine,
    Program,
    BlockData
)
from doc4for.data_models import FileDescription
from doc4for.comment_utils import is_doc4for_comment, format_comments
from doc4for.f90.populate_data_models import (
  parse_function, 
  parse_subroutine, 
  parse_program, 
  parse_module,
  parse_block_data
)

logger = logging.getLogger(__name__)

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
        # Normalize the path to use forward slashes
        # TODO is this the right thing to do
        normalized_path = Path(str(path).replace('\\', '/'))
        parts = normalized_path.parts
        current = self
        for part in parts[:-1]:
            child = next((c for c in current.children if isinstance(c, DirectoryTree) and c.name == part), None)
            if child is None:
                child = DirectoryTree(part, current)
                current.children.append(child)
            current = child
        current.children.append(str(str(path).replace('\\', '/')))

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

def build_directory_tree(files: List[Path]) -> DirectoryTree:
    """Builds a directory tree from a list of file paths.

    Args:
        files: A list of file paths as Path objects.

    Returns:
        A `DirectoryTree` object representing the directory structure.

    Raises:
        MemoryError: If the file list is too large and causes a memory error.
        Exception: If an unexpected error occurs during the building process.

    The function takes a list of Path objects and constructs a directory tree based on the paths.
    It handles both Unix-style and Windows-style paths automatically through the Path objects.

    If an error occurs while adding a path to the directory tree, a warning message is printed,
    and the path is skipped.

    The function returns a `DirectoryTree` object representing the directory structure.

    If a `MemoryError` occurs due to a large file list, an error message is printed,
    and the exception is re-raised.

    If any other unexpected exception occurs, an error message is printed, and the exception
    is re-raised.

    Example:
        files = [
            Path('/path/to/file1.txt'),
            Path('/path/to/file2.txt'),
            Path('/another/path/file3.txt'),
            Path('C:/Windows/path/file4.txt')
        ]
        directory_tree = build_directory_tree(files)
        for item in directory_tree.walk():
            print(item)
    """
    try:
        directory_tree = DirectoryTree('')
        for file_path in files:
            try:
                directory_tree.add_path(file_path)
            except Exception as e:
                print(f'Error adding path: {file_path}. Skipping. Error: {e}')
                continue
        return directory_tree
    except MemoryError as e:
        print(f'Memory error occurred. The file list might be too large: {e}.')
        raise
    except Exception as e:
        print(f'An unexpected error occurred: {e}')
        raise

def extract_file_data(f90_files: List[Path]) -> List[FileDescription]:
    files: List[FileDescription] = []
    for f90_file in f90_files:
        comment_stack: List[Comment] = []
        f90_file_str = os.fspath(f90_file)
        tree: Any = fortran_parser(f90_file_str, ignore_comments=False)
        file_data: FileDescription = {
            'file_name': f90_file_str,
            'file_description': '',
            'functions': {},
            'subroutines': {},
            'modules': {},
            'programs': {},
            'block_data': {}
        }
        first_non_comment_node = True
        for child in tree.content:
            if isinstance(child, Comment) and child.content:
                comment_stack.append(child)
            else:
                if first_non_comment_node and comment_stack and is_doc4for_comment(comment_stack):
                    file_data['file_description'] = format_comments(comment_stack)
                    comment_stack.clear()
                first_non_comment_node = False
                match child:
                    case Module():
                        file_data['modules'][child.name] = parse_module(child, comment_stack, f90_file_str)
                    case Function():
                        file_data['functions'][child.name] = parse_function(child, comment_stack)
                    case Subroutine():
                        file_data['subroutines'][child.name] = parse_subroutine(child, comment_stack)
                    case Program():
                        file_data['programs'][child.name] = parse_program(child, comment_stack, f90_file_str)
                    case BlockData():
                        file_data['block_data'][child.name] = parse_block_data(child, comment_stack)
                    case _:
                        pass
                comment_stack.clear()
        files.append(file_data)
        comment_stack.clear()
    return files

def generate_file_pages(directory_tree: DirectoryTree,
                        file_data: Dict[str, FileDescription],
                        template_dir: str = 'templates', 
                        base_dir: str = ''):
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
    template = env.get_template('file_template.html') #TODO remove hardcoding
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
                content_data='',
                file_data = file_data[node]
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
        code='',
        file_data = {}
    )
    with open('docs/index.html', 'w', encoding='utf-8') as file:
        file.write(index_output)

# TODO check docstring formatting
# TODO remove duplicated code in this and module tree generator for directory creation
