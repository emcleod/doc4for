import os
import logging
from pathlib import Path
from typing import List, Union, Optional, Iterator, Any, Dict, Set
from jinja2 import Environment, FileSystemLoader, Template
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

logger: logging.Logger = logging.getLogger(__name__)


class DirectoryTree:
    """Represents a directory tree structure."""

    def __init__(self, name: str, parent: Optional['DirectoryTree'] = None):
        self.name: str = name
        self.parent: Optional['DirectoryTree'] = parent
        self.children: List[Union[str, 'DirectoryTree']] = []

    def add_path(self, path: Path) -> None:
        normalized_path: Path = Path(str(path).replace('\\', '/'))
        parts: List[str] = normalized_path.parts
        current: DirectoryTree = self
        for part in parts[:-1]:
            child: Optional[DirectoryTree] = next((c for c in current.children if isinstance(c, DirectoryTree) and c.name == part), None)
            if child is None:
                child = DirectoryTree(part, current)
                current.children.append(child)
            current = child
        current.children.append(str(str(path).replace('\\', '/')))

    def walk(self) -> Iterator[Union[str, 'DirectoryTree']]:
        yield self.name
        for child in self.children:
            if isinstance(child, DirectoryTree):
                yield from child.walk()
            else:
                yield child


def build_directory_tree(files: List[Path]) -> DirectoryTree:
    try:
        directory_tree: DirectoryTree = DirectoryTree('')
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
        f90_file_str: str = os.fspath(f90_file)
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
        first_non_comment_node: bool = True
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
                        template_dir: str,
                        file_template: str,
                        output_dir: str,
                        base_dir: str = '') -> None:
    env: Environment = Environment(loader=FileSystemLoader(template_dir))
    template: Template = env.get_template(file_template)

    def generate_html_recursively(node: Union[DirectoryTree, str], current_path: Path) -> None:
        if isinstance(node, str):
            file_path: Path = Path(os.path.join(base_dir, node))
            file_name: str = file_path.name
            target_directory: Path = Path(output_dir) / file_path.parent.relative_to(base_dir) if base_dir else Path(output_dir) / file_path.parent
            target_directory.mkdir(parents=True, exist_ok=True)
            relative_path: str = '../' * len(current_path.parts)
            with open(file_path, 'r', encoding='utf-8', errors='replace') as file:
                code: str = file.read()
            output: str = template.render(
                sidebar_data=directory_tree,
                code=code,
                file=file_name,
                relative_path=relative_path,
                is_index=False,
                content_data='',
                file_data=file_data[node]
            )
            output_path: Path = target_directory / f'{file_path.stem}.html'
            with open(output_path, 'w', encoding='utf-8') as file:
                file.write(output)
        else:
            for child in node.children:
                generate_html_recursively(child, current_path / node.name)

    generate_html_recursively(directory_tree, Path())

    index_output: str = template.render(
        sidebar_data=directory_tree,
        content_data='Welcome to your code!',
        relative_path='',
        is_index=True,
        file='',
        code='',
        file_data={}
    )
    with open(os.path.join(output_dir, 'index.html'), 'w', encoding='utf-8') as file:
        file.write(index_output)
                
# TODO remove duplicated code in this and module tree generator for directory creation
