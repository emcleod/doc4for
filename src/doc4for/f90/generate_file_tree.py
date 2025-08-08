import os
import sys
import logging
from pathlib import Path
from typing import List, Union, Optional, Iterator, Dict, Tuple
from jinja2 import Environment, FileSystemLoader, Template
from fparser.common.readfortran import FortranFileReader
from fparser.two.parser import ParserFactory
from doc4for.models.file_models import FileDescription
from doc4for.parse.file_parser import parse_file_content
from doc4for.models.file_models import FileDescription

logger: logging.Logger = logging.getLogger(__name__)

try:
    from fparser.two import Fortran2008
    sys.modules["fparser.two.Fortran2008"] = Fortran2008
except ImportError:
    pass

class DirectoryTree:
    """Represents a directory tree structure."""

    def __init__(self, name: str, parent: Optional["DirectoryTree"] = None):
        self.name: str = name
        self.parent: Optional["DirectoryTree"] = parent
        self.children: List[Union[str, "DirectoryTree"]] = []

    def add_path(self, path: Path) -> None:
        normalized_path: Path = Path(str(path).replace("\\", "/"))
        parts: Tuple[str] = normalized_path.parts
        current: DirectoryTree = self
        for part in parts[:-1]:
            child: Optional[DirectoryTree] = next((c for c in current.children if isinstance(c, DirectoryTree) and c.name == part), None)
            if child is None:
                child = DirectoryTree(part, current)
                current.children.append(child)
            current = child
        current.children.append(str(str(path).replace("\\", "/")))

    def walk(self) -> Iterator[Union[str, "DirectoryTree"]]:
        yield self.name
        for child in self.children:
            if isinstance(child, DirectoryTree):
                yield from child.walk()
            else:
                yield child


def build_directory_tree(files: List[Path]) -> DirectoryTree:
    try:
        directory_tree: DirectoryTree = DirectoryTree("")
        for file_path in files:
            try:
                directory_tree.add_path(file_path)
            except Exception as e:
                print(f"Error adding path: {file_path}. Skipping. Error: {e}")
                continue
        return directory_tree
    except MemoryError as e:
        print(f"Memory error occurred. The file list might be too large: {e}.")
        raise
    except Exception as e:
        print(f"An unexpected error occurred: {e}")
        raise

#TODO see generate module tree - don"t initialise in this method
def extract_file_data(f90_files: List[Path]) -> List[FileDescription]:
    files: List[FileDescription] = []

    # Create a parser
    try:
        parser = ParserFactory().create(std="f2008")
    except:
        parser = ParserFactory().create(std="f2003")

    for f90_file in f90_files:
        f90_file_path: str = os.fspath(f90_file)

        reader = FortranFileReader(f90_file_path, ignore_comments=False)
        tree = parser(reader)

        file_data: FileDescription = {
            "file_name": f90_file_path,
            "file_description": "",
            "functions": {},
            "subroutines": {},
            "modules": {},
            "programs": {},
            "block_data": {},
            "equivalence": [],
            "external_procedures": {}
        }
        parse_file_content(tree, file_data)
        files.append(file_data)
    return files

def generate_file_pages(directory_tree: DirectoryTree,
                        file_data: Dict[str, FileDescription],
                        template_dir: str,
                        file_template: str,
                        output_dir: str,
                        base_dir: str = "") -> None:
    env: Environment = Environment(loader=FileSystemLoader(template_dir))
    template: Template = env.get_template(file_template)

    def generate_html_recursively(node: Union[DirectoryTree, str], current_path: Path) -> None:
        if isinstance(node, str):
            file_path: Path = Path(os.path.join(base_dir, node))
            file_name: str = file_path.name
            target_directory: Path = Path(output_dir) / file_path.parent.relative_to(base_dir) if base_dir else Path(output_dir) / file_path.parent
            target_directory.mkdir(parents=True, exist_ok=True)
            relative_path: str = "../" * len(current_path.parts)
            with open(file_path, "r", encoding="utf-8", errors="replace") as file:
                code: str = file.read()
            output: str = template.render(
                sidebar_data=directory_tree,
                code=code,
                file=file_name,
                relative_path=relative_path,
                is_index=False,
                content_data="",
                file_data=file_data[node]
            )
            output_path: Path = target_directory / f"{file_path.stem}.html"
            with open(output_path, "w", encoding="utf-8") as file:
                file.write(output)
        else:
            for child in node.children:
                generate_html_recursively(child, current_path / node.name)

    generate_html_recursively(directory_tree, Path())

    index_output: str = template.render(
        sidebar_data=directory_tree,
        content_data="Welcome to your code!",
        relative_path="",
        is_index=True,
        file="",
        code="",
        file_data={}
    )
    with open(os.path.join(output_dir, "index.html"), "w", encoding="utf-8") as file:
        file.write(index_output)
                
# TODO remove duplicated code in this and module tree generator for directory creation
