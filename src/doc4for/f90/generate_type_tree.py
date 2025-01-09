import os
from typing import List, Union, TypedDict, Dict, Any, cast, Literal, Optional
from pathlib import Path
from fparser.one.block_statements import Type
from fparser.api import parse as fortran_parser  # type: ignore
from jinja2 import Environment, FileSystemLoader, Template

UNKNOWN_TYPE = 'UNKNOWN'
UnknownType = Literal['UNKNOWN']

class TypeInfo(TypedDict):
    children: List[Union[str, UnknownType]]
    parent: Union[str, None, UnknownType]

InheritanceTree = Dict[str, TypeInfo]

def process_type(child: Any, tree: InheritanceTree) -> None:
    type_name = child.name
    if type_name not in tree:
        tree[type_name] = {'children': [], 'parent': None}
    
    # Check for extends in the item attribute
    if hasattr(child, 'item'):
        item_str = str(child.item)
        if 'extends' in item_str:
            parent_name = item_str.split('extends(')[1].split(')')[0]
            tree[type_name]['parent'] = parent_name
            if parent_name not in tree:
                tree[parent_name] = cast(TypeInfo, {
                    'children': [type_name],
                    'parent': None
                })
            else:
                tree[parent_name]['children'].append(type_name)

def explore_node(node: Any, tree: InheritanceTree) -> None:
    if hasattr(node, 'content'):
        for child in node.content:
            if isinstance(child, Type):
                process_type(child, tree)
            else:
                explore_node(child, tree)

def generate_inheritance_tree(f90_files: List[Path]) -> InheritanceTree:
    tree: InheritanceTree = {}
    for f90_file in f90_files:
        f90_file_str = os.fspath(f90_file)
        program: Any = fortran_parser(f90_file_str, ignore_comments=False)
        explore_node(program, tree)
    for type_name, type_info in tree.items():
        if type_info['parent'] == UNKNOWN_TYPE:
            parent_name = type_info['parent']
            if parent_name in tree:
                tree[type_name]['parent'] = parent_name
            else:
                tree[type_name]['parent'] = None
        for i, child_name in enumerate(type_info['children']):
            if child_name == UNKNOWN_TYPE:
                if child_name in tree:
                    tree[type_name]['children'][i] = child_name
                else:
                    tree[type_name]['children'][i] = cast(UnknownType, UNKNOWN_TYPE)
    return tree

def transform_inheritance_tree(inheritance_tree: Dict[str, Any]) -> List[Dict[str, Any]]:
    def build_tree(type_name: str, processed: set) -> Optional[Dict[str, Any]]:
        if type_name in processed:
            return None
        processed.add(type_name)
        
        node = {"name": type_name, "children": []}
        if type_name in inheritance_tree:
            for child in inheritance_tree[type_name]["children"]:
                child_node = build_tree(child, processed)
                if child_node:
                    node["children"].append(child_node)
        return node

    root_nodes = []
    processed = set()

    # First, process types with no parent
    for type_name, type_info in inheritance_tree.items():
        if type_info["parent"] is None:
            root_node = build_tree(type_name, processed)
            if root_node:
                root_nodes.append(root_node)

    # Then, process any remaining types
    for type_name in inheritance_tree:
        if type_name not in processed:
            root_node = build_tree(type_name, processed)
            if root_node:
                root_nodes.append(root_node)

    return root_nodes

def generate_inheritance_tree_page(
    inheritance_tree: Dict[str, Any],
    template_dir: str,
    inheritance_tree_template: str,
    output_dir: str
) -> None:
    """
    Generates an HTML page displaying the inheritance tree.

    Args:
        inheritance_tree (Dict[str, Any]): The inheritance tree data structure.
        template_dir (str): The directory containing HTML templates.
        inheritance_tree_template (str): The name of the inheritance tree template file.
        output_dir (str): The directory to output the HTML file.

    Returns:
        None

    Raises:
        jinja2.exceptions.TemplateNotFound: If the template file is not found in the template directory.
        OSError: If there are issues writing the output file.
    """
    env: Environment = Environment(loader=FileSystemLoader(template_dir))
    template: Template = env.get_template(inheritance_tree_template)

    # Transform the inheritance tree
    transformed_tree: Dict[str, Any] = transform_inheritance_tree(inheritance_tree)

    # Render the template
    output: str = template.render(
        inheritance_tree=transformed_tree,
        title="Inheritance Tree"
    )

    # Ensure the output directory exists
    os.makedirs(output_dir, exist_ok=True)

    # Write the output
    output_path: str = os.path.join(output_dir, 'inheritance_tree.html')
    with open(output_path, 'w', encoding='utf-8') as file:
        file.write(output)

    print(f"Inheritance tree page generated at {output_path}")