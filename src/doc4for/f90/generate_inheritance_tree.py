import os
from typing import List, Union, TypedDict, Dict, Any, cast, Literal
from pathlib import Path
from fparser.one.block_statements import Type
from fparser.api import parse as fortran_parser  # type: ignore

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
            print (child, type(child))
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