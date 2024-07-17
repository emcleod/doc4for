import re
from typing import Any, List, Optional, Union
from fparser.one.block_statements import (
    Comment,
    SpecificBinding,
    FinalBinding,
    GenericBinding,
    ModuleProcedure
)
from fparser.one.typedecl_statements import TypeDeclarationStatement
from doc4for.data_models import (
    TypeDescription,
    ProcedureDescription,
    GenericInterface,
    DataComponent,
    Dimension
)
from doc4for.comment_utils import is_doc4for_comment, format_comments

def update_type_with_parsed_data(type: Any, type_info: TypeDescription) -> None:
    type_info['type_name'] = type.name
    comment_stack = []
    for item in type.content:
        if isinstance(item, Comment):
            comment_stack.append(item)
        elif isinstance(item, SpecificBinding):
            procedure_description: ProcedureDescription = {
                'name': item.name,
                'description': format_comments(comment_stack) if is_doc4for_comment(comment_stack) else '',
                'attributes': [attr.lower() for attr in item.attrs],
                'is_final': False,
            }
            type_info['procedures'][item.name] = procedure_description
            comment_stack = []
        elif isinstance(item, ModuleProcedure):
            for name in item.items:
                procedure_description: ProcedureDescription = {
                    'name': name,
                    'description': format_comments(comment_stack) if is_doc4for_comment(comment_stack) else '',
                    'attributes': [], #TODO
                    'is_final': False
                }
                type_info['procedures'][name] = procedure_description
            comment_stack = []
        elif isinstance(item, GenericBinding):
            generic_description: GenericInterface = {
                'generic_spec': item.spec,
                'attributes': [item.aspec.lower()],
                'specific_procedures': item.items,
                'description': format_comments(comment_stack) if is_doc4for_comment(comment_stack) else '',
           }
            type_info['generic_interfaces'][item.spec] = generic_description
            comment_stack = []
        elif isinstance(item, FinalBinding):
            for final_name in item.items:
                procedure_description: ProcedureDescription = {
                    'name': final_name,
                    'description': format_comments(comment_stack) if is_doc4for_comment(comment_stack) else '',
                    'attributes': ['final'],
                    'is_final': True,
                }
                type_info['procedures'][final_name] = procedure_description
            comment_stack = []
        elif isinstance(item, TypeDeclarationStatement):
            for entity_decl in item.entity_decls:
                name, initial_value = get_name_and_initial_value(entity_decl)
                data_component_description: DataComponent = {
                    'name': name,
                    'type': item.name,
                    'kind': extract_kind(item.raw_selector),
                    'len': extract_len(item.raw_selector),
                    'description': format_comments(comment_stack) if is_doc4for_comment(comment_stack) else '',
                    'dimension': extract_dimension(entity_decl, item.attrspec),
                    'initial_value': initial_value,
                    'attributes': [attr.lower() for attr in item.attrspec if 'dimension' not in attr]
                }
                type_info['data_components'][name] = data_component_description
            comment_stack = []
        else:
            comment_stack = []

def get_name_and_initial_value(entity_decl):
    match = re.search(r'(\w+)\s*(?:=\s*(.+))?$', entity_decl)
    if match:
        name = match.group(1)
        value = match.group(2)
        return name.strip(), value.strip() if value else None
    words = entity_decl.split()
    if ':' in words[-1]:
        return re.sub(r'\s*\(\s*(:(?:\s*,\s*:)*)\s*\)\s*$', '', words[-1]), None
    return words[-1].strip(), None

def extract_dimension(entity_decl, attributes: List[str]) -> Optional[Dimension]:
    def extract_dimension_values(s: str) -> Union[Dimension, None]:
        match = re.search(r'\((.*?)\)$', s)
        if match:
            dimensions = match.group(1).split(',')
            return {'dimensions': [d.strip() if d.strip() != ':' else ':' for d in dimensions]}
        return None
    if ':' in entity_decl:
            return extract_dimension_values(entity_decl)
    for attr in attributes:
        if attr.startswith('dimension(') and attr.endswith(')'):
            dim_str = attr[len('dimension('):-1]
            dimensions = []
            for dim in dim_str.split(','):
                dim = dim.strip()
                if dim.isdigit():
                    dimensions.append(int(dim))
                elif dim == ':':
                    dimensions.append(dim)  
                else:
                    return None            
            return {'dimensions': dimensions}
        
def extract_kind(type_spec: str) -> Optional[str]:
    match = re.search(r'\(kind\s*=\s*(\w+)\)', type_spec)
    if match:
        return match.group(1)
    return None

def extract_len(type_spec: str) -> Optional[str]:
    match = re.search(r'\blen\s*=\s*(\d+|:)', type_spec)
    if match:
        return match.group(1)
    return None
