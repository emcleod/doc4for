from typing import List, Type
import re
from fparser.one.block_statements import (
    Module,
    Comment,
    Function,
    Subroutine,
    Program,
    Public,
    Type,
    Use
)
from fparser.one.typedecl_statements import TypeDeclarationStatement
from doc4for.data_models import (
    FunctionDescription,
    SubroutineDescription,
    ParameterDescription,
    TypeDescription,
    ModuleDescription,
    ProgramDescription,
    Uses
)
from doc4for.comment_utils import is_doc4for_comment, format_comments
from doc4for.argument_utils import update_arguments_with_comment_data, update_arguments_with_parsed_data
from doc4for.type_utils import update_type_with_parsed_data

def parse_program(program: Program, comment_stack: List[Comment], file_name: str) -> ProgramDescription:
    program_details: ProgramDescription = {
        'program_name': program.name,
        'file_name': file_name,
        'program_description': '',
        'uses': {}
    }
    if is_doc4for_comment(comment_stack):
        program_details['program_description'] = format_comments(comment_stack)
    for program_child in program.content:
        if isinstance(program_child, Use):
            module_name = program_child.name
            if module_name not in program_details['uses']:
                uses: Uses = {
                    'module_name': program_child.name,
                    'selections': []
                }
                program_details['uses'][module_name] = uses
            if not program_child.items and program_details['uses'][module_name]['selections']:
                # everything in the module is used, so any selections are overwritten
                program_details['uses'][module_name]['selections'] = []
            else:
                program_details['uses'][module_name]['selections'].extend(program_child.items)
    return program_details

def parse_module(module: Module, comment_stack: List[Comment], file_name: str) -> ModuleDescription:
    module_data: ModuleDescription = {
        'module_name': module.name,
        'parameters': {},
        'functions': {},
        'subroutines': {},
        'types': {},
        'file_name': file_name,
        'module_description': ''
    }
    if is_doc4for_comment(comment_stack):
        module_data['module_description'] = format_comments(comment_stack)
    return module_data

def parse_function(function: Function, comment_stack: List[Comment]) -> FunctionDescription:
    attributes: List[str] = [attr.strip().lower() for attr in function.prefix.split() if attr.strip()]
    function_description: FunctionDescription = {
        'attributes': attributes,
        'description': '',
        'arguments': function.args,
        'in': {},
        'out': {},
        'return': {},
        'binding_type': '',
        'interface': ''
    }
    update_arguments_with_parsed_data(function, function_description)
    if comment_stack:
        update_arguments_with_comment_data(comment_stack, function_description)
    return function_description

def parse_subroutine(subroutine: Subroutine, comment_stack: List[Comment]) -> SubroutineDescription:
    attributes: List[str] = [attr.strip().lower() for attr in subroutine.prefix.split() if attr.strip()]
    subroutine_description: SubroutineDescription = {
        'attributes': attributes,
        'description': '',
        'arguments': subroutine.args,
        'in': {},
        'out': {},
        'binding_type': '',
        'interface': ''
    }
    update_arguments_with_parsed_data(subroutine, subroutine_description)
    if comment_stack:
        update_arguments_with_comment_data(comment_stack, subroutine_description)
    return subroutine_description

def parse_type(type: Type, comment_stack: List[Comment], public_declarations: List[str]) -> TypeDescription:
    type_name: str = type.name
    type_description: TypeDescription = {
        'type_name': type_name,
        'attributes': [],
        'description': '',
        'data_components': {},
        'procedures': {},
        'generic_interfaces': {},
        'extends': None
    }
    if any(spec.startswith('extends') for spec in type.specs):
        extends_spec = next(spec for spec in type.specs if spec.startswith('extends'))
        match = re.search(r'extends\s*\(\s*(\w+)\s*\)', extends_spec)
        if match:
            base_type = match.group(1)
            type_description['extends'] = base_type
    else:
        type_description['attributes'].extend(type.specs)
    if type_name in public_declarations:
        type_description['attributes'].append('public')
    update_type_with_parsed_data(type, type_description)
    if comment_stack:
        type_description['description'] = format_comments(comment_stack)
    return type_description

# TODO public declaration should be handled here?
def parse_parameter(declaration: TypeDeclarationStatement, comment_stack: List[Comment]) -> ParameterDescription:
    item_str = declaration.item.line
    name_match = re.search(r'::\s*(\w+)\s*=', item_str)
    if name_match:
        name = name_match.group(1)
        value = item_str.split('=', 1)[1].strip()
    else:
        # Handle cases where the parameter declaration format is not recognized
        name = ''
        value = ''
    parameter_description: ParameterDescription = {
        'description': '',  
        'type': str(declaration.name),
        'name': name.strip(),
        'value': value.strip(),
        'dimension': ''
    }
    if is_doc4for_comment(comment_stack):
        parameter_description['description'] = format_comments(comment_stack)
    return parameter_description