import logging
from dataclasses import dataclass, field
from typing import TypeVar, Generic, Dict, Type, Callable, List, Any
from fparser.one.block_statements import (
    Comment,
    Module,
    Function, 
    Subroutine,
    Interface,
    Program,
    BlockData,
    ModuleProcedure,
    Type as FortranType
)
from fparser.one.typedecl_statements import TypeDeclarationStatement
from doc4for.models.file_models import FileDescription
from doc4for.models.module_models import ModuleDescription
from doc4for.models.type_models import TypeDescription
from doc4for.parse.parameter_parser import parse_parameter, is_parameter
from doc4for.parse.procedure_parser import parse_subroutine, parse_function, parse_interface
from doc4for.f90.populate_data_models import (
    initialise_module_description,
    parse_type, 
    parse_variable,
    parse_block_data,
    parse_program
)


logger: logging.Logger = logging.getLogger(__name__)

T = TypeVar('T')

@dataclass
class VisibilityState:
    is_public: bool = True
    explicit_public: List[str] = field(default_factory=list)
    explicit_private: List[str] = field(default_factory=list)


class FortranHandler(Generic[T]):
    
    def __init__(self):
        self.handlers: Dict[Type, Callable[[Any, T, List[Comment]], None]] = {}

    def register_handler(self, item_type: Type,
                         handler: Callable[[Any, T, List[Comment]], None]) -> None:
        self.handlers[item_type] = handler
    
    def get_handler(self, item_type: Type) -> Callable[[Any, T, List[Comment]], None]:
        for handler_type, handler in self.handlers.items():
            if issubclass(item_type, handler_type):
                return handler
        return self.handle_other_item

    def handle_other_item(self, item: Any, data: T, comment_stack: List[Comment]) -> None:
        logger.warning("Unhandled type %s", type(item))


def handle_function(item: Function, data: T, comment_stack: List[Comment]) -> None:
    data['functions'][item.name] = parse_function(item, comment_stack)

def handle_subroutine(item: Subroutine, data: T, comment_stack: List[Comment]) -> None:
    data['subroutines'][item.name] = parse_subroutine(item, comment_stack)


def handle_module(item: Module, file_data: FileDescription,
                   comment_stack: List[Comment]):
    file_data['modules'][item.name] = initialise_module_description(item, comment_stack, file_data['file_name'])


def handle_program(item: Program, file_data: FileDescription,
                   comment_stack: List[Comment]) -> None:
    file_data['programs'][item.name] = parse_program(item, comment_stack, file_data['file_name'])

def handle_block_data(item: BlockData, file_data: FileDescription,
                      comment_stack: List[Comment]) -> None:
    file_data['block_data'][item.name] = parse_block_data(item, comment_stack)

def handle_type(item: FortranType, module_data: ModuleDescription,
                comment_stack: List[Comment]) -> None:
    type_description: TypeDescription = parse_type(item, comment_stack)
    module_data['types'][type_description['type_name']] = type_description

def handle_type_declaration(item: TypeDeclarationStatement, module_data: ModuleDescription,
                            comment_stack: List[Comment]) -> None:
    if is_parameter(item):
        parameter_descriptions = parse_parameter(item, comment_stack)
        for param in parameter_descriptions:
            module_data['parameters'][param['name']] = param
    else:
        variable_descriptions = parse_variable(item, comment_stack)
        for var in variable_descriptions:
            module_data['variables'][var['name']] = var                

def handle_interface(item: Interface, module_data: ModuleDescription,
                     comment_stack: List[Comment]) -> None:
    module_data['interfaces'].append(parse_interface(item, comment_stack))    