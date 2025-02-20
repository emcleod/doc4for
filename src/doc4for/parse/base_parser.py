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
    Type as FortranType,
    EndModule,
    EndType,
    Contains,
    Implicit,
    Private,
    Public
)
from fparser.one.typedecl_statements import TypeDeclarationStatement
from doc4for.models.file_models import FileDescription
from doc4for.models.module_models import ModuleDescription
from doc4for.models.type_models import TypeDescription
from doc4for.models.procedure_models import ProcedureDescription
from doc4for.parse.parameter_parser import parse_parameter, is_parameter
from doc4for.parse.procedure_parser import parse_subroutine, parse_function, parse_interface
from doc4for.f90.populate_data_models import (
    initialise_module_description, 
    parse_variable,
    parse_block_data,
    parse_program
)
from doc4for.utils.comment_utils import get_formatted_description

logger: logging.Logger = logging.getLogger(__name__)

T = TypeVar('T')

@dataclass
class VisibilityState:
    is_public: bool = True
    explicit_public: List[str] = field(default_factory=list)
    explicit_private: List[str] = field(default_factory=list)

class FortranHandler(Generic[T]):
    TYPE_HANDLING = {
        EndModule: None,
        EndType: None,
        Contains: None,
        Implicit: None,
        Comment: None,  # Though we shouldn't get here
        Private: 'WARNING',
        Public: 'WARNING',
    }

    def __init__(self):
        self.handlers: Dict[Type, Callable[[Any, T, List[Comment]], None]] = {}

    def register_handler(self, item_type: Type,
                         handler: Callable[[Any, T, List[Comment]], None]) -> None:
        self.handlers[item_type] = handler
 
    def get_handler(self, item_type: Type) -> Callable[[Any, T, List[Comment]], None]:
        # Check registered handlers first
        for handler_type, handler in self.handlers.items():
            if issubclass(item_type, handler_type):
                return handler
                
        # Then check TYPE_HANDLING
        for type_class, handling in self.TYPE_HANDLING.items():
            if issubclass(item_type, type_class):
                if handling is None:
                    return self.handle_ignored_item
                if handling == 'WARNING':
                    return self.handle_unimplemented_item
                    
        return self.handle_unimplemented_item
    
    def handle_unimplemented_item(self, item: Any, data: T, comment_stack: List[Comment]) -> None:
        logger.warning("Unimplemented type %s", type(item))
        
    def handle_ignored_item(self, item: Any, data: T, comment_stack: List[Comment]) -> None:
        logger.info("Ignored type %s", type(item))


def handle_function(item: Function, data: T, comment_stack: List[Comment]) -> None:
    data['functions'][item.name] = parse_function(item, comment_stack)

def handle_subroutine(item: Subroutine, data: T, comment_stack: List[Comment]) -> None:
    data['subroutines'][item.name] = parse_subroutine(item, comment_stack)

def handle_module(item: Module, file_data: FileDescription,
                   comment_stack: List[Comment]):
    file_data['modules'][item.name] = initialise_module_description(item, comment_stack, file_data['file_name'])


def handle_module_procedure(item: ModuleProcedure, data: T, 
                            comment_stack: List[Comment]) -> None:
   # TODO: attributes are missing
   for name in item.items:
       procedure_description: ProcedureDescription = {
           'name': name,
           'description': get_formatted_description(comment_stack),
           'attributes': [],
           'is_final': False,
           'bound_to': None
       }
       data['procedures'][name] = procedure_description


def handle_program(item: Program, data: FileDescription,
                   comment_stack: List[Comment]) -> None:
    data['programs'][item.name] = parse_program(item, comment_stack, data['file_name'])

def handle_block_data(item: BlockData, data: FileDescription,
                      comment_stack: List[Comment]) -> None:
    data['block_data'][item.name] = parse_block_data(item, comment_stack)

def handle_type(item: FortranType, data: ModuleDescription,
                comment_stack: List[Comment]) -> None:
    from doc4for.parse.type_parser import parse_type  # local import to break circular dependency
    type_description: TypeDescription = parse_type(item, comment_stack)
    data['types'][type_description['type_name']] = type_description

def handle_type_declaration(item: TypeDeclarationStatement, data: ModuleDescription,
                            comment_stack: List[Comment]) -> None:
    if is_parameter(item):
        parameter_descriptions = parse_parameter(item, comment_stack)
        for param in parameter_descriptions:
            data['parameters'][param['name']] = param
    else:
        variable_descriptions = parse_variable(item, comment_stack)
        for var in variable_descriptions:
            data['variables'][var['name']] = var                

def handle_interface(item: Interface, data: ModuleDescription,
                     comment_stack: List[Comment]) -> None:
    data['interfaces'].append(parse_interface(item, comment_stack))    

