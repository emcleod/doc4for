import re
import logging
from dataclasses import dataclass, field
from typing import TypeVar, Generic, Dict, Type, Callable, List, Any, Tuple, Optional, Protocol
from fparser.one.block_statements import (
    #Comment,
    Module,
    Program,
    BlockData,
    ModuleProcedure,
    Type as FortranType,
    EndModule,
    EndType,
    Contains,
    Implicit,
    Private,
    Public,
    Enum,
    Enumerator,
    Use,
    Common
)
from fparser.two.Fortran2003 import (
    Type_Declaration_Stmt,
    Derived_Type_Def,
    Function_Subprogram,
    Subroutine_Subprogram,
    Interface_Block,
    Comment,
    Enum_Def,
)
from fparser.two.utils import walk
from doc4for.models.common import BindingType, BindingTypeEnum
from doc4for.models.file_models import FileDescription
from doc4for.models.module_models import ModuleDescription
from doc4for.models.type_models import TypeDescription
from doc4for.models.procedure_models import ProcedureDescription
from doc4for.parse.parameter_parser import parse_parameter
from doc4for.parse.function_parser import parse_function
from doc4for.parse.subroutine_parser import parse_subroutine
from doc4for.parse.interface_parser import parse_interface
from doc4for.parse.enum_parser import parse_enum
from doc4for.parse.shared_data_parser import parse_block_data, parse_common_block
from doc4for.parse.type_parser import handle_type_definition
from doc4for.parse.variable_parser import parse_variable
from doc4for.f90.populate_data_models import initialise_module_description, parse_program
from doc4for.utils.comment_utils import get_formatted_description
from doc4for.utils.attribute_utils import has_attribute
from doc4for.parse.common_parser import FortranHandler

logger: logging.Logger = logging.getLogger(__name__)

T = TypeVar("T")

@dataclass
class VisibilityState:
    is_public: bool = True
    explicit_public: List[str] = field(default_factory=list)
    explicit_private: List[str] = field(default_factory=list)

def handle_derived_type(item: Derived_Type_Def, data: T, comment_stack: List[Comment], **kwargs: Any) -> None:
    # TODO is this just over-cautious?
    if "types" not in data:
        data["types"] = {}
    type_desc: TypeDescription = handle_type_definition(item, comment_stack)
    data["types"][type_desc["type_name"]] = type_desc

def handle_type_declaration(item: Type_Declaration_Stmt, data: T,
                            comment_stack: List[Comment], **kwargs: Any) -> None:
    dimension_stack = kwargs.get("dimension_stack", None)  

    if has_attribute(item, "PARAMETER"):
        parameter_descriptions = parse_parameter(item, comment_stack, dimension_stack)
        for param in parameter_descriptions:
            data["parameters"][param["name"]] = param
    else:
        variable_descriptions = parse_variable(item, comment_stack, dimension_stack)
        for var in variable_descriptions:
            data["variables"][var["name"]] = var                


def handle_function(item: Function_Subprogram, data: T, comment_stack: List[Comment], **kwargs: Any) -> None:
    function_name, function_description = parse_function(item, comment_stack)
    data["functions"][function_name] = function_description

def handle_subroutine(item: Subroutine_Subprogram, data: T, comment_stack: List[Comment], **kwargs: Any) -> None:
    subroutine_name, subroutine_description = parse_subroutine(item, comment_stack)
    data["subroutines"][subroutine_name] = subroutine_description


def handle_interface(item: Interface_Block, data: T, comment_stack: List[Comment], **kwargs: Any) -> None:
    interface_name, interface_description = parse_interface(item, comment_stack)
    data["interfaces"].append(interface_description)

def handle_enum(item: Enum_Def, data: T, comment_stack: List[Comment], **kwargs: Any) -> None:
    enumerator_name, enumerator_description = parse_enum(item, comment_stack)
    data["enums"][enumerator_name] = enumerator_description


#------------------------------------------- old stuff to be replaced
def handle_module(item: Module, file_data: FileDescription,
                   comment_stack: List[Comment]):
    file_data["modules"][item.name] = initialise_module_description(item, comment_stack, file_data["file_name"])


def handle_module_procedure(item: ModuleProcedure, data: T, 
                            comment_stack: List[Comment]) -> None:
   # TODO: attributes are missing
   for name in item.items:
       procedure_description: ProcedureDescription = {
           "name": name,
           "description": get_formatted_description(comment_stack),
           "attributes": [],
           "is_final": False,
           "bound_to": None
       }
       data["procedures"][name] = procedure_description


def handle_program(item: Program, data: FileDescription,
                   comment_stack: List[Comment]) -> None:
    data["programs"][item.name] = parse_program(item, comment_stack, data["file_name"])

def handle_block_data(item: BlockData, data: FileDescription,
                      comment_stack: List[Comment]) -> None:
    data["block_data"][item.name] = parse_block_data(item, comment_stack)


def handle_use(item: Use, data: T, comment_stack: List[Comment]) -> None:
    module_name = item.name  # The first item is the module name
    selections = item.items  # "items" contains any selections
    
    # Process selections to handle renames
    processed_selections = []
    if selections:
        for selection in selections:
            if "=>" in selection:
                # This is a renamed import
                local_name, original_name = [s.strip() for s in selection.split("=>", 1)]
                processed_selections.append({local_name: original_name})
            else:
                # This is a regular import
                processed_selections.append(selection)
    
    if module_name in data["uses"]:
        # already using something from this module
        if not selections:
            # if items is empty, it"s using everything, so remove selections
            data["uses"][module_name]["selections"] = []
        else:
            # otherwise, add new selections
            data["uses"][module_name]["selections"].extend(processed_selections)
    else:
        # first time we"ve seen this module name
        uses = {
            "module_name": module_name,
            "selections": processed_selections
        }
        # Add to the uses dictionary with the module name as the key
        data["uses"][module_name] = uses


def handle_common_block(item: Common, data: ModuleDescription,
                        comment_stack: List[Comment]) -> None:
    data["common_blocks"].append(parse_common_block(item, comment_stack))

