import re
import logging
from dataclasses import dataclass, field
from typing import TypeVar, Generic, Dict, Type, Callable, List, Any, Tuple, Optional, Protocol
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
    Public,
    Enum,
    Enumerator,
    Use,
    Common
)
from fparser.two.Fortran2003 import (
    Type_Declaration_Stmt,
    Derived_Type_Def
)
from doc4for.models.common import BindingType, BindingTypeEnum
from doc4for.models.file_models import FileDescription
from doc4for.models.module_models import ModuleDescription
from doc4for.models.type_models import TypeDescription
from doc4for.models.procedure_models import ProcedureDescription
from doc4for.parse.parameter_parser import parse_parameter
from doc4for.parse.procedure_parser import parse_subroutine, parse_function, parse_interface
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
    if "types" not in data:
        data["types"] = {}
    type_desc: TypeDescription = handle_type_definition(item, comment_stack)
    data["types"][type_desc["type_name"]] = type_desc

def handle_type_declaration(item: Type_Declaration_Stmt, data: ModuleDescription,
                            comment_stack: List[Comment], **kwargs) -> None:
    dimension_stack = kwargs.get("dimension_stack", None)  

    if has_attribute(item, "PARAMETER"):
        parameter_descriptions = parse_parameter(item, comment_stack, dimension_stack)
        for param in parameter_descriptions:
            data["parameters"][param["name"]] = param
    else:
        variable_descriptions = parse_variable(item, comment_stack, dimension_stack)
        for var in variable_descriptions:
            data["variables"][var["name"]] = var                


def handle_function(item: Function, data: T, comment_stack: List[Comment]) -> None:
    data["functions"][item.name] = parse_function(item, comment_stack)

def handle_subroutine(item: Subroutine, data: T, comment_stack: List[Comment]) -> None:
    data["subroutines"][item.name] = parse_subroutine(item, comment_stack)

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




def handle_interface(item: Interface, data: ModuleDescription,
                     comment_stack: List[Comment]) -> None:
    data["interfaces"].append(parse_interface(item, comment_stack))    

def handle_common_block(item: Common, data: ModuleDescription,
                        comment_stack: List[Comment]) -> None:
    data["common_blocks"].append(parse_common_block(item, comment_stack))
    
def handle_enum(item: Enum, data: ModuleDescription,
                comment_stack: List[Comment]) -> None:
    
    enum_description = parse_enum(item, comment_stack)
    attributes, binding_type = extract_enum_attributes(item.item.line)
    enum_description["attributes"] = attributes
    enum_description["binding_type"] = binding_type
    
    # Find the name to use as key
    if not item.name or item.name == "__ENUM__":
        # Look explicitly for first enumerator"s name
        for content in item.content:
            if isinstance(content, Enumerator) and content.items:
                first_item = content.items[0]
                name = first_item.split("=")[0].strip() if "=" in first_item else first_item.strip()
                break
        else:
            # Fallback in case no enumerator is found
            name = "__ENUM__" + str(len(data["enums"]) + 1)
    else:
        name = item.name
    
    data["enums"][name] = enum_description
    
def handle_enum(item: Enum, data: ModuleDescription,
                comment_stack: List[Comment]) -> None:
    first_name, enum_description = parse_enum(item, comment_stack)
    attributes, binding_type = extract_enum_attributes(item.item.line)
    enum_description["attributes"] = attributes
    enum_description["binding_type"] = binding_type
    
    name = item.name if item.name and item.name != "__ENUM__" else first_name
    data["enums"][name] = enum_description
    
def extract_enum_attributes(line: str) -> Tuple[List[str], Optional[BindingType]]:
    # Remove any comments (everything after !)
    line = line.split("!")[0].strip()
    
    # Split on commas and remove "enum"
    parts = [part.strip() for part in line.split(",")]
    parts = [part for part in parts if part.lower() != "enum"]
    
    attributes = []
    binding_type = None
    
    for part in parts:
        if part.lower().startswith("bind"):
            # Extract binding information
            match = re.match(r'bind\s*\(\s*c\s*(?:,\s*name\s*=\s*[\""]([^\""]*)[\""])?\s*\)', part, re.IGNORECASE)
            if match:
                binding_name = match.group(1)  # Will be None if no name specified
                binding_type = {
                    "type": BindingTypeEnum.BIND_C,
                    "name": binding_name
                }
        else:
            # It"s a regular attribute
            attributes.append(part)
    
    return attributes, binding_type
