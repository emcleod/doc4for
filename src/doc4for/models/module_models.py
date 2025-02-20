from typing import TypedDict, Dict, List
from doc4for.models.variable_models import VariableDescription, ParameterDescription
from doc4for.models.procedure_models import FunctionDescription, InterfaceDescription, SubroutineDescription
from doc4for.models.type_models import TypeDescription

Uses = TypedDict("Uses", {"module_name": str, "selections": List[str]})

#TODO have a common block type rather than a dict of dicts
BlockDataDescription = TypedDict(
    "BlockDataDescription",
    {
        "name": str,
        "description": str,
        "common_blocks": Dict[str, Dict[str, VariableDescription]],
    },
)

ModuleDescription = TypedDict(
    "ModuleDescription",
    {
        "module_name": str,
        "parameters": Dict[str, ParameterDescription],
        "variables": Dict[str, VariableDescription],
        "functions": Dict[str, FunctionDescription],
        "subroutines": Dict[str, SubroutineDescription],
        "interfaces": List[InterfaceDescription],
        "types": Dict[str, TypeDescription],
        "file_name": str,
        "module_description": str,
    },
)


ProgramDescription = TypedDict(
    "ProgramDescription",
    {
        "program_name": str,
        "file_name": str,
        "program_description": str,
        "uses": Dict[str, Uses],
    },
)