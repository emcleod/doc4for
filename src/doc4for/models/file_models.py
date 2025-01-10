from typing import TypedDict, Dict
from doc4for.models.module_models import BlockDataDescription, ProgramDescription, ModuleDescription
from doc4for.models.procedure_models import FunctionDescription, SubroutineDescription

FileDescription = TypedDict(
    "FileDescription",
    {
        "file_name": str,
        "file_description": str,
        "functions": Dict[str, FunctionDescription],
        "subroutines": Dict[str, SubroutineDescription],
        "modules": Dict[str, ModuleDescription],
        "programs": Dict[str, ProgramDescription],
        "block_data": Dict[str, BlockDataDescription],
    },
)