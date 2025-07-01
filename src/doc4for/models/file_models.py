from typing import TypedDict, Dict, Optional, List
from doc4for.models.module_models import BlockDataDescription, ProgramDescription, ModuleDescription
from doc4for.models.procedure_models import FunctionDescription, SubroutineDescription
from doc4for.models.common import Uses, EquivalenceRelationship

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
        "equivalence": Optional[List[EquivalenceRelationship]], # optional because this is obsolete
        "uses": Dict[str, Uses],
    },
)

# include statements
# global parameters + variables
# common blocks
# interfaces
# data statements (data x, y /1.0, 2.0/)
# preprocessing
# external declarations (external :: external_sub)
# C interopability declarations
# Compiler directives
# type definitions?
# namelist declarations (namelist /my_list/ var1, var2)
