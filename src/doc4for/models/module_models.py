from typing import TypedDict, Dict, List, Optional
from doc4for.models.variable_models import VariableDescription, ParameterDescription
from doc4for.models.procedure_models import FunctionDescription, InterfaceDescription, SubroutineDescription
from doc4for.models.type_models import TypeDescription
from doc4for.models.common import EnumDescription, Uses, BindingType

DataStatementDescription = TypedDict(
    "DataStatementDescription",
    {
        "variable": str,            
        "value": str,              
        "description": Optional[str],  
        "implied_initialisation": Optional[str]   
    }
)

CommonBlockDescription = TypedDict(
    "CommonBlockDescription",
    {
        "name": str,
        "variables": Dict[str, VariableDescription],
        "description": Optional[str],
        "binding_type": BindingType
    },
)


BlockDataDescription = TypedDict(
    "BlockDataDescription",
    {
        "name": str,
        "description": str,
        "common_blocks": Dict[str, CommonBlockDescription],
        "data_statements": List[DataStatementDescription],
        "other_variables": Dict[str, VariableDescription] # variables can be declared but not used in common blocks
        #TODO what about parameters that are used for initialisation
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
        "enums": Dict[str, EnumDescription],
        "types": Dict[str, TypeDescription],
        "uses": Dict[str, Uses], 
        "common_blocks": Dict[str, CommonBlockDescription],
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

