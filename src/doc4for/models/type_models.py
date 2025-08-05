from typing import TypedDict, List, Dict, Optional
from doc4for.models.procedure_models import ProcedureDescription
from doc4for.models.variable_models import DataComponent
from doc4for.models.common import EnumDescription, BindingType

#TODO add source file information to all of these

GenericInterface = TypedDict(
    "GenericInterface",
    {
        "generic_spec": str,
        "description": str,
        "attributes": List[str],
        "specific_procedures": List[str]
    },
)

TypeParameter = TypedDict(
    "TypeParameter",
    {
        "name": str,
        "type": str,
        "parameter_type": str,  # "KIND" or "LEN"
        "default": Optional[str],
        "description": str,
    },
)

TypeDescription = TypedDict(
    "TypeDescription",
    {
        "type_name": str,
        "attributes": List[str],
        "description": str,
        "data_components": Dict[str, DataComponent],
        "enums": Dict[str, EnumDescription],  
        "procedures": Dict[str, ProcedureDescription],
        "generic_interfaces": Dict[str, GenericInterface],
        "extends": Optional[str],
        "type_parameters": Dict[str, TypeParameter],
        "binding_type": Optional[BindingType]
    },
)
