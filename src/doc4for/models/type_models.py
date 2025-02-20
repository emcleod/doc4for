from typing import TypedDict, List, Dict, Optional
from doc4for.models.procedure_models import ProcedureDescription
from doc4for.models.variable_models import DataComponent
from doc4for.models.common import BindingType

GenericInterface = TypedDict(
    "GenericInterface",
    {
        "generic_spec": str,
        "description": str,
        "attributes": List[str],
        "specific_procedures": List[str],
        "binding_type": Optional[BindingType]
    },
)

TypeDescription = TypedDict(
    "TypeDescription",
    {
        "type_name": str,
        "attributes": List[str],
        "description": str,
        "data_components": Dict[str, DataComponent],
        "procedures": Dict[str, ProcedureDescription],
        "generic_interfaces": Dict[str, GenericInterface],
        "extends": Optional[str],
        "binding_type": Optional[BindingType]
    },
)
