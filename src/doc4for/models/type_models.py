from typing import TypedDict, List, Dict, Optional, Union
from doc4for.models.procedure_models import ProcedureDescription, FunctionDescription, SubroutineDescription
from doc4for.models.variable_models import DataComponent

GenericInterface = TypedDict(
    "GenericInterface",
    {
        "generic_spec": str,
        "description": str,
        "attributes": List[str],
        "specific_procedures": List[str],
    },
)

InterfaceDescription = TypedDict(
    "InterfaceDescription",
    {
        "name": Optional[str], # abstract interfaces don't have a name
        "description": str,
        "attributes": List[str],
        "operator_symbol": Optional[str],
        "procedures": Dict[str, Union[FunctionDescription, SubroutineDescription]],
        "module_procedure_names": List[str],
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
    },
)
