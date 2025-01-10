from typing import TypedDict, Optional, List
from doc4for.models.dimension_models import Dimension, Dimension_TEMP

VariableDescription = TypedDict(
    "VariableDescription",
    {
        "description": str,
        "type": str,
        "name": str,
        "dimension": Optional[Dimension],
        "attributes": List[str],  # For public/private, etc.
        "kind": Optional[str],
        "initial_value": Optional[str],
        "length": Optional[str]  # for character lengths
    },
)

ParameterDescription = TypedDict(
    "ParameterDescription",
    {
        "description": str,
        "type": "str",
        "name": "str",
        "value": "str",
        "dimension": Optional[str],
        "attributes": List[str],  # For public/private, etc.
    },
)

DataComponent = TypedDict(
    "DataComponent",
    {
        "name": str,
        "type": str,
        "kind": Optional[str],
        "description": str,
        "dimension": Optional[Dimension_TEMP],
        "len": Optional[str],
        "initial_value": Optional[str],
        "attributes": List[str],
    },
)