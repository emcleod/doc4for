from typing import TypedDict, Optional, List, Union
from enum import Enum
from dataclasses import dataclass
from doc4for.models.common import Expression

class BoundType(Enum):
    FIXED = "fixed",
    VARIABLE = "variable",
    ALLOCATABLE = "allocatable",
    ASSUMED = "assumed",
    ASSUMED_RANK = "assumed rank"

@dataclass
class ArrayBound:
    bound_type: BoundType
    lower: Optional[Expression] = None  # Expression for the lower bound
    upper: Optional[Expression] = None  # Expression for the upper bound
    stride: Optional[Expression] = None  # Expression for the stride

Dimension = TypedDict("Dimension", {
    "dimensions": List[ArrayBound]
})

# TODO replace all occurrences with Dimension
Dimension_TEMP = TypedDict("Dimension_TEMP", {"dimensions": List[Union[int, str]]})
