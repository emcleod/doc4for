from typing import TypedDict, Optional, List, Union
from enum import Enum
from dataclasses import dataclass
from doc4for.models.common import Expression

class BoundType(Enum):
    FIXED = "fixed",
    VARIABLE = "variable",
    ALLOCATABLE = "allocatable",
    ASSUMED = "assumed"

@dataclass
class ArrayBound:
    bound_type: BoundType
    lower: Optional[Expression] = None  # Expression for the lower bound
    upper: Optional[Expression] = None  # Expression for the upper bound
    stride: Optional[Expression] = None  # Expression for the stride

# ArrayBound = TypedDict("ArrayBound", {
#     "lower": Optional[str],  # None for allocatable, should be 1 by default
#     "upper": Optional[str],  # None for allocatable
#     "stride": Optional[str]  # None if no stride is specified
# }, total=False)

Dimension = TypedDict("Dimension", {
    "dimensions": List[ArrayBound]
})

# TODO replace all occurrences with Dimension
Dimension_TEMP = TypedDict("Dimension_TEMP", {"dimensions": List[Union[int, str]]})
