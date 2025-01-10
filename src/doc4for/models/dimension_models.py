from typing import TypedDict, Optional, List, Union

ArrayBound = TypedDict("ArrayBound", {
    "lower": Optional[str],  # None for allocatable, should be 1 by default
    "upper": Optional[str],  # None for allocatable
    "stride": Optional[str]  # None if no stride is specified
}, total=False)

Dimension = TypedDict("Dimension", {
    "dimensions": List[ArrayBound]
})

# TODO replace all occurrences with Dimension
Dimension_TEMP = TypedDict("Dimension_TEMP", {"dimensions": List[Union[int, str]]})
