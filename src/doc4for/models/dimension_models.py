from typing import TypedDict, Optional, List, Union
from enum import Enum
from dataclasses import dataclass
from doc4for.models.common import Expression


class BoundType(Enum):
    FIXED = "fixed",
    VARIABLE = "variable",
    ASSUMED_SHAPE = "assumed shape"
    ASSUMED_SIZE = "assumed size",
    ASSUMED_RANK = "assumed rank"


@dataclass
class ArrayBound:
    bound_type: BoundType
    lower: Optional[Expression] = None  # Expression for the lower bound
    upper: Optional[Expression] = None  # Expression for the upper bound

    def __str__(self):
        if self.bound_type == BoundType.ASSUMED_SIZE:
            return "* (assumed size)"
        elif self.bound_type == BoundType.ASSUMED_SHAPE:
            return ": (assumed shape)"
        elif self.bound_type == BoundType.ASSUMED_RANK:
            return ".. (assumed rank)"
        else:
            lower = str(self.lower.value) if self.lower else "1"
            upper = str(self.upper.value) if self.upper else ""
            stride = f":{self.stride.value}" if self.stride else ""

            formatted = None
            if not upper:
                formatted = f"{lower}"
            elif self.bound_type == BoundType.VARIABLE:
                formatted = f"{lower}:{upper}"
            else:  # BoundType.FIXED
                formatted = f"{lower}:{upper}"
            return f"{formatted}:{stride}" if stride else formatted

#TODO doesn"t need to be a typed dict really
Dimension = TypedDict("Dimension", {
    "dimensions": List[ArrayBound]
})


def format_dimension(dimension: Dimension) -> str:
    if not dimension or not dimension["dimensions"]:
        return ""
    return " &times; ".join(str(bound) for bound in dimension["dimensions"])


# TODO usage in jinja:
# {% macro render_argument(arg) %}
#   <tr>
#     <td>{{ arg.name }}</td>
#     <td>{{ arg.type }}</td>
#     <td>{{ format_dimension(arg.dimension) }}</td>
#     <td>{{ arg.description }}</td>
#   </tr>
# {% endmacro %}

# env.filters["format_dimension"] = format_dimension

# or
# from jinja2 import Environment

# def render_argument(argument):
#     env = Environment()
#     env.filters["dimension_str"] = dimension_to_string

#     template = env.from_string("""
#         Type: {{ argument.type }}
#         {% if argument.dimension %}
#         Dimension: {{ argument.dimension|dimension_str }}
#         {% endif %}
#         Description: {{ argument.description }}
#     """)

#     return template.render(argument=argument)

# TODO replace all occurrences with Dimension
Dimension_TEMP = TypedDict(
    "Dimension_TEMP", {"dimensions": List[Union[int, str]]})
