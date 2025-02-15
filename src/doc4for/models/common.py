from typing import Optional, List
from enum import Enum
from dataclasses import dataclass

ANNOTATION_PREFIX = "@"
"""Prefix used to identify annotations in comments."""

"""Prefix used to mark the start of a section to be ignored."""
IGNORE_PREFIX = "!*"

IGNORE_SUFFIX = "*!"
"""Suffix used to mark the end of a section to be ignored."""

class ExpressionType(Enum):
    LITERAL = "literal"
    VARIABLE = "variable"
    FUNCTION_CALL = "function_call"

@dataclass
class Expression:
    expr_type: ExpressionType
    value: str
    function_name: Optional[str] = None
    arguments: Optional[List['Expression']] = None

    def __str__(self):
        return f"Expression({self.expr_type}, {self.value}, {self.function_name if self.function_name else ''}, {self.arguments if self.arguments else ''})"
    

ARGUMENT_PATTERN = r'''
    (?P<var_name>\w+)      # Variable name
    \s+                    # Required whitespace
    (?P<description>.+)    # Description (rest of the line)
'''

