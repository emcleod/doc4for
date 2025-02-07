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
    

# This pattern matches:
# 1. @in x: real description
# 2. @in x:real description
# 3. @in x : real description
# 4. @in x :real description
# 5. @in x : real(10) description
# 6. @in x: real(10) description
# 7. @in x :real(10) description
# including coarrays
ARGUMENT_PATTERN = r'''
    (?P<var_name>\w+)      # Variable name (one or more word characters)
    \s*                    # Optional whitespace
    :                      # Colon
    \s*                    # Optional whitespace
    (?P<var_type>\w+       # Variable type
        (\s*\([^)]+\))?    # Optional array dimension in parentheses
        (?:\s*\[[^\]]+\])? # Optional coarray dimension in square brackets
    )
    \s+                    # Required whitespace
    (?P<description>.+)    # Description (rest of the line)
'''


# This pattern matches:
# 1. @return: real description
# 2. @return : real description
# 3. @return :real description
# 4. @return res: real description
# 5. @return res : real description
# 6. @return res :real description
# 7. @return res:real description
# 8. @return description (no type specified)
# plus coarrays
RETURN_PATTERN = r'''
    (?:                    # Non-capturing group for the optional name and type
        (?:                # Non-capturing group for the two main patterns
            (?:           
                (?P<return_name>\w+)  # Optional return name
                \s*                   # Optional whitespace
                :                     # Colon
                \s*                   # Optional whitespace
                (?P<return_type>\w+)  # Return type
                (?:\s*\([^)]+\))?     # Optional array dimension in parentheses
                (?:\s*\[[^\]]+\])?    # Optional array dimension in square brackets
            )
            |              # OR
            (?:           
                :          # Just a colon
                \s*       # Optional whitespace
                (?P<unnamed_type>\w+) # Return type without name
                (?:\s*\([^)]+\))?    # Optional array dimension in parentheses
                (?:\s*\[[^\]]+\])?   # Optional coarray dimension in square brackets
            )
        )
        \s+               # Required whitespace
    )?                    # All of the above is optional (for case 8)
    (?P<description>.+)   # Description (rest of the line)
'''

