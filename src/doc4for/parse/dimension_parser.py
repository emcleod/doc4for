from typing import List, Optional
from doc4for.models.common import Expression
from doc4for.models.dimension_models import Dimension, ArrayBound
from doc4for.parse.common_parser import (
    parse_expression,    
    Expression,
    ExpressionType,
    handle_dimension_errors
)

def extract_dimension_string(name: str) -> Optional[str]:
    """Extract the dimension specification string from a variable name.
    
    Args:
        name (str): Variable name potentially including dimension specification
        
    Returns:
        Optional[str]: The dimension string if present, None otherwise
        
    Examples:
        "x(10)"     -> "10"
        "x(1:10)"   -> "1:10"
        "x"         -> None
    """
    if "(" not in name or ")" not in name:
        return None
    
    start = name.index('(')
    end = find_matching_parenthesis(name, start)
    if end == -1:
        return None
    
    return name[start + 1: end]

def find_matching_parenthesis(s: str, start: int = 0) -> int:
    """Find the index of the matching closing parenthesis.

    Args:
        s: The string to search in
        start: The index of the opening parenthesis

    Returns:
        The index of the matching closing parenthesis, or -1 if not found
    """
    count = 0
    for i in range(start, len(s)):
        if s[i] == '(':
            count += 1
        elif s[i] == ')':
            count -= 1
            if count == 0:
                return i
    return -1

def split_dimensions(s: str) -> List[str]:
    """Split dimension specifications, respecting nested parentheses."""
    result = []
    current = ""
    paren_count = 0
    
    for char in s:
        if char == ',' and paren_count == 0:
            result.append(current.strip())
            current = ""
        else:
            current += char
            if char == '(':
                paren_count += 1
            elif char == ')':
                paren_count -= 1
    
    result.append(current.strip())
    
    return [':' if not dim else dim for dim in result]

def extract_dimension_from_attributes(attributes: List[str]) -> Optional[Dimension]:
    """Extract dimension information from a list of attributes."""
    for attr in attributes:
        if attr.startswith("dimension(") and attr.endswith(")"):
            dim_str = attr[10:-1]  # 10 is len("dimension(")
            return extract_variable_dimension(f"x({dim_str})")
    return None

@handle_dimension_errors
def extract_variable_dimension(name: str) -> Optional[Dimension]:
    """Extract dimension information from a variable name with array specification.

    Args:
        name (str): Variable name potentially including dimension specification

    Returns:
        Optional[Dimension]: The dimension information if present, None otherwise

    Examples:
        Simple dimensions:
            "x(10)"     -> {'dimensions': [{'lower': '1', 'upper': '10'}]}
            "x(10,20)"  -> {'dimensions': [{'lower': '1', 'upper': '10'}, 
                                        {'lower': '1', 'upper': '20'}]}

        Explicit bounds:
            "x(0:10)"   -> {'dimensions': [{'lower': '0', 'upper': '10'}]}
            "x(-5:5)"   -> {'dimensions': [{'lower': '-5', 'upper': '5'}]}

        Variable bounds:
            "x(n)"      -> {'dimensions': [{'lower': '1', 'upper': 'n'}]}
            "x(-n:n)"   -> {'dimensions': [{'lower': '-n', 'upper': 'n'}]}

        Expressions:
            "x(2*n+1)"  -> {'dimensions': [{'lower': '1', 'upper': '2*n+1'}]}
            "x(0:n-1)"  -> {'dimensions': [{'lower': '0', 'upper': 'n-1'}]}

        Allocatable arrays:
            "x(:)"      -> {'dimensions': [{'lower': None, 'upper': None}]}
            "x(:,:)"    -> {'dimensions': [{'lower': None, 'upper': None},
                                        {'lower': None, 'upper': None}]}

        Not an array:
            "x"         -> None
    """
    dim_str = extract_dimension_string(name)
    if dim_str is None:
        return None

    dims = [parse_dimension_spec(d) for d in split_dimensions(dim_str)]
    return {"dimensions": dims} if dims else None

def parse_dimension_spec(spec: str) -> ArrayBound:
    """Parse a single dimension specification into an ArrayBound."""
    spec = spec.strip()

    # assumed size, first index is 1 with no upper bound
    if spec == '*':
        return {"lower": Expression(ExpressionType.LITERAL, "1"), "upper": None, "stride": None}

    # allocatable, no explicit dimensions
    if spec == ':':
        return {"lower": None, "upper": None, "stride": None}

    if ':' in spec:
        parts = [p.strip() for p in spec.split(':')]
        if len(parts) == 2:
            lower, upper = parts
            return {
                "lower": parse_expression(lower) if lower else None,
                "upper": parse_expression(upper) if upper else None,
                "stride": None
            }
        elif len(parts) == 3:
            lower, upper, stride = parts
            return {
                "lower": parse_expression(lower) if lower else None,
                "upper": parse_expression(upper) if upper else None,
                "stride": parse_expression(stride) if stride else None
            }

    return {
        "lower": Expression(ExpressionType.LITERAL, "1"),
        "upper": parse_expression(spec),
        "stride": None
    }

