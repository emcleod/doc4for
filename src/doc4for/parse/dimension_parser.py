# dimension_parser.py - dimension-specific parsing
from typing import List, Optional
from doc4for.models.common import Expression, ExpressionType
from doc4for.models.dimension_models import Dimension, ArrayBound
from doc4for.parse.parsing_utils import (
    parse_expression,
    handle_dimension_errors,
    find_matching_parenthesis
)

def extract_dimension_string(name: str) -> Optional[str]:
    if "(" not in name or ")" not in name:
        return None
    
    start = name.index('(')
    end = find_matching_parenthesis(name, start)
    if end == -1:
        return None
    
    return name[start + 1: end]

def split_dimensions(s: str) -> List[str]:
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
    for attr in attributes:
        if attr.startswith("dimension(") and attr.endswith(")"):
            dim_str = attr[10:-1]
            return extract_variable_dimension(f"x({dim_str})")
    return None

@handle_dimension_errors
def extract_variable_dimension(name: str) -> Optional[Dimension]:
    dim_str = extract_dimension_string(name)
    if dim_str is None:
        return None

    dims = [parse_dimension_spec(d) for d in split_dimensions(dim_str)]
    return {"dimensions": dims} if dims else None

def parse_dimension_spec(spec: str) -> ArrayBound:
    spec = spec.strip()

    if spec == '*':
        return {"lower": Expression(ExpressionType.LITERAL, "1"), "upper": None, "stride": None}

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