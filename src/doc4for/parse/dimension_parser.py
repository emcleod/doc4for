from typing import List, Optional, Tuple
from doc4for.models.common import Expression, ExpressionType
from doc4for.models.dimension_models import Dimension, ArrayBound, BoundType
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

def parse_dimension_spec(spec: str, default_lower: str = "1") -> ArrayBound:
    """Parse a Fortran array dimension specification.

    Args:
        spec: The dimension specification string
        default_lower: Default lower bound for single-value specs (default: "1")

    Returns:
        ArrayBound object representing the dimension

    Raises:
        ValueError: If the specification is malformed
    """
    if not spec:
        raise ValueError("Empty dimension specification")

    spec = spec.strip()

    # Special cases
    if spec == '..':
        return ArrayBound(bound_type=BoundType.ASSUMED_RANK)
    if spec == ':':
        return ArrayBound(bound_type=BoundType.ALLOCATABLE)
    if spec == '*':
        return ArrayBound(bound_type=BoundType.ASSUMED)

    # Handle range specification
    if ':' in spec:
        parts = [p.strip() for p in spec.split(':')]
        if len(parts) > 3:
            raise ValueError(f"Invalid dimension specification: too many colons in {spec}")

        try:
            lower = parse_expression(parts[0]) if parts[0] else None
            upper = parse_expression(parts[1]) if len(parts) > 1 and parts[1] else None
            stride = parse_expression(parts[2]) if len(parts) > 2 and parts[2] else None
        except Exception as e:
            raise ValueError(f"Failed to parse expression in {spec}: {str(e)}")

        is_variable = any(is_variable_expression(bound) 
                         for bound in [lower, upper, stride])

        return ArrayBound(
            bound_type=BoundType.VARIABLE if is_variable else BoundType.FIXED,
            lower=lower,
            upper=upper,
            stride=stride
        )

    # Single bound case
    try:
        expr = parse_expression(spec)
    except Exception as e:
        raise ValueError(f"Failed to parse expression {spec}: {str(e)}")

    return ArrayBound(
        bound_type=BoundType.VARIABLE if is_variable_expression(expr) else BoundType.FIXED,
        lower=Expression(ExpressionType.LITERAL, default_lower),
        upper=expr,
        stride=None
    )

def is_variable_expression(expr: Optional[Expression]) -> bool:
    return expr is not None and (
        expr.expr_type == ExpressionType.VARIABLE or 
        expr.expr_type == ExpressionType.FUNCTION_CALL
    )

def extract_coarray_dimensions(spec: str) -> Optional[Dimension]:
    """Extract coarray dimensions from a variable name.
    
    Args:
        spec: The specification containing coarray dimensions (e.g., 'x[*]', 'y[3,*]')
        
    Returns:
        A Dimension object representing the coarray dimensions, or None if none processed
    """
    if '[' not in spec:
        return None
            
    # Remove the closing bracket and split dimensions
    coarray_dims = spec.strip('[]').split(',')    

    # Use existing parse_dimension_spec for each dimension
    dims = [parse_dimension_spec(d.strip()) for d in coarray_dims]
    
    return {"dimensions": dims} if dims else None