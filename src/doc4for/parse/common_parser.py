from typing import List
from functools import wraps
from fparser.one.typedecl_statements import TypeDeclarationStatement

from doc4for.models.common import (
    Expression,
    ExpressionType
)

def handle_dimension_errors(func):
    @wraps(func)
    def wrapper(*args, **kwargs):
        try:
            return func(*args, **kwargs)
        except Exception as e:
            # For now, just re-raise, but could log or handle differently in future
            raise
    return wrapper

def parse_expression(expr: str) -> Expression:
    if '(' in expr:
        # This is a function call
        fname = expr[:expr.index('(')]
        args_str = expr[expr.index('(')+1:expr.rindex(')')]
        args = [parse_expression(arg.strip()) for arg in split_args(args_str)]
        return Expression(
            expr_type=ExpressionType.FUNCTION_CALL,
            value=expr,
            function_name=fname,
            arguments=args
        )
    elif expr.isdigit() or expr[0] in '+-' and expr[1:].isdigit():
        # This is a literal
        return Expression(expr_type=ExpressionType.LITERAL, value=expr)
    else:
        # This is a variable
        return Expression(expr_type=ExpressionType.VARIABLE, value=expr)

def split_args(s: str) -> List[str]:
    """Split function arguments, respecting nested parentheses.

    Args:
        s: The argument string

    Returns:
        A list of individual arguments

    Examples:
        split_args("1, 2")            -> ["1", "2"]
        split_args("f(1,2), 3")       -> ["f(1,2)", "3"]
        split_args("1, g(2,h(3,4))") -> ["1", "g(2,h(3,4))"]
    """
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

    if current:
        result.append(current.strip())
    return result

def get_attributes(declaration: TypeDeclarationStatement) -> List[str]:
    """Extracts attributes from a TypeDeclarationStatement.

    Args:
        declaration: The TypeDeclarationStatement to process

    Returns:
        List[str]: List of attributes (public, private, etc.)
    """
    return declaration.attrspec if declaration.attrspec else []

