from typing import List, Optional, Tuple
from functools import wraps
from fparser.one.typedecl_statements import TypeDeclarationStatement
from doc4for.models.common import Expression, ExpressionType

def handle_dimension_errors(func):
    @wraps(func)
    def wrapper(*args, **kwargs):
        try:
            return func(*args, **kwargs)
        except Exception as e:
            raise
    return wrapper

def parse_expression(expr: str) -> Expression:
    if '(' in expr:
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
        return Expression(expr_type=ExpressionType.LITERAL, value=expr)
    else:
        return Expression(expr_type=ExpressionType.VARIABLE, value=expr)

def split_args(s: str) -> List[str]:
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

def find_matching_parenthesis(s: str, start: int = 0) -> int:
    count = 0
    for i in range(start, len(s)):
        if s[i] == '(':
            count += 1
        elif s[i] == ')':
            count -= 1
            if count == 0:
                return i
    return -1

def get_attributes(declaration: TypeDeclarationStatement) -> List[str]:
    return declaration.attrspec if declaration.attrspec else []


def get_character_length(base_type: str, attributes: List[str], selector: Optional[Tuple] = None,
                         initial_value: Optional[str] = None) -> Optional[str]:
    """
    Determine character length from various possible sources:
    1. Explicit length specification in selector
    2. Length specification in attributes
    3. Initial value string length
    """
    # If it's not a character, return immediately
    if base_type != "character":
        return None
    
    # First check if length is in selector
    if selector and isinstance(selector, tuple):
        len_spec, old_style_len = selector
        if len_spec and 'selected_char_kind' in len_spec:
            pass # this is a kind spec, not a length spec
        elif len_spec == '*':  # assumed-length character: character(len=*)
            return '*'
        elif len_spec:  # new style: character(len=10)
            return len_spec
        if old_style_len:  # old style: character*20
            return old_style_len
                
    # Then check attributes (for character(len=10) form)
    for attr in attributes:
        if attr.startswith("len="):
            return attr[4:]  # remove "len=" prefix
        elif attr.isdigit() or (attr[0] == '-' and attr[1:].isdigit()):
            # This handles the case where length is just a number
            return attr
            
    # Finally, if we have an initial value, calculate length from that
    if initial_value and initial_value.startswith("'") and initial_value.endswith("'"):
        string_content = initial_value[1:-1]
        return str(len(string_content))
    
    return "1"

def extract_kind(declaration: TypeDeclarationStatement) -> Optional[str]:
    """Extract kind specification from a type declaration.

    Args:
        declaration: The type declaration statement

    Returns:
        Optional[str]: The kind specification if present, None otherwise
    """
    if not hasattr(declaration, "selector"):
        return None
        
    # For character type, selector tuple represents (length, kind)
    # For other types, it represents (kind, old_style_kind)
    if isinstance(declaration.selector, tuple):
        if declaration.name.lower() == "character":
            # For character, return the second element (kind value)
            _, kind_value = declaration.selector
            return kind_value or None
        else:
            # For other types, return the first non-empty value
            star_value, kind_value = declaration.selector
            return star_value or kind_value or None
            
    return None