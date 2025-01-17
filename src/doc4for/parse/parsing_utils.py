from typing import List, Optional, Tuple
from functools import wraps
from fparser.one.typedecl_statements import TypeDeclarationStatement
from doc4for.models.common import Expression, ExpressionType

CHARACTER_KIND_FUNC = 'selected_char_kind'

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

def get_character_length(base_type: str, declaration, attributes: List[str], selector: Optional[Tuple] = None,
                        initial_value: Optional[str] = None) -> Optional[str]:
    if base_type.lower() != "character":
        return None
    
    # First check selector
    if selector and isinstance(selector, tuple):
        len_spec, kind_spec = selector
                
        # Check if there's an explicit length specification
        if hasattr(declaration, 'item') and hasattr(declaration.item, 'line'):
            line = declaration.item.line.lower()
            if "len=" in line:
                # Extract the length value after len=
                return len_spec
            
        # If len_spec is present and doesn't look like a kind specification
        if len_spec and not any(k in len_spec.lower() for k in 
                              ['kind=', 'selected_char_kind', 'ascii_kind']):
            return len_spec
    
    # Check initial value
    if initial_value:
        if initial_value.startswith("'") and initial_value.endswith("'"):
            return str(len(initial_value[1:-1]))
    
    # Default length is 1
    return "1"

def extract_kind(declaration: TypeDeclarationStatement) -> Optional[str]:
    if not hasattr(declaration, "selector"):
        return None

    if isinstance(declaration.selector, tuple):
        len_spec, kind_spec = declaration.selector
        
        
        # Check if there's an explicit kind specification in the declaration
        if hasattr(declaration, 'item') and hasattr(declaration.item, 'line'):
            line = declaration.item.line.lower()
            if "kind=" in line:
                return kind_spec or len_spec
                
        # Handle explicit kind specification in selector
        if kind_spec:
            return kind_spec
            
        # Handle cases where the specification is ambiguous (character(selected_char_kind('ASCII'))) for
        # now but there might be others
        if len_spec and CHARACTER_KIND_FUNC in len_spec.lower():
            return len_spec
            
    return None

