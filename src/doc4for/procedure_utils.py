import re
from fparser.one.statements import Call, Assignment

def extract_procedure_calls(program_node):
    procedure_calls = []
    for content in program_node.content:
        if isinstance(content, Call):
            procedure_calls.append(content.designator)
        elif isinstance(content, Assignment):
            procedure_calls.extend(extract_function_calls_from_expr(content.expr))
    return procedure_calls

def extract_function_calls_from_expr(expr):
    function_calls = []
    # This regex matches function calls, including nested ones
    pattern = r'\b(\w+)\s*\('
    
    # Find all matches
    matches = re.finditer(pattern, expr)
    
    # Extract function names from matches
    for match in matches:
        function_calls.append(match.group(1))
    
    return function_calls

# def extract_function_calls_from_expr(expr):
#     function_calls = []
#     if isinstance(expr, CallBase):
#         function_calls.append(expr.name)
#     elif hasattr(expr, 'items'):
#         for item in expr.items:
#             function_calls.extend(extract_function_calls_from_expr(item))
#     return function_calls