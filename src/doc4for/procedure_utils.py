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
    pattern = r'\b(\w+)\s*\('
    matches = re.finditer(pattern, expr)
    for match in matches:
        function_calls.append(match.group(1))    
    return function_calls

