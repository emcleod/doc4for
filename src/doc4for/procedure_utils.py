from fparser.one.statements import Call

def extract_procedure_calls(program_node):
    procedure_calls = []
    for content in program_node.content:
        if isinstance(content, Call):
            procedure_calls.append(content.designator)
    return procedure_calls