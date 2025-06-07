from typing import List
from fparser.two.Fortran2003 import (
    Comment, 
    Module
)
from fparser.one.block_statements import (
    Program,
    Use
)
from doc4for.models.module_models import ModuleDescription, ProgramDescription
from doc4for.utils.comment_utils import is_doc4for_comment, format_comments

# TODO move these to separate parsers like type_parser
# TODO should use the handlers instead of testing for cases

def initialise_module_description(
    module: Module, comment_stack: List[Comment], file_name: str
) -> ModuleDescription:
    module_data: ModuleDescription = {
        "module_name": module.content[0].items[1].string,
        "parameters": {},
        "variables": {},
        "functions": {},
        "subroutines": {},
        "enums": {},
        "interfaces": [],
        "types": {},
        "uses": {},
        "common_blocks": {},
        "file_name": file_name,
        "module_description": "",
    }
    if is_doc4for_comment(comment_stack):
        module_data["module_description"] = format_comments(comment_stack)
    return module_data

# TODO public declaration should be handled here?


# TODO: this is to link any function calls in dimensions to their definition -
# will need to have a function list for this to work
# def process_dimensions_for_documentation(dims: List[ArrayBound],
#                                         function_registry: Dict[str, 'FunctionInfo']):
#     function_references = []
#     for dim in dims:
#         for bound in (dim['lower'], dim['upper'], dim['stride']):
#             if bound and bound.expr_type == ExpressionType.FUNCTION_CALL:
#                 if bound.function_name in function_registry:
#                     function_references.append({
#                         'name': bound.function_name,
#                         'info': function_registry[bound.function_name]
#                     })
#     return function_references

# {% for dim in dimensions %}
#   {% if dim.upper.expr_type == 'function_call' %}
#     <a href="#function-{{ dim.upper.function_name }}">{{ dim.upper.value }}</a>
#   {% else %}
#     {{ dim.upper.value }}
#   {% endif %}
# {% endfor %}
