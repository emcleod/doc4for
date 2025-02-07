from typing import List
from fparser.one.block_statements import (
    Comment,
    Function,
    Subroutine,
)
from doc4for.models.procedure_models import FunctionDescription, SubroutineDescription
from doc4for.parse.procedure_argument_parser import (
    update_arguments_with_comment_data,
    update_arguments_with_parsed_data,
)
from doc4for.utils.comment_utils import is_doc4for_comment

def parse_function(
    function: Function, comment_stack: List[Comment]
) -> FunctionDescription:
    attributes: List[str] = [
        attr.strip().lower() for attr in function.prefix.split() if attr.strip()
    ]
    function_description: FunctionDescription = {
        "attributes": attributes,
        "description": "",
        "arguments": function.args,
        "in": {},
        "out": {},
        "return": {},
#        "binding_type": "",
#        "interface": "",
    }
    update_arguments_with_parsed_data(function, function_description)
    if is_doc4for_comment(comment_stack):
        update_arguments_with_comment_data(comment_stack, function_description)
    return function_description

def parse_subroutine(
    subroutine: Subroutine, comment_stack: List[Comment]
) -> SubroutineDescription:
    attributes: List[str] = [
        attr.strip().lower() for attr in subroutine.prefix.split() if attr.strip()
    ]
    subroutine_description: SubroutineDescription = {
        "attributes": attributes,
        "description": "",
        "arguments": subroutine.args,
        "in": {},
        "out": {},
 #       "binding_type": "",
 #       "interface": "",
    }
    update_arguments_with_parsed_data(subroutine, subroutine_description)
    if is_doc4for_comment(comment_stack):
        update_arguments_with_comment_data(
            comment_stack, subroutine_description)
    return subroutine_description
