import logging
from typing import List, Tuple
from fparser.two.Fortran2003 import (
    Function_Stmt,
    Function_Subprogram,
    Name,
    Comment,
    Prefix_Spec,
    Suffix,
    Name,
    Intrinsic_Type_Spec,
    Type_Declaration_Stmt,
    Language_Binding_Spec,
    Procedure_Declaration_Stmt
)
from fparser.two.utils import walk
from doc4for.models.procedure_models import FunctionDescription
from doc4for.parse.procedure_parser import parse_procedure, update_arguments_with_comment_data
from doc4for.parse.common_parser import _extract_binding_type

logger: logging.Logger = logging.getLogger(__name__)

def parse_function(function: Function_Subprogram, comment_stack: List[Comment]) -> Tuple[str, FunctionDescription]:
    type_decls = walk(function, Type_Declaration_Stmt)
    procedure_decls = walk(function, Procedure_Declaration_Stmt)
    common = parse_procedure(function, Function_Stmt, type_decls, procedure_decls, comment_stack)
    if common is None:
        return None
    
    # Handle return type and return variable
    return_type, return_variable = None, None
    if common["prefixes"]:
        for node in common["prefixes"][0].children:
            if not isinstance(node, Prefix_Spec):
                if return_type is not None:
                    logger.error(f"Found more than one return type for {common['procedure_name']}")
                    continue
                return_type = node.string.upper() if isinstance(node, Intrinsic_Type_Spec) else node.string
    
    # Handle suffixes
    suffixes = walk(common["procedure_declaration"], Suffix)
    binding_type = None
    if suffixes:
        binding_type = _extract_binding_type(walk(suffixes, Language_Binding_Spec))
        return_variable = walk(suffixes, Name)[0].string
    else:
        return_variable = common["procedure_name"]
    
    # Handle return argument - it's been found in the declarations or is the function name
    return_argument = None
    if return_variable in common["all_parsed_arguments"]:
        return_argument = common["all_parsed_arguments"][return_variable]
    
    if not return_argument:
        return_argument = {
            "description": "",
            "dimension": None,
            "enum_type": None,
            "interface_name": None,
            "type": return_type
        }
    
    function_description = {
        "attributes": common["attributes"],
        "description": "",
        "arguments": common["arguments"],
        "in": common["intent_in"],
        "out": common["intent_out"],
        "argument_interfaces": common["argument_interfaces"],
        "binding_type": binding_type,
        "return": return_argument
    }
    
    update_arguments_with_comment_data(comment_stack, function_description)
    return common["procedure_name"], function_description
