import logging
from typing import List, Tuple
from fparser.two.Fortran2003 import (
    Subroutine_Subprogram,
    Subroutine_Stmt,
    Comment,
    Type_Declaration_Stmt,
    Language_Binding_Spec,
    Procedure_Declaration_Stmt,
    External_Stmt
)
from fparser.two.utils import walk
from doc4for.models.procedure_models import SubroutineDescription
from doc4for.parse.common_parser import _extract_binding_type
from doc4for.parse.procedure_parser import parse_procedure, update_arguments_with_comment_data

logger: logging.Logger = logging.getLogger(__name__)


def parse_subroutine(subroutine: Subroutine_Subprogram, comment_stack: List[Comment]) -> Tuple[str, SubroutineDescription]:
    type_decls = walk(subroutine, Type_Declaration_Stmt)
    procedure_decls = walk(subroutine, Procedure_Declaration_Stmt)
    external_decls = walk(subroutine, External_Stmt)
    common = parse_procedure(subroutine, Subroutine_Stmt, type_decls, procedure_decls, external_decls, comment_stack)
    if common is None:
        return None
    
    # Subroutine-specific: only need binding type, no return handling
    binding_type = _extract_binding_type(walk(common["procedure_declaration"], Language_Binding_Spec))
    
    subroutine_description = {
        "attributes": common["attributes"],
        "description": "",
        "arguments": common["arguments"],
        "in": common["intent_in"],
        "out": common["intent_out"],
        "argument_interfaces": {},
        "binding_type": binding_type,
        "uses": common["uses"],
        "imports": common["imports"],
        "external_procedures": common["external_procedures"]
    }
    
    update_arguments_with_comment_data(comment_stack, subroutine_description)
    return common["procedure_name"], subroutine_description
