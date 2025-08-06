import logging
from typing import List, Tuple
from fparser.two.Fortran2003 import (
    Comment,
    Main_Program,
    Program_Stmt,
    Name,
    Use_Stmt
)
from fparser.two.utils import walk
from doc4for.models.module_models import ProgramDescription
from doc4for.parse.uses_parser import parse_uses_list
from doc4for.utils.comment_utils import format_comments

logger: logging.Logger = logging.getLogger(__name__)


def parse_program(
    program: Main_Program, comment_stack: List[Comment], file_name: str
) -> Tuple[str, ProgramDescription]:
    program_stmt = walk(program, Program_Stmt)
    name = walk(program_stmt, Name)[0].string
    uses = parse_uses_list(walk(program, Use_Stmt), [])
    external_procedures = {} #TODO
    program_details: ProgramDescription = {
        "program_name": name,
        "file_name": file_name,
        "program_description": format_comments(comment_stack),
        "uses": uses,
        "external_procedures": external_procedures
    }
    return name, program_details

