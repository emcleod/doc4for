import logging
from typing import List, Tuple, Dict
from fparser.two.Fortran2003 import (
    Comment,
    Main_Program,
    Use_Stmt,
    Program_Stmt,
    Name,
    Specification_Part,
    Only_List,  # type: ignore[attr-defined]
    Rename
)
from fparser.two.utils import walk
from doc4for.models.module_models import ProgramDescription
from doc4for.models.common import Uses
from doc4for.utils.comment_utils import format_comments, is_doc4for_comment

logger: logging.Logger = logging.getLogger(__name__)


def parse_program(
    program: Main_Program, comment_stack: List[Comment], file_name: str
) -> Tuple[str, ProgramDescription]:
    program_stmt = walk(program, Program_Stmt)
    name = walk(program_stmt, Name)[0].string
    uses = parse_uses(program)
    program_details: ProgramDescription = {
        "program_name": name,
        "file_name": file_name,
        "program_description": "",
        "uses": uses,
    }

    if is_doc4for_comment(comment_stack):
        program_details["program_description"] = format_comments(comment_stack)

    return name, program_details


def parse_uses(program: Main_Program) -> Dict[str, Uses]:
    uses = {}
    uses_comment_stack: List[Comment] = []
    for child in program.children:
        if isinstance(child, Comment):
            uses_comment_stack.append(child)
        elif isinstance(child, Specification_Part):
            use_stmts = walk(child, Use_Stmt)
            for use_stmt in use_stmts:
                module_name = walk(use_stmt, Name)[0].string
                only = walk(use_stmt, Only_List)
                renames = walk(only, Rename)
                selections = uses[module_name]["selections"] if module_name in uses else []
                if renames:
                    for rename in renames:                        
                        import_selection = rename.children[1].string
                        import_rename = rename.children[2].string
                        selections.append((import_selection, import_rename))
                else:
                  selections = [selection.string for selection in walk(only, Name)]
                if module_name in uses:
                    if not only:
                        # overwrite previous selections with entire module
                        uses[module_name]["selections"] = []
                    else:
                        uses[module_name]["selections"].extend(selections)
                else:
                    uses[module_name] = {
                        "module_name": module_name,
                        "selections": selections,
                        "description": (
                            format_comments(uses_comment_stack) if is_doc4for_comment(uses_comment_stack) else ""
                        ),
                    }
            uses_comment_stack.clear()
    return uses

