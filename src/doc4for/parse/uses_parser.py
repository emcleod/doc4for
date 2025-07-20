import logging
from typing import List, Dict
from fparser.two.Fortran2003 import (
    Comment,
    Use_Stmt,
    Name,
    Only_List,  # type: ignore[attr-defined]
    Rename,
    Import_Stmt,
    Import_Name_List  # type: ignore[attr-defined]
)
from fparser.two.utils import walk
from doc4for.models.common import Uses, Import, ImportType
from doc4for.utils.comment_utils import get_formatted_description

logger: logging.Logger = logging.getLogger(__name__)

def parse_uses(use_stmt: Use_Stmt, comment_stack: List[Comment]) -> Dict[str, Uses]:
    uses = {}
    module_name = walk(use_stmt, Name)[0].string
    only = walk(use_stmt, Only_List)
    
    selections = []
    renames = []
    
    if only:
        rename_nodes = walk(only, Rename)
        if rename_nodes:
            for rename in rename_nodes:                        
                local_name = rename.children[1].string     
                original_name = rename.children[2].string  
                renames.append({
                    "local": local_name,
                    "original": original_name
                })
        else:
            selections = [selection.string for selection in walk(only, Name)]
    
    uses[module_name] = {
        "module_name": module_name,
        "selections": selections,
        "renames": renames,
        "description": get_formatted_description(comment_stack)
    }
    comment_stack.clear()
    return uses

def merge_use_statements(existing_use: Uses, new_use: Uses) -> Uses:
    """Merge two use statements for the same module, handling Fortran precedence rules."""
    # If existing already imports everything, keep importing everything
    if not existing_use["selections"] and not existing_use["renames"]:
        return existing_use
    
    # If new imports everything, switch to importing everything
    if not new_use["selections"] and not new_use["renames"]:
        return new_use
    
    # Both have selections - merge them
    combined_selections = existing_use["selections"] + new_use["selections"]
    unique_selections = list(dict.fromkeys(combined_selections))
    
    # Merge renames (assuming no conflicts for now)
    combined_renames = existing_use["renames"] + new_use["renames"]
    
    return {
        "module_name": existing_use["module_name"], 
        "selections": unique_selections,
        "renames": combined_renames,
        "description": new_use.get("description") or existing_use.get("description")
    }

def parse_uses_list(use_stmts: List[Use_Stmt], comment_stack: List[Comment] = []) -> Dict[str, Uses]:
    uses = {}
    
    for use_stmt in use_stmts:
        single_use = parse_uses(use_stmt, comment_stack or [])
        
        for module_name, use_data in single_use.items():
            if module_name in uses:
                uses[module_name] = merge_use_statements(uses[module_name], use_data)
            else:
                uses[module_name] = use_data
    
    return uses

def parse_imports_list(import_stmts: List[Import_Stmt]) -> List[Import]:
    imports = []
    
    for import_stmt in import_stmts:
        single_import = parse_import(import_stmt)
        imports.append(single_import)
    
    return imports

def parse_import(import_stmt: Import_Stmt) -> Import:
    import_name_list = walk(import_stmt, Import_Name_List)
    if not import_name_list:
        return {
            "import_type": ImportType.IMPLICIT,
            "entities": [],
        }
    imports = [name.string for name in walk(import_name_list, Name)]
    if len(imports) == 1 and imports[0] == "all":
        return {
            "import_type": ImportType.ALL,
            "entities": [],
        }
    return {
        "import_type": ImportType.EXPLICIT,
        "entities": imports,
    }

