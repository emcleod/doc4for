import logging
from typing import List, Dict
from fparser.two.Fortran2003 import (
    Comment,
    Use_Stmt,
    Name,
    Only_List,  # type: ignore[attr-defined]
    Rename,
    Import_Stmt,
    Import_Name_List,  # type: ignore[attr-defined]
    Module_Nature,
    Generic_Spec
)
from fparser.two.utils import walk
from doc4for.models.common import Uses, Import, ImportType, UseType
from doc4for.utils.comment_utils import get_formatted_description

logger: logging.Logger = logging.getLogger(__name__)

def parse_uses(use_stmt: Use_Stmt, comment_stack: List[Comment]) -> Dict[str, Uses]:
    uses = {}
    module_name = walk(use_stmt, Name)[0].string
    only_lists = walk(use_stmt, Only_List)
    
    selections = []
    renames = []
    
    if only_lists:
        only_list = only_lists[0]  # There should only be one Only_List per Use_Stmt
        # Iterate through the children of Only_List
        for item in only_list.children:
            if isinstance(item, Rename):
                local_name = item.children[1].string     
                original_name = item.children[2].string  
                renames.append({
                    "local": local_name,
                    "original": original_name
                })            
            elif isinstance(item, (Name, Generic_Spec)):
                selections.append(item.string)
                
    natures: List[Module_Nature] = walk(use_stmt, Module_Nature) 
    use_type: UseType = UseType.NONE
    if natures:
        match natures[0].string:
            case "INTRINSIC":
                use_type = UseType.INTRINSIC
            case "NON_INTRINSIC":
                use_type = UseType.NON_INTRINSIC

    use: Uses = {
        "module_name": module_name,
        "selections": selections,
        "renames": renames,
        "description": get_formatted_description(comment_stack),
        "use_type": use_type
    }
    uses[module_name] = use
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
    
    use_type: UseType
    if existing_use["use_type"] == new_use["use_type"]:
        use_type = existing_use["use_type"]
    else:
        if existing_use["use_type"] == UseType.NONE and new_use["use_type"] != UseType.NONE:
            use_type = new_use["use_type"]
        elif new_use["use_type"] == UseType.NONE and existing_use["use_type"] != UseType.NONE:
            use_type = existing_use["use_type"]
        else:
            logger.error(f"Have conflicting use types in {existing_use} and {new_use}: using existing")
            use_type = existing_use["use_type"]
    return {
        "module_name": existing_use["module_name"], 
        "selections": unique_selections,
        "renames": combined_renames,
        "description": new_use.get("description") or existing_use.get("description"),
        "use_type": use_type
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
        # Just "import" with no list - imports everything from host
        return {
            "import_type": ImportType.ALL,
            "entities": [],
        }
    
    # "import :: name1, name2, ..." - specific imports
    imports = [name.string for name in walk(import_name_list, Name)]
    return {
        "import_type": ImportType.SPECIFIC,
        "entities": imports,
    }