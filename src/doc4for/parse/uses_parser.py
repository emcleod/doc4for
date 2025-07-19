import logging
from typing import List, Dict
from fparser.two.Fortran2003 import (
    Comment,
    Use_Stmt,
    Name,
    Only_List,  # type: ignore[attr-defined]
    Rename
)
from fparser.two.utils import walk
from doc4for.models.common import Uses
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

# def parse_uses(use_stmt: Use_Stmt, comment_stack: List[Comment]) -> Dict[str, Uses]:
#     uses = {}
#     module_name = walk(use_stmt, Name)[0].string
#     only = walk(use_stmt, Only_List)
    
#     # Initialize empty lists for selections and renames
#     selections = []
#     renames_list = []
    
#     # If this module was already processed, get existing selections and renames
#     if module_name in uses:
#         selections = uses[module_name]["selections"]
#         renames_list = uses[module_name]["renames"]
    
#     # Process the ONLY list if it exists
#     if only:
#         # Find renames in the ONLY list
#         rename_nodes = walk(only, Rename)
#         if rename_nodes:
#             for rename_node in rename_nodes:                        
#                 local_name = rename_node.children[0].string
#                 original_name = rename_node.children[2].string
#                 renames_list.append({
#                     "local": local_name,
#                     "original": original_name
#                 })
        
#         # Find regular selections (not renames)
#         # This needs to be adjusted to only get names that aren't part of renames
#         name_nodes = walk(only, Name)
#         for name_node in name_nodes:
#             # Skip names that are part of renames
#             if not any(name_node in rename.children for rename in rename_nodes):
#                 selections.append(name_node.string)
    
#     if module_name in uses:
#         if not only:
#             # overwrite previous selections with entire module
#             uses[module_name]["selections"] = []
#             uses[module_name]["renames"] = []
#         else:
#             uses[module_name]["selections"] = selections
#             uses[module_name]["renames"] = renames_list
#     else:
#         uses[module_name] = {
#             "module_name": module_name,
#             "selections": selections,
#             "renames": renames_list,
#             "description": get_formatted_description(comment_stack)
#         }
#     comment_stack.clear()
#     return uses

# def merge_use_statements(existing_use: Uses, new_use: Uses) -> Uses:
#     """Merge two use statements for the same module, handling Fortran precedence rules."""
#     # If existing already imports everything, keep importing everything
#     if not existing_use["selections"]:
#         return existing_use  # Keep the unrestricted import
    
#     # If new imports everything, switch to importing everything
#     if not new_use["selections"]:
#         return new_use  # Switch to unrestricted import
    
#     # Both have selections - merge them (cumulative)
#     combined = existing_use["selections"] + new_use["selections"]
#     unique_selections = list(dict.fromkeys(combined))
    
#     # Merge renames as well
#     combined_renames = existing_use["renames"] + new_use["renames"]
#     # Remove duplicates by converting to a dict with local name as key
#     unique_renames = {}
#     for rename in combined_renames:
#         unique_renames[rename["local"]] = rename
    
#     return {
#         "module_name": existing_use["module_name"], 
#         "selections": unique_selections,
#         "renames": list(unique_renames.values()),
#         "description": new_use.get("description") or existing_use.get("description")
#     }

# def parse_uses(use_stmt: Use_Stmt, comment_stack: List[Comment]) -> Dict[str, Uses]:
#     uses = {}
#     module_name = walk(use_stmt, Name)[0].string
#     only = walk(use_stmt, Only_List)
#     renames = walk(only, Rename)
#     selections = uses[module_name]["selections"] if module_name in uses else []
#     if renames:
#         for rename in renames:                        
#             import_selection = rename.children[1].string
#             import_rename = rename.children[2].string
#             selections.append((import_selection, import_rename))
#     else:
#       selections = [selection.string for selection in walk(only, Name)]
#     if module_name in uses:
#         if not only:
#             # overwrite previous selections with entire module
#             uses[module_name]["selections"] = []
#         else:
#             uses[module_name]["selections"].extend(selections)
#     else:
#         uses[module_name] = {
#             "module_name": module_name,
#             "selections": selections,
#             "description": get_formatted_description(comment_stack)
#             }
#     comment_stack.clear()
#     return uses

# def merge_use_statements(existing_use: Uses, new_use: Uses) -> Uses:
#     """Merge two use statements for the same module, handling Fortran precedence rules."""
#     # If existing already imports everything, keep importing everything
#     if not existing_use["selections"]:
#         return existing_use  # Keep the unrestricted import
    
#     # If new imports everything, switch to importing everything
#     if not new_use["selections"]:
#         return new_use  # Switch to unrestricted import
    
#     # Both have selections - merge them (cumulative)
#     combined = existing_use["selections"] + new_use["selections"]
#     unique_selections = list(dict.fromkeys(combined))
#     renames = []
#     return {
#         "module_name": existing_use["module_name"], 
#         "selections": unique_selections,
#         "renames": renames,
#         "description": new_use.get("description") or existing_use.get("description")
#     }

def parse_uses_list(use_stmts: List[Use_Stmt], comment_stack: List[Comment] = []) -> Dict[str, Uses]:
    """Parse a list of use statements and accumulate selections for each module."""
    uses = {}
    
    for use_stmt in use_stmts:
        single_use = parse_uses(use_stmt, comment_stack or [])
        
        for module_name, use_data in single_use.items():
            if module_name in uses:
                uses[module_name] = merge_use_statements(uses[module_name], use_data)
            else:
                uses[module_name] = use_data
    
    return uses

