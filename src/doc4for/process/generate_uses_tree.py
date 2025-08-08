from typing import Dict, List, Iterator, Tuple
from doc4for.models.module_models import ModuleDescription
from doc4for.models.file_models import FileDescription
from doc4for.models.common import Uses

ALL = "All"

def generate_imports_tree(module_descriptions: List[ModuleDescription]) -> Dict[str, Dict[str, List[str]]]:
    # Initialize the uses tree
    uses_tree: Dict[str, Dict[str, List[str]]] = {}
    
    # First pass: ensure all module names are in the tree
    for module in module_descriptions:
        if module["module_name"] not in uses_tree:
            uses_tree[module["module_name"]] = {}
    
    # Second pass: populate the tree with usage relationships
    for module in module_descriptions:
        current_module = module["module_name"]
        
        # Process each "use" statement in the module
        for used_module_name, use_details in module["uses"].items():
            # Ensure the used module exists in the tree
            if used_module_name not in uses_tree:
                uses_tree[used_module_name] = {}
            
            # If there are specific selections (only clause)
            if use_details["selections"]:
                for selection in use_details["selections"]:
                    # Convert to lowercase to match fparser behavior
                    selection_lower = selection.lower()
                    if selection_lower not in uses_tree[used_module_name]:
                        uses_tree[used_module_name][selection_lower] = []
                    if current_module not in uses_tree[used_module_name][selection_lower]:
                        uses_tree[used_module_name][selection_lower].append(current_module)
            else:
                # Full import case (no only clause)
                if ALL not in uses_tree[used_module_name]:
                    uses_tree[used_module_name][ALL] = []
                if current_module not in uses_tree[used_module_name][ALL]:
                    uses_tree[used_module_name][ALL].append(current_module)
    
    return uses_tree

def _find_interface_function(used_module: ModuleDescription, item_name: str) -> bool:
    """
    Check if an item is a function defined in an interface block.
    
    Args:
        used_module: The module description to check
        item_name: The name of the item to find
        
    Returns:
        True if the item is a function in an interface block, False otherwise
    """
    if "interfaces" in used_module:
        for interface in used_module["interfaces"]:
            # Check if the interface has a functions attribute
            if hasattr(interface, "functions") or isinstance(interface, dict) and "functions" in interface:
                functions = interface.get("functions", []) if isinstance(interface, dict) else interface.functions
                # Check if the function name is in the interface
                for func_name in functions:
                    if func_name.lower() == item_name.lower():
                        return True
    return False

def _get_item_type_link(item_name: str, used_module: ModuleDescription, used_module_name: str) -> str:
    """
    Determine the type of a selected item and create an appropriate HTML link.
    
    Args:
        item_name: The name of the selected item
        used_module: The module description containing the item
        used_module_name: The name of the module containing the item
        
    Returns:
        A string containing the HTML link to the item
    """
    # Check for each item type and preserve the original case
    if item_name.lower() in [p.lower() for p in used_module["parameters"]]:
        # Find the original case
        for param in used_module["parameters"]:
            if param.lower() == item_name.lower():
                return f"{used_module_name}.html#parameter-{param}"
    
    if item_name.lower() in [v.lower() for v in used_module["variables"]]:
        for var in used_module["variables"]:
            if var.lower() == item_name.lower():
                return f"{used_module_name}.html#variable-{var}"
    
    if item_name.lower() in [f.lower() for f in used_module["functions"]]:
        for func in used_module["functions"]:
            if func.lower() == item_name.lower():
                return f"{used_module_name}.html#function-{func}"
    
    if item_name.lower() in [s.lower() for s in used_module["subroutines"]]:
        for sub in used_module["subroutines"]:
            if sub.lower() == item_name.lower():
                return f"{used_module_name}.html#subroutine-{sub}"
    
    if item_name.lower() in [t.lower() for t in used_module["types"]]:
        for typ in used_module["types"]:
            if typ.lower() == item_name.lower():
                return f"{used_module_name}.html#type-{typ}"
    
    # Check for interface functions
    if _find_interface_function(used_module, item_name):
        return f"{used_module_name}.html#function-{item_name}"
    
    # If we can"t determine the type, just link to the module
    return f"{used_module_name}.html"


# def transform_uses_to_html_references(file_description: FileDescription) -> None:
#     module_descriptions: List[ModuleDescription] = list(file_description["modules"].values())
#     # Create a mapping of module names to their descriptions for quick lookup
#     module_map = {module["module_name"]: module for module in module_descriptions}
    
#     for module in module_descriptions:
#         # Process each "use" statement in the module
#         for used_module_name, use_details in module["uses"].items():
#             # If the used module doesn"t exist in our module descriptions, can"t link (could be external)
#             if used_module_name not in module_map:
#                 module["uses"][used_module_name]["module_name"] = ""
#                 continue
                
#             used_module = module_map[used_module_name]
            
#             # If there are specific selections (only clause)
#             if use_details["selections"]:
#                 transformed_selections = []
#                 for selection in use_details["selections"]:
#                     # Handle renamed items (e.g., print_greeting => print_hello)
#                     if isinstance(selection, dict):
#                         # Get the original name from the dict
#                         for _, original_name in selection.items():
#                             selection = original_name
#                             break
                    
#                     # Create the appropriate link based on the item type
#                     link = _get_item_type_link(selection, used_module, used_module_name)
#                     transformed_selections.append(link)
                
#                 module["uses"][used_module_name]["selections"] = transformed_selections
            
#             # Always set the module_name link for the full module
#             module["uses"][used_module_name]["module_name"] = f"{used_module_name}.html"


def gather_all_uses(file_description: FileDescription) -> Iterator[Tuple[str, Uses]]:
    """
    Walk through the file description and yield all uses statements with their context.
    
    Yields tuples of (context_path, uses_dict) where context_path helps identify
    where the uses statement came from (e.g., "module:geometry", "function:circle_area")
    """    
    # Module uses
    for module_name, module_desc in file_description.get("modules", {}).items():
        for used_module, uses in module_desc.get("uses", {}).items():
            yield (f"module:{module_name}", uses)
        
        # Function uses within modules
        for func_name, func_desc in module_desc.get("functions", {}).items():
            for used_module, uses in func_desc.get("uses", {}).items():
                yield (f"module:{module_name}/function:{func_name}", uses)
        
        # Subroutine uses within modules
        for sub_name, sub_desc in module_desc.get("subroutines", {}).items():
            for used_module, uses in sub_desc.get("uses", {}).items():
                yield (f"module:{module_name}/subroutine:{sub_name}", uses)
    
    # Program uses
    for prog_name, prog_desc in file_description.get("programs", {}).items():
        for used_module, uses in prog_desc.get("uses", {}).items():
            yield (f"program:{prog_name}", uses)
    
    # Block data uses
    for block_name, block_desc in file_description.get("block_data", {}).items():
        for used_module, uses in block_desc.get("uses", {}).items():
            yield (f"block_data:{block_name}", uses)
    
    # Top-level function uses (outside modules)
    for func_name, func_desc in file_description.get("functions", {}).items():
        for used_module, uses in func_desc.get("uses", {}).items():
            yield (f"function:{func_name}", uses)
    
    # Top-level subroutine uses (outside modules)
    for sub_name, sub_desc in file_description.get("subroutines", {}).items():
        for used_module, uses in sub_desc.get("uses", {}).items():
            yield (f"subroutine:{sub_name}", uses)


def transform_single_use(uses: Uses, used_module_name: str, module_map: Dict[str, ModuleDescription]) -> None:
    """
    Transform a single uses statement in place.
    
    Args:
        uses: The Uses dict to transform
        used_module_name: The name of the module being used
        module_map: Mapping of module names to their descriptions
    """
    # If the used module doesn't exist in our module descriptions, can't link (could be external)
    if used_module_name not in module_map:
        uses["module_name"] = ""
        return
    
    used_module = module_map[used_module_name]
    
    # If there are specific selections (only clause)
    if uses["selections"]:
        transformed_selections = []
        for selection in uses["selections"]:
            # Create the appropriate link based on the item type
            link = _get_item_type_link(selection, used_module, used_module_name)
            transformed_selections.append(link)
        
        uses["selections"] = transformed_selections
    
    # Always set the module_name link for the full module
    uses["module_name"] = f"{used_module_name}.html"


def transform_uses_to_html_references(file_descriptions: List[FileDescription]) -> None:
    """
    Transform all uses statements in the file descriptions to HTML references.
    """
    # Build a global module map from all files
    module_map = {}
    for file_desc in file_descriptions:
        for module_name, module_desc in file_desc.get("modules", {}).items():
            module_map[module_name] = module_desc
    
    # Transform all uses statements in all files
    for file_desc in file_descriptions:
        # Transform file-level uses
        for used_module_name, uses in file_desc.get("uses", {}).items():
            transform_single_use(uses, used_module_name, module_map)
        
        # Transform module uses
        for module_desc in file_desc.get("modules", {}).values():
            for used_module_name, uses in module_desc.get("uses", {}).items():
                transform_single_use(uses, used_module_name, module_map)
            
            # Transform function uses within modules
            for func_desc in module_desc.get("functions", {}).values():
                for used_module_name, uses in func_desc.get("uses", {}).items():
                    transform_single_use(uses, used_module_name, module_map)
            
            # Transform subroutine uses within modules
            for sub_desc in module_desc.get("subroutines", {}).values():
                for used_module_name, uses in sub_desc.get("uses", {}).items():
                    transform_single_use(uses, used_module_name, module_map)
        
        # Transform program uses
        for prog_desc in file_desc.get("programs", {}).values():
            for used_module_name, uses in prog_desc.get("uses", {}).items():
                transform_single_use(uses, used_module_name, module_map)
        
        # Transform block data uses
        for block_desc in file_desc.get("block_data", {}).values():
            for used_module_name, uses in block_desc.get("uses", {}).items():
                transform_single_use(uses, used_module_name, module_map)
        
        # Transform top-level function uses
        for func_desc in file_desc.get("functions", {}).values():
            for used_module_name, uses in func_desc.get("uses", {}).items():
                transform_single_use(uses, used_module_name, module_map)
        
        # Transform top-level subroutine uses
        for sub_desc in file_desc.get("subroutines", {}).values():
            for used_module_name, uses in sub_desc.get("uses", {}).items():
                transform_single_use(uses, used_module_name, module_map)