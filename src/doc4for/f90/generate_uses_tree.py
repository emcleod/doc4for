from typing import Dict, List
from doc4for.models.module_models import ModuleDescription

ALL = 'All'

def generate_imports_tree(module_descriptions: List[ModuleDescription]) -> Dict[str, Dict[str, List[str]]]:
    # Initialize the uses tree
    uses_tree: Dict[str, Dict[str, List[str]]] = {}
    
    # First pass: ensure all module names are in the tree
    for module in module_descriptions:
        if module['module_name'] not in uses_tree:
            uses_tree[module['module_name']] = {}
    
    # Second pass: populate the tree with usage relationships
    for module in module_descriptions:
        current_module = module['module_name']
        
        # Process each 'use' statement in the module
        for used_module_name, use_details in module['uses'].items():
            # Ensure the used module exists in the tree
            if used_module_name not in uses_tree:
                uses_tree[used_module_name] = {}
            
            # If there are specific selections (only clause)
            if use_details['selections']:
                for selection in use_details['selections']:
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
    if 'interfaces' in used_module:
        for interface in used_module['interfaces']:
            # Check if the interface has a functions attribute
            if hasattr(interface, 'functions') or isinstance(interface, dict) and 'functions' in interface:
                functions = interface.get('functions', []) if isinstance(interface, dict) else interface.functions
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
    if item_name.lower() in [p.lower() for p in used_module['parameters']]:
        # Find the original case
        for param in used_module['parameters']:
            if param.lower() == item_name.lower():
                return f"{used_module_name}.html#parameter-{param}"
    
    if item_name.lower() in [v.lower() for v in used_module['variables']]:
        for var in used_module['variables']:
            if var.lower() == item_name.lower():
                return f"{used_module_name}.html#variable-{var}"
    
    if item_name.lower() in [f.lower() for f in used_module['functions']]:
        for func in used_module['functions']:
            if func.lower() == item_name.lower():
                return f"{used_module_name}.html#function-{func}"
    
    if item_name.lower() in [s.lower() for s in used_module['subroutines']]:
        for sub in used_module['subroutines']:
            if sub.lower() == item_name.lower():
                return f"{used_module_name}.html#subroutine-{sub}"
    
    if item_name.lower() in [t.lower() for t in used_module['types']]:
        for typ in used_module['types']:
            if typ.lower() == item_name.lower():
                return f"{used_module_name}.html#type-{typ}"
    
    # Check for interface functions
    if _find_interface_function(used_module, item_name):
        return f"{used_module_name}.html#function-{item_name}"
    
    # If we can't determine the type, just link to the module
    return f"{used_module_name}.html"

def transform_uses_to_html_references(module_descriptions: List[ModuleDescription]) -> None:
    """
    Transform module 'use' statements into HTML references for documentation.
    
    This function processes each module's 'use' statements and converts them into
    HTML links that can be used in the generated documentation. It handles both
    full module imports and specific item selections (with the 'only' clause),
    including renamed items.
    
    Args:
        module_descriptions: A list of module descriptions to process
    """
    # Create a mapping of module names to their descriptions for quick lookup
    module_map = {module['module_name']: module for module in module_descriptions}
    
    for module in module_descriptions:
        # Process each 'use' statement in the module
        for used_module_name, use_details in module['uses'].items():
            # If the used module doesn't exist in our module descriptions, can't link (could be external)
            if used_module_name not in module_map:
                module["uses"][used_module_name]["module_name"] = ""
                continue
                
            used_module = module_map[used_module_name]
            
            # If there are specific selections (only clause)
            if use_details['selections']:
                transformed_selections = []
                for selection in use_details['selections']:
                    # Handle renamed items (e.g., print_greeting => print_hello)
                    original_selection = selection
                    if isinstance(selection, dict):
                        # Get the original name from the dict
                        for _, original_name in selection.items():
                            selection = original_name
                            break
                    
                    # Create the appropriate link based on the item type
                    link = _get_item_type_link(selection, used_module, used_module_name)
                    transformed_selections.append(link)
                
                module["uses"][used_module_name]["selections"] = transformed_selections
            
            # Always set the module_name link for the full module
            module["uses"][used_module_name]["module_name"] = f"{used_module_name}.html"
