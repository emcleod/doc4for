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