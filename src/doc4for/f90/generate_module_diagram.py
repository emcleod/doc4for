"""
Module for generating dependency diagrams for Fortran modules using basic HTML.
This is a simplified implementation that doesn't require external packages.
"""
from typing import List, Dict, Optional, Set
import os
from pathlib import Path
from doc4for.models.module_models import ModuleDescription
from doc4for.process.generate_uses_tree import generate_imports_tree

def generate_module_diagram(module_descriptions: List[ModuleDescription], 
                           output_dir: str, 
                           filename: str = "module_dependencies") -> str:
    """
    Generate a simple HTML table showing module dependencies.
    
    Args:
        module_descriptions: List of module descriptions
        output_dir: Directory to save the diagram
        filename: Base filename for the diagram (without extension)
        
    Returns:
        Path to the generated HTML file
    """
    # Create a dictionary of module dependencies
    dependencies = {}
    for module in module_descriptions:
        module_name = module['module_name']
        dependencies[module_name] = []
        
        for used_module_name, use_details in module['uses'].items():
            # Only add modules that are in our documentation
            if any(m['module_name'] == used_module_name for m in module_descriptions):
                # If there are specific selections, add them as details
                if use_details['selections']:
                    # Limit the number of items shown to avoid cluttering
                    selections = use_details['selections']
                    if len(selections) > 3:
                        details = f"{', '.join(str(s) for s in selections[:3])}..."
                    else:
                        details = f"{', '.join(str(s) for s in selections)}"
                    dependencies[module_name].append((used_module_name, details))
                else:
                    dependencies[module_name].append((used_module_name, ""))
    
    # Ensure the output directory exists
    os.makedirs(output_dir, exist_ok=True)
    
    # Create the HTML file
    html_path = os.path.join(output_dir, f"{filename}.html")
    
    # Generate HTML table
    html_content = f"""<!DOCTYPE html>
<html>
<head>
    <title>Module Dependencies</title>
    <style>
        body {{ font-family: Arial, sans-serif; margin: 20px; }}
        h1 {{ color: #333; }}
        table {{ 
            border-collapse: collapse;
            width: 100%;
            margin-top: 20px;
        }}
        th, td {{ 
            border: 1px solid #ddd;
            padding: 8px;
            text-align: left;
        }}
        th {{ 
            background-color: #f2f2f2;
            color: #333;
        }}
        tr:nth-child(even) {{ background-color: #f9f9f9; }}
        tr:hover {{ background-color: #f2f2f2; }}
        .module-name {{ font-weight: bold; }}
        .details {{ color: #666; font-style: italic; font-size: 0.9em; }}
    </style>
</head>
<body>
    <h1>Module Dependencies</h1>
    <p>Click on any module to view its documentation.</p>
    <table>
        <tr>
            <th>Module</th>
            <th>Depends On</th>
        </tr>
"""
    
    # Add rows for each module
    for module_name, used_modules in dependencies.items():
        html_content += f"""
    <tr>
        <td class="module-name"><a href="{module_name}.html">{module_name}</a></td>
        <td>"""
        
        if used_modules:
            for i, (used_module, details) in enumerate(used_modules):
                if i > 0:
                    html_content += "<br>"
                html_content += f'<a href="{used_module}.html">{used_module}</a>'
                if details:
                    html_content += f' <span class="details">({details})</span>'
        else:
            html_content += "None"
            
        html_content += """</td>
    </tr>"""
    
    html_content += """
    </table>
</body>
</html>
"""
    
    with open(html_path, 'w') as f:
        f.write(html_content)
    
    return html_path

def generate_module_usage_diagram(module_descriptions: List[ModuleDescription], 
                                 output_dir: str,
                                 module_name: Optional[str] = None,
                                 filename: str = "module_usage") -> str:
    """
    Generate a simple HTML table showing which modules use a specific module.
    
    Args:
        module_descriptions: List of module descriptions
        output_dir: Directory to save the diagram
        module_name: Name of the module to show usage for (if None, shows all)
        filename: Base filename for the diagram (without extension)
        
    Returns:
        Path to the generated HTML file
    """
    # Generate the imports tree
    imports_tree = generate_imports_tree(module_descriptions)
    
    # Ensure the output directory exists
    os.makedirs(output_dir, exist_ok=True)
    
    # Set the filename
    if module_name:
        filename = f"{filename}_{module_name}"
    
    # Create the HTML file
    html_path = os.path.join(output_dir, f"{filename}.html")
    
    # Title for the page
    title = f"Usage of {module_name}" if module_name else "Module Usage"
    
    # Generate HTML table
    html_content = f"""<!DOCTYPE html>
<html>
<head>
    <title>{title}</title>
    <style>
        body {{ font-family: Arial, sans-serif; margin: 20px; }}
        h1 {{ color: #333; }}
        table {{ 
            border-collapse: collapse;
            width: 100%;
            margin-top: 20px;
        }}
        th, td {{ 
            border: 1px solid #ddd;
            padding: 8px;
            text-align: left;
        }}
        th {{ 
            background-color: #f2f2f2;
            color: #333;
        }}
        tr:nth-child(even) {{ background-color: #f9f9f9; }}
        tr:hover {{ background-color: #f2f2f2; }}
        .module-name {{ font-weight: bold; }}
        .details {{ color: #666; font-style: italic; font-size: 0.9em; }}
    </style>
</head>
<body>
    <h1>{title}</h1>
    <p>Click on any module to view its documentation.</p>
    <table>
        <tr>
            <th>Module</th>
            <th>Used By</th>
            <th>Items Used</th>
        </tr>
"""
    
    # If a specific module is selected, only show its usage
    if module_name:
        html_content += f"""
        <tr>
            <td class="module-name"><a href="{module_name}.html">{module_name}</a></td>
            <td>"""
        
        # Add modules that use this module
        if module_name in imports_tree:
            used_by_modules: Set[str] = set()
            items_used: Dict[str, List[str]] = {}
            
            for item, using_modules in imports_tree[module_name].items():
                for using_module in using_modules:
                    used_by_modules.add(using_module)
                    if using_module not in items_used:
                        items_used[using_module] = []
                    if item != 'All':
                        items_used[using_module].append(item)
            
            # Add the using modules
            for i, using_module in enumerate(sorted(used_by_modules)):
                if i > 0:
                    html_content += "<br>"
                html_content += f'<a href="{using_module}.html">{using_module}</a>'
            
            html_content += """</td>
            <td>"""
            
            # Add the items used
            for i, (using_module, items) in enumerate(sorted(items_used.items())):
                if i > 0:
                    html_content += "<br>"
                if items:
                    html_content += f'{using_module}: {", ".join(items)}'
                else:
                    html_content += f'{using_module}: All'
        else:
            html_content += "None</td><td>None"
            
        html_content += """</td>
        </tr>"""
    else:
        # Show all module relationships
        for module_name in sorted(imports_tree.keys()):
            html_content += f"""
        <tr>
            <td class="module-name"><a href="{module_name}.html">{module_name}</a></td>
            <td>"""
            
            used_by_modules: Set[str] = set()
            items_used: Dict[str, List[str]] = {}
            
            for item, using_modules in imports_tree[module_name].items():
                for using_module in using_modules:
                    used_by_modules.add(using_module)
                    if using_module not in items_used:
                        items_used[using_module] = []
                    if item != 'All':
                        items_used[using_module].append(item)
            
            # Add the using modules
            if used_by_modules:
                for i, using_module in enumerate(sorted(used_by_modules)):
                    if i > 0:
                        html_content += "<br>"
                    html_content += f'<a href="{using_module}.html">{using_module}</a>'
            else:
                html_content += "None"
            
            html_content += """</td>
            <td>"""
            
            # Add the items used
            if items_used:
                for i, (using_module, items) in enumerate(sorted(items_used.items())):
                    if i > 0:
                        html_content += "<br>"
                    if items:
                        html_content += f'{using_module}: {", ".join(items)}'
                    else:
                        html_content += f'{using_module}: All'
            else:
                html_content += "None"
                
            html_content += """</td>
        </tr>"""
    
    html_content += """
    </table>
</body>
</html>
"""
    
    with open(html_path, 'w') as f:
        f.write(html_content)
    
    return html_path 