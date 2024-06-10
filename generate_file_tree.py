import os
import shutil
from jinja2 import Environment, FileSystemLoader

def find_f90_files(directory):
    f90_files = []
    for root, _, files in os.walk(directory):
        for file in files:
            # Get the relative path from the current directory to the file
            if file.endswith(".f90"):
                relative_path = os.path.relpath(os.path.join(root, file), directory)
                f90_files.append(relative_path)
    return f90_files

def build_directory_tree(files):
    # Create a dictionary to store the directory structure
    directory_tree = {}

    for file in files:
        # Extract the directory path and file name
        directory, filename = os.path.split(file)

        # Build the directory tree
        current_level = directory_tree
        for part in directory.split(os.path.sep):
            if part not in current_level:
                current_level[part] = {}
            current_level = current_level[part]
        current_level[filename] = file

    return directory_tree

def create_docs_directory():
    docs_directory = 'docs'
    if not os.path.exists(docs_directory):
        os.makedirs(docs_directory)
    else:
        # Clear the docs directory (except for the static directory)
        for item in os.listdir(docs_directory):
            item_path = os.path.join(docs_directory, item)
            if item == 'static':
                continue
            elif os.path.isfile(item_path):
                os.remove(item_path)
            elif os.path.isdir(item_path):
                shutil.rmtree(item_path)

env = Environment(loader=FileSystemLoader('templates'))

def generate_index_html(directory_tree):
    # Load the template
    template = env.get_template('file_template.html')
    
    # Render the template with the data
    output = template.render(sidebar_data = directory_tree, 
                             content_data = 'Welcome to your code!',
                             path = '', 
                             current_path = '', 
                             is_index = True,
                             separator = os.sep)

    # Save the rendered output to a file
    with open(os.path.join('docs', 'index.html'), 'w') as file:
        file.write(output)

def generate_html_files(directory_tree):
    template = env.get_template('file_template.html')

    def generate_html_recursively(tree, current_path):
        for key, value in tree.items():
            if isinstance(value, str):
                file_path = value
                file_name = os.path.basename(file_path)
                target_directory = os.path.join('docs', os.path.dirname(file_path))
                os.makedirs(target_directory, exist_ok=True)
                
                # Include 'docs' in the current_path
                current_path_with_docs = os.path.join('docs', current_path)  
                # Read the file
                with open(file_path, 'r') as file:
                    code = file.read()

                # Render the template
                output = template.render(sidebar_data = directory_tree, 
                                         content_data = '',
                                         code = code,
                                         path = current_path, 
                                         file = file_name, 
                                         current_path = current_path_with_docs, 
                                         is_index = False,
                                         separator = os.sep)
                
                with open(os.path.join(target_directory, file_name[:-4] + '.html'), 'w') as file:
                    file.write(output)
            else:
                generate_html_recursively(value, os.path.join(current_path, key))

    generate_html_recursively(directory_tree, '')

# Find .f90 files starting from the current directory
current_directory = os.getcwd()
fortran_files = find_f90_files(current_directory)

# Build the directory tree
directory_tree = build_directory_tree(fortran_files)

# Manage the docs directory
create_docs_directory()

# Generate the HTML content
generate_index_html(directory_tree)

generate_html_files(directory_tree)