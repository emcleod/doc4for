import os
import fparser
from fparser.api import parse as fortran_parser
from fparser.one.block_statements import Module, Comment, Subroutine, Function
from jinja2 import Environment, FileSystemLoader
import html

def find_f90_files(directory):
    f90_files = []
    for root, _, files in os.walk(directory):
        for file in files:
            if file.endswith(".f90"):
                relative_path = os.path.relpath(os.path.join(root, file), directory)
                f90_files.append(relative_path)
    return f90_files

def find_modules(f90_files):
    modules = []
    for f90_file in f90_files:
        module_data = {}
        comment_stack = []
        try:
            tree = fortran_parser(f90_file, ignore_comments=False)
            for child in tree.content:
                if isinstance(child, Comment):
                    comment_stack.append(child)
                if isinstance(child, Module):
                    module_data['module_name'] = child.name
                    if comment_stack and comment_stack[0].content.startswith('!*') and comment_stack[-1].content == '*!':
                        module_data['module_description'] = ''
                        for comment in comment_stack[1:-1]:
                            content = html.escape(comment.content.lstrip('! '))
                            if content:
                                module_data['module_description'] += f'{content}\n'
                    module_data['constants'] = {}  # name: value
                    module_data['subroutines'] = {}  # name description, details
                    module_data['functions'] = {}  # name description details
                    module_data['file_name'] = f90_file
                    modules.append(module_data)
                    module_data = {}  # Reset module_data for the next module
                    comment_stack = []  # Reset comment_stack for the next module
        except Exception as e:
            print(f"Skipping file '{f90_file}' due to parsing error: {str(e)}")
            continue
    return modules

env = Environment(loader=FileSystemLoader('templates'))

def create_modules_directory():
    modules_dir = os.path.join('docs', 'modules')
    os.makedirs(modules_dir, exist_ok=True)

    for file in os.listdir(modules_dir):
        file_path = os.path.join(modules_dir, file)
        if os.path.isfile(file_path):
            os.remove(file_path)

def generate_home_html(modules):
    template = env.get_template('module_template.html')

    module_names = list(map(lambda module : module['module_name'], modules))
    output = template.render(module_names=module_names,
                             module_data=[],
                             content_data='Welcome to your modules!')
    
    with open(os.path.join('docs', 'modules', 'module_index.html'), 'w') as file:
        file.write(output)

def generate_module_html(modules):
    template = env.get_template('module_template.html')

    module_names = list(map(lambda m: m['module_name'], modules))
    for module in modules:
        print(f"Module data for {module['module_name']}:")
        print(module)
        output = template.render(module_names=module_names,
                                 module_data=module,
                                 content_data='')
        with open(os.path.join('docs', 'modules', f'{module["module_name"]}.html'), 'w') as file:
            file.write(output)

current_directory = os.getcwd()
fortran_files = find_f90_files(current_directory)
modules = find_modules(fortran_files)

create_modules_directory()
generate_home_html(modules)
generate_module_html(modules)