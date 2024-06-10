import os
from fparser.api import parse as fortran_parser
from fparser.one.block_statements import Module, Comment, Function, Class, Real, Integer
from jinja2 import Environment, FileSystemLoader
import html
import re


def find_f90_files(directory):
    f90_files = []
    for root, _, files in os.walk(directory):
        for file in files:
            if file.endswith(".f90"):
                relative_path = os.path.relpath(os.path.join(root, file), directory)
                f90_files.append(relative_path)
    return f90_files


def process_comment(comment):
    processed_comment = re.sub(r"{(.*?)}", r"<code>\1</code>", comment)
    return html.escape(processed_comment.lstrip("! "))

def find_modules(f90_files):
    modules = []
    for f90_file in f90_files:
        module_data = {}
        comment_stack = []
        tree = fortran_parser(f90_file, ignore_comments=False)
        for child in tree.content:
            if isinstance(child, Comment):
                comment_stack.append(child)
            elif isinstance(child, Module):
                module_data["module_name"] = child.name
                module_data["constants"] = {}
                module_data["functions"] = {}
                module_data["subroutines"] = {}
                module_data["file_name"] = f90_file

                # collect module comments
                if (
                    comment_stack
                    and comment_stack[0].content.startswith("!*")
                    and comment_stack[-1].content.endswith("*!")
                ):
                    module_data["module_description"] = ""
                    for comment in comment_stack[1:-1]:
                        content = process_comment(comment.content)
                        if content:
                            module_data["module_description"] += f"{content}\n"

                function_comments = []
                for item in child.content:
                    if isinstance(item, Comment):
                        function_comments.append(item)
                    elif isinstance(item, Function):
                        function_name = item.name
                        module_data["functions"][function_name] = {
                            "description": "",
                            "details": {},
                        }

                        # Extract function details
                        attributes = [attr.strip().lower() for attr in item.prefix.split() if attr.strip()]
                        module_data['functions'][function_name]['details'] = {
                            'attributes': attributes,
                            'arguments': {}
                        }

                        inputs, outputs, result = extract_arg_info(item)

                        if function_comments:
                            # Process function comments
                            comment_arg_info, comment_return_info, func_description = process_function_comments(function_comments)

                            # Compare and update input argument info
                            for arg_name, arg_data in comment_arg_info.items():
                                if arg_name in inputs:
                                    if arg_data['type'] != inputs[arg_name]['type']:
                                        print(f"Warning: Mismatched type for input argument {arg_name} in function {function_name}")
                                    inputs[arg_name].update(arg_data)
                                elif arg_name in outputs:
                                    if arg_data['type'] != outputs[arg_name]['type']:
                                        print(f"Warning: Mismatched type for output argument {arg_name} in function {function_name}")
                                    outputs[arg_name].update(arg_data)
                                else:
                                    print(f"Warning: Argument {arg_name} in comment not found in function {function_name}")

                            # Update result info
                            if comment_return_info:
                                result_name = list(result.keys())[0] if result else None
                                if result_name and comment_return_info['name'] != result_name:
                                    print(f"Warning: Mismatched result name in comment for function {function_name}")
                                if result_name and comment_return_info['type'] != result[result_name]['type']:
                                    print(f"Warning: Mismatched result type in comment for function {function_name}")
                                result.update(comment_return_info)

                            # Update function description
                            module_data["functions"][function_name]["description"] = func_description

                        # Update function details with input, output, and result info
                        module_data['functions'][function_name]['details']['inputs'] = inputs
                        module_data['functions'][function_name]['details']['outputs'] = outputs
                        module_data['functions'][function_name]['details']['return'] = result
                        function_comments = []  # Reset for next function
                    else:
                        function_comments = []  # Reset if next item is not a function

                modules.append(module_data)
                print(module_data)
                print()
                module_data = {}  # Reset module_data for the next module
                comment_stack = []  # Reset comment_stack for the next module
    return modules

env = Environment(loader=FileSystemLoader("templates"))

def create_modules_directory():
    modules_dir = os.path.join("docs", "modules")
    os.makedirs(modules_dir, exist_ok=True)

    for file in os.listdir(modules_dir):
        file_path = os.path.join(modules_dir, file)
        if os.path.isfile(file_path):
            os.remove(file_path)


def generate_home_html(modules):
    template = env.get_template("module_template.html")

    module_names = list(map(lambda module: module["module_name"], modules))
    output = template.render(
        module_names=module_names,
        module_data=[],
        content_data="Welcome to your modules!",
    )

    with open(os.path.join("docs", "modules", "module_index.html"), "w") as file:
        file.write(output)


def generate_module_html(modules):
    template = env.get_template("module_template.html")

    module_names = list(map(lambda m: m["module_name"], modules))
    for module in modules:
        output = template.render(
            module_names=module_names, module_data=module, content_data=""
        )
        with open(
            os.path.join("docs", "modules", f'{module["module_name"]}.html'), "w"
        ) as file:
            file.write(output)


current_directory = os.getcwd()
fortran_files = find_f90_files(current_directory)
modules = find_modules(fortran_files)

create_modules_directory()
generate_home_html(modules)
generate_module_html(modules)
