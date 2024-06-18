import os
from f90.generate_file_tree import (find_f90_files,
                                    build_directory_tree,
                                    create_docs_directory,
                                    generate_file_pages)
from f90.generate_module_tree import process_modules, generate_module_pages

create_docs_directory()
current_directory = os.getcwd()

fortran_files = find_f90_files(current_directory)
directories_and_files = build_directory_tree(fortran_files)

generate_file_pages(directories_and_files)
modules = process_modules(fortran_files)
generate_module_pages(modules)
