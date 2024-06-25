import os
import logging
from doc4for.f90.generate_file_tree import build_directory_tree, generate_file_pages
from doc4for.f90.generate_module_tree import process_modules, generate_module_pages
from doc4for.file_utils import find_files_by_extensions, create_docs_directory
from doc4for.f90.generate_inheritance_tree import generate_inheritance_tree, generate_inheritance_tree_page

logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
    datefmt='%Y-%m-%d %H:%M:%S'
)
logger = logging.getLogger(__name__)

if (create_docs_directory()):
    current_directory = os.getcwd()
    logger.info("Finding fortran files")
    fortran_files = find_files_by_extensions(current_directory)
    logger.info("Building inheritance tree")
    inheritance_tree = generate_inheritance_tree(fortran_files)
    logger.info("Building pages")
    directories_and_files = build_directory_tree(fortran_files)
    generate_file_pages(directories_and_files)
    modules = process_modules(fortran_files)
    generate_module_pages(modules)
    generate_inheritance_tree_page(inheritance_tree)
else:
    logger.info("Nothing written")