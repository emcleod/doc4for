import os
import logging
from typing import Dict, List
from typing import List
from pathlib import Path
from doc4for.data_models import ModuleDescription, FileDescription
from doc4for.f90.generate_file_tree import build_directory_tree, generate_file_pages, extract_file_data, DirectoryTree
from doc4for.f90.generate_module_tree import extract_module_data, generate_module_pages
from doc4for.file_utils import find_files_by_extensions, create_docs_directory
from doc4for.f90.generate_type_tree import generate_inheritance_tree, generate_inheritance_tree_page

logging.basicConfig(
    level = logging.INFO,
    format = '%(asctime)s - %(name)s - %(levelname)s - %(message)s',
    datefmt = '%Y-%m-%d %H:%M:%S'
)
logger = logging.getLogger(__name__)

if (create_docs_directory()):
    current_directory: str = os.getcwd()
    logger.info("Finding fortran files")
    fortran_files: List[Path] = find_files_by_extensions(current_directory)
    logger.info("Building inheritance tree")
    inheritance_tree = generate_inheritance_tree(fortran_files)
    logger.info("Building pages")
    directories_and_files: DirectoryTree = build_directory_tree(fortran_files)
    file_data: List[FileDescription] = extract_file_data(fortran_files)
    file_dict: Dict[str, FileDescription] = {}
    for data in file_data:
        file_dict[data['file_name']] = data
    generate_file_pages(directories_and_files, file_dict)
    modules: List[ModuleDescription] = extract_module_data(fortran_files)
    generate_module_pages(modules)
    generate_inheritance_tree_page(inheritance_tree)
    
else:
    logger.info("Nothing written")