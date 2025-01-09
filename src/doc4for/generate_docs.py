import os
import logging
import argparse
from typing import Dict, List
from typing import List
from pathlib import Path
from doc4for.data_models import ModuleDescription, FileDescription
from doc4for.f90.generate_file_tree import build_directory_tree, generate_file_pages, extract_file_data, DirectoryTree
from doc4for.f90.generate_module_tree import extract_module_data, generate_module_pages
from doc4for.file_utils import find_files_by_extensions, create_docs_directory
from doc4for.f90.generate_type_tree import generate_inheritance_tree, generate_inheritance_tree_page
from doc4for.config import load_configuration, ConfigKeys
from doc4for import __version__
from doc4for.logging_config import setup_logging

logger = logging.getLogger(__name__)

def main() -> None:
    args = create_cli_options()

    # set up logging
    log_level = set_logging_level(args)
    setup_logging(log_level)
    logger = logging.getLogger(__name__)

    # load configuration file or use defaults
    config = load_configuration(args.config)

    if create_docs_directory(config[ConfigKeys.OUTPUT_DIR.value]):
        current_directory: str = os.getcwd()
        logger.info("Finding fortran files")
        fortran_files: List[Path] = find_files_by_extensions(
            current_directory, 
            extensions=set(config[ConfigKeys.EXTENSIONS.value]),
            exclude_dirs=set(config[ConfigKeys.EXCLUDE_DIRS.value])
        )
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

def create_cli_options(): 
    parser = argparse.ArgumentParser(description="Generate documentation for Fortran code")
    parser.add_argument('-c', '--config', 
                        default='doc4for.json',
                        help='Path to the configuration file (default: doc4for.json in current directory)')
    parser.add_argument('--version',
                        action='version',
                        version=f'{parser.prog} {__version__}',
                        help='The version of doc4for')
    
    # Add logging-related arguments
    log_group = parser.add_argument_group('logging options')
    log_group.add_argument('-v', '--verbose', 
                          action='count',
                          default=0,
                          help='Increase verbosity (can be repeated: -v, -vv, -vv)')
    log_group.add_argument('--log-level',
                          choices=['DEBUG', 'INFO', 'WARNING', 'ERROR', 'CRITICAL'],
                          help='Set explicit log level. This option takes precedence over --verbose')
    
    args = parser.parse_args()
    return args

def set_logging_level(args):
    # If explicit log level is provided, use it
    if args.log_level:
        return getattr(logging, args.log_level)
    
    # Otherwise, use verbosity
    verbosity_to_log_level = {
        0: logging.WARNING,  # default
        1: logging.INFO,     # -v
        2: logging.DEBUG,    # -vv
    }
    verbosity = min(args.verbose, 2)  # cap at 2
    return verbosity_to_log_level[verbosity]

if __name__ == "__main__":
    main()