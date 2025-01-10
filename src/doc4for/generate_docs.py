import os
import logging
import argparse
import shutil
from typing import Dict, List, Optional, Any
from pathlib import Path
from doc4for.models.module_models import ModuleDescription
from doc4for.models.file_models import FileDescription
from doc4for.f90.generate_file_tree import build_directory_tree, generate_file_pages, extract_file_data, DirectoryTree
from doc4for.f90.generate_module_tree import extract_module_data, generate_module_pages
from doc4for.utils.file_utils import find_files_by_extensions, create_docs_directory
from doc4for.f90.generate_type_tree import generate_inheritance_tree, generate_inheritance_tree_page
from doc4for.config import (load_configuration, ConfigKeys, OutputFormatKeys, CommonOutputKeys, TemplateKeys, CoreTemplateKeys, StaticAssetKeys)
from doc4for import __version__
from doc4for.logging_config import setup_logging

logger: logging.Logger = logging.getLogger(__name__)


def main() -> None:
    args: argparse.Namespace = create_cli_options()

    # set up logging
    log_level: int = set_logging_level(args)
    setup_logging(log_level)
    logger: logging.Logger = logging.getLogger(__name__)

    # load configuration file or use defaults
    config: Dict[str, Any] = load_configuration(args.config)

    if create_docs_directory(config[ConfigKeys.OUTPUT_DIR.value]):

        copy_static_assets(config)
    
        # Extract template information from config
        html_config: Dict[str, Any] = config[ConfigKeys.OUTPUT_FORMATS.value][OutputFormatKeys.HTML.value]
        templates_config: Dict[str, Any] = html_config[CommonOutputKeys.TEMPLATES.value]
        template_root: str = templates_config[TemplateKeys.ROOT_DIR.value]
        core_config: Dict[str, Any] = templates_config[TemplateKeys.CORE.value]
        output_dir: str = config[ConfigKeys.OUTPUT_DIR.value]

        # Construct the path in which to look for html template files
        html_template_dir: str = os.path.join(template_root, TemplateKeys.CORE.value, core_config[CoreTemplateKeys.DIR.value])
        
        # Get template file names
        file_template: str = core_config[CoreTemplateKeys.FILE.value]
        module_template: str = core_config[CoreTemplateKeys.MODULE.value]
        inheritance_tree_template: str = core_config[CoreTemplateKeys.INHERITANCE_TREE.value]

        current_directory: str = os.getcwd()

        logger.info("Finding fortran files")
        fortran_files: List[Path] = find_files_by_extensions(
            current_directory,
            extensions=set(config[ConfigKeys.EXTENSIONS.value]),
            exclude_dirs=set(config[ConfigKeys.EXCLUDE_DIRS.value])
        )

        logger.info("Building directory tree")
        directories_and_files: DirectoryTree = build_directory_tree(fortran_files)

        logger.info("Extracting file data")
        file_data: List[FileDescription] = extract_file_data(fortran_files)
        file_dict: Dict[str, FileDescription] = {}
        for data in file_data:
            file_dict[data['file_name']] = data
        
        logger.info("Generating file pages")
        generate_file_pages(
            directories_and_files, 
            file_dict,
            html_template_dir,
            file_template,
            output_dir
        )
        logger.info("Extracting module data")
        modules: List[ModuleDescription] = extract_module_data(fortran_files)

        logger.info("Generating module pages")
        generate_module_pages(modules, html_template_dir, module_template, output_dir)

        logger.info("Building inheritance tree")
        inheritance_tree: Dict[str, Any] = generate_inheritance_tree(fortran_files)

        logger.info("Generating inheritance tree pages")
        generate_inheritance_tree_page(
            inheritance_tree,
            html_template_dir,
            inheritance_tree_template,
            output_dir
        )
    else:
        logger.info("Nothing written")

def create_cli_options() -> argparse.Namespace:
    parser: argparse.ArgumentParser = argparse.ArgumentParser(
        description="Generate documentation for Fortran code")
    parser.add_argument('-c', '--config',
                        default='doc4for.json',
                        help='Path to the configuration file (default: doc4for.json in current directory)')
    parser.add_argument('--version',
                        action='version',
                        version=f'{parser.prog} {__version__}',
                        help='The version of doc4for')

    # Add logging-related arguments
    log_group: argparse._ArgumentGroup = parser.add_argument_group('logging options')
    log_group.add_argument('-v', '--verbose',
                           action='count',
                           default=0,
                           help='Increase verbosity (can be repeated: -v, -vv, -vv)')
    log_group.add_argument('--log-level',
                           choices=['DEBUG', 'INFO', 'WARNING', 'ERROR', 'CRITICAL'],
                           help='Set explicit log level. This option takes precedence over --verbose')

    args: argparse.Namespace = parser.parse_args()
    return args


def set_logging_level(args: argparse.Namespace) -> int:
    # If explicit log level is provided, use it
    if args.log_level:
        return getattr(logging, args.log_level)

    # Otherwise, use verbosity
    verbosity_to_log_level: Dict[int, int] = {
        0: logging.WARNING,  # default
        1: logging.INFO,     # -v
        2: logging.DEBUG,    # -vv
    }
    verbosity: int = min(args.verbose, 2)  # cap at 2
    return verbosity_to_log_level[verbosity]

def copy_static_assets(config: Dict[str, Any]) -> None:
    """
    Copy static assets (CSS, images) from the template directory to the output directory.

    Args:
        config (Dict[str, Any]): The configuration dictionary.
    """
    html_config: Dict[str, Any] = config[ConfigKeys.OUTPUT_FORMATS.value][OutputFormatKeys.HTML.value]
    templates_config: Dict[str, Any] = html_config[CommonOutputKeys.TEMPLATES.value]
    static_config: Dict[str, Any] = templates_config[TemplateKeys.STATIC.value]
    output_dir: str = config[ConfigKeys.OUTPUT_DIR.value]

    # Copy CSS
    css_src: str = static_config[StaticAssetKeys.CSS.value]
    css_dest: str = os.path.join(output_dir, 'static', 'css')
    copy_directory(css_src, css_dest)

    # Copy images
    images_src: str = static_config[StaticAssetKeys.IMAGES.value]
    images_dest: str = os.path.join(output_dir, 'static', 'images')
    copy_directory(images_src, images_dest)

def copy_directory(src: str, dest: str) -> None:
    """
    Copy a directory from src to dest, creating dest if it doesn't exist.

    Args:
        src (str): The source directory path.
        dest (str): The destination directory path.
    """
    if not os.path.exists(src):
        logger.warning(f"Source directory does not exist: {src}")
        return

    os.makedirs(dest, exist_ok=True)
    for item in os.listdir(src):
        s = os.path.join(src, item)
        d = os.path.join(dest, item)
        if os.path.isdir(s):
            copy_directory(s, d)
        else:
            shutil.copy2(s, d)

if __name__ == "__main__":
    main()