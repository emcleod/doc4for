import json
import logging
from typing import Dict, Any, Set, Optional
from pathlib import Path
from enum import Enum, auto

logger = logging.getLogger(__name__)

ConfigDict = Dict[str, Any]
FormatDict = Dict[str, Dict[str, Any]]


class ConfigKeys(Enum):
    OUTPUT_FORMATS = "output_formats"
    EXTENSIONS = "extensions"
    EXCLUDE_DIRS = "exclude_dirs"
    OUTPUT_DIR = "output_dir"
    AUTHOR = "author"
    TITLE = "title"
    VERSION = "version"
    INCLUDE_PRIVATE = "include_private"


class OutputFormatKeys(Enum):
    HTML = "html"
    MARKDOWN = "markdown"
    PDF = "pdf"


class CommonOutputKeys(Enum):
    ENABLED = "enabled"
    TEMPLATES = "templates"


class HTMLKeys(Enum):
    STATIC = "static"
    CSS = "css"
    IMAGES = "images"
    ROOT_DIR = "root_dir"
    SYNTAX_HIGHLIGHTING = "syntax_highlighting"
    TEMPLATE_DIR = "template_dir"
    SINGLE_PAGE = "single_page"


class DirectoryTypes(Enum):
    ROOT = auto()
    CSS = auto()
    IMAGES = auto()

    def __str__(self):
        return self.name.capitalize()


def normalize_path(path: str) -> str:
    """Normalize a path to absolute path with consistent separators."""
    return str(Path(path).resolve())


def normalize_extensions(extensions: list) -> list:
    """Convert all extensions to lowercase."""
    return [ext.lower() for ext in extensions]


def normalize_dirs(dirs: list) -> list:
    """Convert all directory names to lowercase."""
    return [d.lower() for d in dirs]


DEFAULT_CONFIG: ConfigDict = {
    ConfigKeys.OUTPUT_DIR.value: normalize_path("docs"),
    ConfigKeys.EXTENSIONS.value: normalize_extensions(["f90", "f95", "f03", "f08"]),
    ConfigKeys.OUTPUT_FORMATS.value: {
        OutputFormatKeys.HTML.value: {
            CommonOutputKeys.ENABLED.value: True,
            CommonOutputKeys.TEMPLATES.value: {
                HTMLKeys.ROOT_DIR.value: normalize_path("templates"),
                HTMLKeys.STATIC.value: {
                    HTMLKeys.CSS.value: normalize_path("static/css"),
                    HTMLKeys.IMAGES.value: normalize_path("static/images")
                }
            },
            HTMLKeys.SYNTAX_HIGHLIGHTING.value: True,
            HTMLKeys.TEMPLATE_DIR.value: None,
            HTMLKeys.SINGLE_PAGE.value: False
        },
        OutputFormatKeys.PDF.value: {
            CommonOutputKeys.ENABLED.value: False,
        },
        OutputFormatKeys.MARKDOWN.value: {
            CommonOutputKeys.ENABLED.value: False,
        },
    },
    ConfigKeys.EXCLUDE_DIRS.value: normalize_dirs(["docs", ".git", "__pycache__", "build", "dist"]),
    ConfigKeys.AUTHOR.value: None,
    ConfigKeys.TITLE.value: None,
    ConfigKeys.VERSION.value: None,
    ConfigKeys.INCLUDE_PRIVATE.value: False
}


def load_configuration(config_file_name: str = 'doc4for.json') -> ConfigDict:
    """
    Load configuration from file, falling back to defaults if necessary.

    Args:
        config_file_name: The name of the configuration file to load.

    Returns:
        The loaded configuration, either from file or defaults.

    Raises:
        ValueError: If the configuration is invalid.
    """
    config = DEFAULT_CONFIG.copy()
    user_config = load_user_config(config_file_name)
    if user_config:
        merge_configs(config, user_config)
    normalize_config_paths(config)
    validate_config(config)
    return config


def load_user_config(config_file_name: str) -> Optional[ConfigDict]:
    """
    Load and parse the user configuration file.

    Args:
        config_file_name: The name of the configuration file to load.

    Returns:
        The parsed user configuration, or None if issues occurred.
    """
    config_file = Path(config_file_name)
    if not config_file.exists():
        return None

    try:
        with open(config_file, 'r') as f:
            content = f.read().strip()
            if not content:
                logger.warning("Configuration file is empty. Using defaults.")
                return None

            user_config = json.loads(content)
            if not user_config:
                logger.warning("Configuration file is empty. Using defaults.")
                return None

            return user_config
    except json.JSONDecodeError as e:
        logger.warning(f"Error reading configuration file: {
                       e}. Using defaults.")
        return None


def merge_configs(base_config: ConfigDict, user_config: ConfigDict) -> None:
    """
    Merge user configuration into base configuration.

    Args:
        base_config: The base configuration to update.
        user_config: The user configuration to merge.
    """
    unknown_keys = set(user_config.keys()) - set(DEFAULT_CONFIG.keys())
    if unknown_keys:
        logger.warning(
            f"Unknown configuration keys found and ignored: {unknown_keys}")
        user_config = {k: v for k, v in user_config.items()
                       if k in DEFAULT_CONFIG}

    if ConfigKeys.EXTENSIONS.value in user_config:
        user_config[ConfigKeys.EXTENSIONS.value] = normalize_extensions(
            user_config[ConfigKeys.EXTENSIONS.value])
    if ConfigKeys.EXCLUDE_DIRS.value in user_config:
        user_config[ConfigKeys.EXCLUDE_DIRS.value] = normalize_dirs(
            user_config[ConfigKeys.EXCLUDE_DIRS.value])

    deep_update(base_config, user_config)


def normalize_config_paths(config: ConfigDict) -> None:
    """
    Normalize all paths in the configuration.

    Args:
        config: The configuration to normalize paths in.
    """
    config[ConfigKeys.OUTPUT_DIR.value] = normalize_path(
        config[ConfigKeys.OUTPUT_DIR.value])
    html_config = config[ConfigKeys.OUTPUT_FORMATS.value][OutputFormatKeys.HTML.value]
    if html_config[CommonOutputKeys.TEMPLATES.value][HTMLKeys.ROOT_DIR.value]:
        html_config[CommonOutputKeys.TEMPLATES.value][HTMLKeys.ROOT_DIR.value] = normalize_path(
            html_config[CommonOutputKeys.TEMPLATES.value][HTMLKeys.ROOT_DIR.value])

    static_config = html_config[CommonOutputKeys.TEMPLATES.value][HTMLKeys.STATIC.value]

    css_path = static_config[HTMLKeys.CSS.value]
    if css_path:
        static_config[HTMLKeys.CSS.value] = normalize_path(css_path)

    images_path = static_config[HTMLKeys.IMAGES.value]
    if images_path:
        static_config[HTMLKeys.IMAGES.value] = normalize_path(images_path)


def validate_config(config: ConfigDict) -> None:
    """
    Validate the configuration structure and values.

    Args:
        config: The configuration dictionary to validate.

    Raises:
        ValueError: If any required keys are missing or if the structure is invalid.
    """
    validate_required_keys(config)
    validate_output_formats(config[ConfigKeys.OUTPUT_FORMATS.value])


def validate_required_keys(config: ConfigDict) -> None:
    """
    Check for required top-level keys in the configuration.

    Args:
        config: The configuration dictionary to check.

    Raises:
        ValueError: If any required keys are missing.
    """
    required_keys: Set[str] = {ConfigKeys.OUTPUT_DIR.value, 
                               ConfigKeys.EXTENSIONS.value,
                               ConfigKeys.OUTPUT_FORMATS.value, 
                               ConfigKeys.EXCLUDE_DIRS.value}
    missing_keys: Set[str] = required_keys - set(config.keys())
    if missing_keys:
        raise ValueError(
            f"Missing required configuration keys: {missing_keys}")


def validate_output_formats(formats: FormatDict) -> None:
    """
    Validate the output formats configuration.

    Args:
        formats: The output formats configuration to validate.

    Raises:
        ValueError: If any required formats are missing or invalid.
    """
    required_formats = {format_key.value for format_key in OutputFormatKeys}
    missing_formats = required_formats - set(formats.keys())
    if missing_formats:
        raise ValueError(f"Missing required output formats: {
                         ', '.join(f for f in missing_formats)}")

    for format_key in OutputFormatKeys:
        format_config = formats[format_key.value]
        if not isinstance(format_config, dict):
            raise ValueError(
                f"{format_key.value} format configuration must be a dictionary")

        if format_key == OutputFormatKeys.HTML:
            validate_html_config(format_config)
        elif format_key == OutputFormatKeys.PDF:
            validate_pdf_config(format_config)
        elif format_key == OutputFormatKeys.MARKDOWN:
            validate_markdown_config(format_config)
        else:
            raise ValueError(f"Unhandled format type: {format_key}")


def validate_html_config(html_config: Dict[str, Any]) -> None:
    """
    Validate the HTML configuration.

    Args:
        html_config: The HTML configuration to validate.

    Raises:
        ValueError: If the HTML configuration is invalid.
    """
    required_html_keys: Set[str] = {CommonOutputKeys.ENABLED.value,
                                    CommonOutputKeys.TEMPLATES.value,
                                    HTMLKeys.SYNTAX_HIGHLIGHTING.value,
                                    HTMLKeys.TEMPLATE_DIR.value,
                                    HTMLKeys.SINGLE_PAGE.value}
    missing_html_keys: Set[str] = required_html_keys - set(html_config.keys())
    if missing_html_keys:
        raise ValueError(f"Missing required HTML configuration keys: {
                         missing_html_keys}")

    if CommonOutputKeys.TEMPLATES.value in html_config:
        _validate_templates_structure(
            html_config[CommonOutputKeys.TEMPLATES.value])


def validate_pdf_config(config: Dict[str, Any]) -> None:
    """
    Validate the PDF configuration.

    Args:
        config: The PDF configuration to validate.

    Raises:
        ValueError: If the configuration is invalid.
    """
    if CommonOutputKeys.ENABLED.value not in config:
        raise ValueError(f"PDF format configuration missing '{
                         CommonOutputKeys.ENABLED.value}' field")


def validate_markdown_config(config: Dict[str, Any]) -> None:
    """
    Validate the Markdown configuration.

    Args:
        config: The Markdown configuration to validate.

    Raises:
        ValueError: If the configuration is invalid.
    """
    if CommonOutputKeys.ENABLED.value not in config:
        raise ValueError(f"Markdown format configuration missing '{
                         CommonOutputKeys.ENABLED.value}' field")


def _validate_templates_structure(templates_config: Dict[str, Any]) -> None:
    """
    Validate the structure of templates configuration.

    Args:
        templates_config: The templates configuration to validate.

    Raises:
        ValueError: If the templates structure is invalid or required paths don't exist.
    """
    validate_template_keys(templates_config)
    validate_static_config(templates_config[HTMLKeys.STATIC.value])
    validate_template_directories(templates_config)


def validate_template_keys(templates_config: Dict[str, Any]) -> None:
    """
    Check for required keys in templates configuration.

    Args:
        templates_config: The templates configuration to check.

    Raises:
        ValueError: If any required keys are missing.
    """
    required_keys: Set[str] = {HTMLKeys.ROOT_DIR.value, HTMLKeys.STATIC.value}
    missing_keys: Set[str] = required_keys - set(templates_config.keys())
    if missing_keys:
        raise ValueError(f"Missing required templates keys: {missing_keys}")


def validate_static_config(static_config: Dict[str, Any]) -> None:
    """
    Check for required keys in static configuration.

    Args:
        static_config: The static configuration to check.

    Raises:
        ValueError: If any required keys are missing.
    """
    required_static_keys: Set[str] = {
        HTMLKeys.CSS.value, HTMLKeys.IMAGES.value}
    missing_static_keys: Set[str] = required_static_keys - \
        set(static_config.keys())
    if missing_static_keys:
        raise ValueError(f"Missing required static configuration keys: {
                         missing_static_keys}")


def validate_template_directories(templates_config: Dict[str, Any]) -> None:
    """
    Check if required template directories exist.

    Args:
        templates_config: The templates configuration to check.

    Raises:
        ValueError: If any required directories don't exist.
    """
    check_directory_exists(
        templates_config[HTMLKeys.ROOT_DIR.value], DirectoryTypes.ROOT)

    static_config = templates_config[HTMLKeys.STATIC.value]
    check_directory_exists(
        static_config[HTMLKeys.CSS.value], DirectoryTypes.CSS)

    if static_config[HTMLKeys.IMAGES.value] is not None:
        check_directory_exists(
            static_config[HTMLKeys.IMAGES.value], DirectoryTypes.IMAGES)


def check_directory_exists(path: str, dir_type: DirectoryTypes) -> None:
    """
    Check if a directory exists and raise an error if it doesn't.

    Args:
        path: The path to check.
        dir_type: The type of directory being checked.

    Raises:
        ValueError: If the directory doesn't exist.
    """
    dir_path = Path(normalize_path(path))
    if not dir_path.exists() or not dir_path.is_dir():
        raise ValueError(
            f"{dir_type.name} directory does not exist: {dir_path}")


def deep_update(base_dict: ConfigDict, update_dict: ConfigDict) -> None:
    """
    Recursively update a dictionary with values from another dictionary.

    Args:
        base_dict: The dictionary to be updated.
        update_dict: The dictionary containing the updates.
    """
    for k, v in update_dict.items():
        if isinstance(v, dict) and k in base_dict and isinstance(base_dict[k], dict):
            deep_update(base_dict[k], v)
        else:
            base_dict[k] = v
