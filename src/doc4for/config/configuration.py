import json
import logging
import os
from typing import Dict, Any, Set
from pathlib import Path

logger = logging.getLogger(__name__)

# Type aliases for clarity
ConfigDict = Dict[str, Any]
FormatDict = Dict[str, Dict[str, Any]]

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
    "output_dir": normalize_path("docs"),
    "extensions": normalize_extensions(["f90", "f95", "f03", "f08"]),
    "output_formats": {
        "html": {
            "enabled": True,
            "templates": {
                "root_dir": normalize_path("templates"),
                "static": {
                    "css": normalize_path("static/css"),
                    "images": normalize_path("static/images")
                }
            },
            "syntax_highlighting": True,
            "template_dir": None,
            "single_page": False
        },
        "pdf": {
            "enabled": False,
        },
        "markdown": {
            "enabled": False,
        },
    },
    "exclude_dirs": normalize_dirs(["docs", ".git", "__pycache__", "build", "dist"]),
    "author": None,
    "title": None,
    "version": None,
    "include_private": False
}

def validate_config(config: ConfigDict) -> None:
    """
    Validate the configuration structure and values.
    
    Args:
        config: The configuration dictionary to validate.
    
    Raises:
        ValueError: If any required keys are missing or if the structure is invalid.
    
    Example:
        >>> config = DEFAULT_CONFIG.copy()
        >>> validate_config(config)  # No exception raised
        >>> del config['output_dir']
        >>> validate_config(config)  # Raises ValueError
    """
    # Check required top-level keys
    required_keys: Set[str] = {"output_dir", "extensions", "output_formats", "exclude_dirs"}
    missing_keys: Set[str] = required_keys - set(config.keys())
    if missing_keys:
        raise ValueError(f"Missing required configuration keys: {missing_keys}")
    
    # Check output_formats structure
    formats: FormatDict = config["output_formats"]
    required_formats: Set[str] = {"html", "pdf", "markdown"}
    missing_formats: Set[str] = required_formats - set(formats.keys())
    if missing_formats:
        raise ValueError(f"Missing required output formats: {missing_formats}")
    
    # Check html format structure
    html_config: Dict[str, Any] = formats["html"]
    required_html_keys: Set[str] = {"enabled", "templates", "syntax_highlighting", 
                                    "template_dir", "single_page"}
    missing_html_keys: Set[str] = required_html_keys - set(html_config.keys())
    if missing_html_keys:
        raise ValueError(f"Missing required HTML configuration keys: {missing_html_keys}")
    
    # Validate templates structure
    if "templates" in html_config:
        _validate_templates_structure(html_config["templates"])
    
    # Check other format structures 
    for format_name in ["pdf", "markdown"]:
        format_config: Dict[str, Any] = formats[format_name]
        if "enabled" not in format_config:
            raise ValueError(f"{format_name.upper()} format configuration missing 'enabled' field")

def _validate_templates_structure(templates_config: Dict[str, Any]) -> None:
    """
    Validate the structure of templates configuration.
    
    Args:
        templates_config: The templates configuration dictionary to validate.
    
    Raises:
        ValueError: If the templates structure is invalid or required paths don't exist.
    """
    required_keys: Set[str] = {"root_dir", "static"}
    missing_keys: Set[str] = required_keys - set(templates_config.keys())
    if missing_keys:
        raise ValueError(f"Missing required templates keys: {missing_keys}")
    
    static_config: Dict[str, Any] = templates_config["static"]
    required_static_keys: Set[str] = {"css", "images"}
    missing_static_keys: Set[str] = required_static_keys - set(static_config.keys())
    if missing_static_keys:
        raise ValueError(f"Missing required static configuration keys: {missing_static_keys}")
    
    # Check if root_dir exists
    root_dir: Path = Path(normalize_path(templates_config["root_dir"]))
    if not root_dir.exists() or not root_dir.is_dir():
        raise ValueError(f"Templates root directory does not exist: {root_dir}")
    
    # Check if CSS directory exists
    css_path = normalize_path(templates_config["static"]["css"])
    css_dir: Path = Path(css_path)
    if not css_dir.exists() or not css_dir.is_dir():
        raise ValueError(f"CSS directory does not exist: {css_dir}")
    
    # Images can be None or an existing directory
    if templates_config["static"]["images"] is not None:
        images_path = normalize_path(templates_config["static"]["images"])
        images_dir: Path = Path(images_path)
        if not images_dir.exists() or not images_dir.is_dir():
            raise ValueError(f"Images directory does not exist: {images_dir}")
                
def deep_update(base_dict: ConfigDict, update_dict: ConfigDict) -> None:
    """
    Recursively update a dictionary with values from another dictionary.
    
    Args:
        base_dict: The dictionary to be updated.
        update_dict: The dictionary containing the updates.
    
    Example:
        >>> base = {"a": 1, "b": {"c": 2}}
        >>> update = {"b": {"d": 3}}
        >>> deep_update(base, update)
        >>> base
        {"a": 1, "b": {"c": 2, "d": 3}}
    """
    for k, v in update_dict.items():
        if isinstance(v, dict) and k in base_dict and isinstance(base_dict[k], dict):
            deep_update(base_dict[k], v)
        else:
            base_dict[k] = v

def load_configuration(config_file_name: str = 'doc4for.json') -> ConfigDict:
    """
    Load configuration from file, falling back to defaults if necessary.
    
    Args:
        config_file_name: The name of the configuration file to load (default: 'doc4for.json')
    
    Returns:
        Dict[str, Any]: The loaded configuration, either from file or defaults
    
    Example:
        >>> config = load_configuration()
        >>> config['output_dir']
        'docs'
    """
    config: ConfigDict = DEFAULT_CONFIG.copy()
    config_file: Path = Path(config_file_name)
    
    if config_file.exists():
        try:
            with open(config_file, 'r') as f:
                content = f.read().strip()
                if not content:
                    logger.warning("Configuration file is empty. Using defaults.")
                    return DEFAULT_CONFIG.copy()
                
                user_config: ConfigDict = json.loads(content)
                
                if not user_config:
                    logger.warning("Configuration file is empty. Using defaults.")
                    return DEFAULT_CONFIG.copy()
                
                # Check for unknown keys
                unknown_keys: Set[str] = set(user_config.keys()) - set(DEFAULT_CONFIG.keys())
                if unknown_keys:
                    logger.warning(f"Unknown configuration keys found and ignored: {unknown_keys}")
                    # Remove unknown keys
                    user_config = {k: v for k, v in user_config.items() 
                                  if k in DEFAULT_CONFIG}

                # Normalize extensions and exclude_dirs in user config
                if 'extensions' in user_config:
                    user_config['extensions'] = normalize_extensions(user_config['extensions'])
                if 'exclude_dirs' in user_config:
                    user_config['exclude_dirs'] = normalize_dirs(user_config['exclude_dirs'])
                                
                # For partial configurations, don't validate until after merge
                deep_update(config, user_config)
                
        except json.JSONDecodeError as e:
            logger.warning(f"Error reading configuration file: {e}. Using defaults.")
            return DEFAULT_CONFIG.copy()
    
    # Normalize paths in the final config
    config['output_dir'] = normalize_path(config['output_dir'])
    if config['output_formats']['html']['templates']['root_dir']:
        config['output_formats']['html']['templates']['root_dir'] = \
            normalize_path(config['output_formats']['html']['templates']['root_dir'])
    
    css_path = config['output_formats']['html']['templates']['static']['css']
    if css_path:
        config['output_formats']['html']['templates']['static']['css'] = normalize_path(css_path)
    
    images_path = config['output_formats']['html']['templates']['static']['images']
    if images_path:
        config['output_formats']['html']['templates']['static']['images'] = normalize_path(images_path)
    
    try:
        validate_config(config)
    except ValueError as e:
        logger.error(f"Configuration validation failed: {e}")
        raise
    
    return config

