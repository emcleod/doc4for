import json
import logging
from typing import Dict, Any, Set
from pathlib import Path

logger = logging.getLogger(__name__)

# Type aliases for clarity
ConfigDict = Dict[str, Any]
FormatDict = Dict[str, Dict[str, Any]]

DEFAULT_CONFIG: ConfigDict = {
    "output_dir": "docs",
    "extensions": ["f90", "f95", "f03", "f08"],
    "output_formats": {
        "html": {
            "enabled": True,
            "use_default_css": True,
            "custom_css": None,
        },
        "pdf": {
            "enabled": False,
        },
        "markdown": {
            "enabled": False,
        },
    },
    "exclude_dirs": ["docs", ".git", "__pycache__", "build", "dist"],
    "author": None,
    "title": None,
    "version": None
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
    required_html_keys: Set[str] = {"enabled", "use_default_css", "custom_css"}
    missing_html_keys: Set[str] = required_html_keys - set(html_config.keys())
    if missing_html_keys:
        raise ValueError(f"Missing required HTML configuration keys: {missing_html_keys}")
    
    # Check other format structures 
    # TODO change this when we generate other output types
    for format_name in ["pdf", "markdown"]:
        format_config: Dict[str, Any] = formats[format_name]
        if "enabled" not in format_config:
            raise ValueError(f"{format_name.upper()} format configuration missing 'enabled' field")

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

def load_configuration(config_file_name: str) -> ConfigDict:
    """
    Load configuration from file, falling back to defaults if necessary.
    
    The function looks for a file named 'doc4for.json' in the current directory.
    If found, it loads and validates the configuration, merging it with defaults.
    If any issues are encountered, it falls back to the default configuration.
    
    Args:
        The name of the JSON configuration file.

    Returns:
        The loaded configuration, either from file or defaults.
    
    Raises:
        ValueError: If the merged configuration fails validation.
    
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
                user_config: ConfigDict = json.load(f)
                
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
                
                # For partial configurations, don't validate until after merge
                deep_update(config, user_config)
                
        except json.JSONDecodeError as e:
            logger.warning(f"Error reading configuration file: {e}. Using defaults.")
            return DEFAULT_CONFIG.copy()
    
    try:
        validate_config(config)
    except ValueError as e:
        logger.error(f"Configuration validation failed: {e}")
        raise
    
    return config