from .logging_config import setup_logging
from importlib.metadata import version, PackageNotFoundError

setup_logging()

try:
    __version__ = version('doc4for')
except PackageNotFoundError:
    __version__ = 'unknown'