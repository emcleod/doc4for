from importlib.metadata import version, PackageNotFoundError

try:
    __version__ = version('doc4for')
except PackageNotFoundError:
    __version__ = 'unknown'