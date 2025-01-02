import logging

def setup_logging():
    logging.basicConfig(
        level=logging.INFO,
        format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
        datefmt='%Y-%m-%d %H:%M:%S'
    )
    # TODO optionally write fparser logs to a file?
    logging.getLogger('fparser').setLevel(logging.CRITICAL)
    logging.getLogger('jinja2').setLevel(logging.CRITICAL)
    
setup_logging()