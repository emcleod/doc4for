import logging

def setup_logging(log_level=logging.INFO):
    logging.basicConfig(
        level=log_level,
        format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
        datefmt='%Y-%m-%d %H:%M:%S'
    )
    # TODO optionally write fparser logs to a file?
    logging.getLogger('fparser').setLevel(logging.CRITICAL)
    logging.getLogger('jinja2').setLevel(logging.CRITICAL)

def initial_setup():
    setup_logging(logging.WARNING)  

initial_setup()
