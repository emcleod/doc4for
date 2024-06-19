from setuptools import setup, find_packages

setup(
    name='doc4for',
    version='0.1',
    packages=find_packages(where='src'),
    package_dir={'': 'src'},
)

from setuptools import setup, find_packages

setup(
    name='doc4for',
    version='0.1',
    packages=find_packages(where='src'),
    package_dir={'': 'src'},
    description='A tool for generating documentation for Fortran code',
    long_description=open('README.md').read(),
    long_description_content_type='text/markdown',
    author='emcleod',
    author_email='',
    url='https://github.com/emcleod/doc4for',
    install_requires=[
        'fparser>=0.1.4'
    ],
    classifiers=[
        'Programming Language :: Python :: 3',
        'License :: OSI Approved :: MIT License',
        'Operating System :: OS Independent',
    ],
    python_requires='>=3.6',
)