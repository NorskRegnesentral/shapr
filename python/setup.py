from setuptools import setup, find_packages

setup(
    name="shaprpy",
    version="0.2",
    author="Martin Jullum, Lars Henry Berge Olsen, Didrik Nielsen",
    author_email="jullum@nr.no",
    description="Wrapper for R-package shapr",
    long_description="",
    long_description_content_type="text/markdown",
    install_requires=[
        'rpy2>=3.5.1', 
        'numpy>=1.22.3',
        'pandas>=1.4.2',
        'scikit-learn>=1.0.0',
    ],
    packages=[
        'shaprpy'
    ],
)
