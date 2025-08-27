# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.3.0] - 2025-08-27

### Added
- Publish `shaprpy` on PyPI
- Restructured the `shaprpy` package for improved modularity and maintainability
- Test suite with snapshot testing for output validation using syrupy
- GitHub Actions CI/CD pipeline for automated testing across Python versions
- Input validation tests for error handling

### Changed
- **BREAKING**: Minimum Python version requirement increased from 3.8+ to 3.10+
- Updated package classifiers to reflect Python 3.10+ requirement

### Dependencies
- Added `tabulate>=0.8.10` dependency for markdown table formatting in DataFrame comparisons

### Documentation
- Added Python 3.10+ requirement notice in README
- Improved installation instructions
- Added this changelog for better version tracking

## [0.2.1] - Previous Release

### Known Issues
- Limited testing coverage
- Some numerical precision issues in certain environments
- Python 3.8-3.9 compatibility issues with newer type annotations
