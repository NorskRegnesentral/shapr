# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.3.0] - 2025-08-27

### Added
- Comprehensive test suite with snapshot testing for output validation using syrupy
- Support for multiple explanation approaches: empirical, gaussian, ctree, regression_separate, regression_surrogate
- Tests for feature grouping and causal ordering functionality
- GitHub Actions CI/CD pipeline for automated testing across Python versions
- Markdown-formatted snapshot comparisons for better readability and diff visualization
- Test coverage for classification and regression models (RandomForest, XGBoost)
- Input validation tests for error handling
- Support for custom regression models as strings in regression-based approaches

### Changed
- **BREAKING**: Minimum Python version requirement increased from 3.8+ to 3.10+
- Improved numerical stability in tests with controlled precision (6 decimal places)
- Enhanced test coverage across different model types and explanation methods
- Updated package classifiers to reflect Python 3.10+ requirement
- Migrated from dictionary-based to markdown table format for snapshot comparisons

### Fixed
- Resolved numerical precision issues causing test failures across different environments
- Fixed DataFrame display truncation issues in snapshot comparisons
- Improved reproducibility of test results between local and CI environments
- Fixed Python 3.9 compatibility issues with union type syntax (`str | list[str]`)

### Dependencies
- Added `tabulate>=0.8.10` dependency for markdown table formatting in DataFrame comparisons

### Documentation
- Added Python 3.10+ requirement notice in README
- Improved installation instructions
- Added this changelog for better version tracking

## [0.2.1] - Previous Release

### Added
- Initial Python wrapper for R shapr package via rpy2
- Basic explanation functionality for scikit-learn models
- Support for empirical and independence approaches
- Example datasets (California housing, binary iris)
- Basic usage examples and documentation
- Support for XGBoost and RandomForest models

### Known Issues
- Limited testing coverage
- Some numerical precision issues in certain environments
- Python 3.8-3.9 compatibility issues with newer type annotations
