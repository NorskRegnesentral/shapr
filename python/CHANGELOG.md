# Changelog

All notable changes to the Python `shaprpy` library will be documented in this file.
Changes to the underlying `shapr` R package is documented in
[shapr CHANGELOG](https://norskregnesentral.github.io/shapr/news/index.html)

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.4.0] - 2025-11-28

### Added
- **NEW**: `Shapr` class for exploration and analysis of explanation results
  - Object-oriented interface for working with Shapley value explanations
  - Methods for extracting results: `get_results()`, `get_explanation_dict()`, `get_r_object()`
  - Summary and printing functionality: `summary()`, `print()`
  - Integration with SHAP library via `to_shap()` method for plotting
- Enhanced example scripts demonstrating new class-based functionality
- Added comprehensive unit and snapshot tests covering all approaches and full functionality,
including categorical features and causal ordering

### Changed
- **BREAKING**: `explain()` function now returns a `Shapr` object instead of a plain dictionary
- Restructured internal module organization with private `_explain.py` and `_rutils.py` modules
- Updated all example scripts, tests and README to demonstrate new class-based API
- Improved lazy loading for R package dependencies
- Cleaned up unused library components

### Fixed
- Enhanced R package import reliability by removing empty paths from library locations,
effectively resolving confusing warnings from R on package load
- Fixed a bug in handling categorical features (R factors) during conversion to Python
- Fixed an issue with passing boolean vectors for the `confounding` argument


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
