# Changelog

All notable changes to the Python `pyshapr` library will be documented
in this file. Changes to the underlying `shapr` R package is documented
in [shapr
CHANGELOG](https://norskregnesentral.github.io/shapr/news/index.html)

The format is based on [Keep a
Changelog](https://keepachangelog.com/en/1.0.0/), and this project
adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## \[Unreleased\]

### Added

- Added support for computing SAGE values (Shapley Additive Global
  importancE) via the new
  [`explain()`](https://norskregnesentral.github.io/shapr/reference/explain.md)
  arguments `scope` and `y_explain`. Set `scope="global"` to compute
  SAGE values. A custom Python loss function can be passed through
  `extra_computation_args={"global_loss_func": my_loss}` and is bridged
  into the R computation. Added the `Shapr.get_shap_values_est()`
  accessor for the per-observation Shapley values computed alongside the
  SAGE values, and made `Shapr.to_shap()` SAGE-aware (returns the single
  global loss explanation). (branch: sage)

## \[0.5.1\] - 2026-06-24

### Fixed

- Constrained `numpy<2.5` to keep `pip install pyshapr` resolvable on
  Python 3.12 and 3.13. No `numba` release yet supports numpy 2.5, so an
  unconstrained install (via the transitive `shap` -\> `numba`
  dependency) backtracked to an ancient, incompatible `numba` that fails
  to build. The cap may be lifted once `numba` supports numpy 2.5.

## \[0.5.0\] - 2026-06-23

### Changed

- **Renamed the package from `shaprpy` to `pyshapr`** (both the PyPI
  distribution name and the import name). Update installs to
  `pip install pyshapr` and imports to `import pyshapr`. This is the
  first release published under the `pyshapr` name. The previous
  `shaprpy` package remains available on PyPI for a transition period as
  a thin compatibility shim (released as `shaprpy 0.4.4`) that depends
  on and forwards to `pyshapr`.

## \[0.4.4\] - 2026-06-23

### Added

- Added basic snapshot tests for `approach="arf"` on both numerical and
  mixed categorical feature sets.
- Added an `arf`/`vaeac` end-to-end example script for numerical and
  mixed categorical feature sets.

### Changed

- Updated documentation to explicitly list `arf` and `vaeac` as
  supported approaches.
- Updated local `vaeac` snapshot tests to pass `vaeac_*` arguments
  directly to
  [`explain()`](https://norskregnesentral.github.io/shapr/reference/explain.md).
- Made the `python/` package pass `ruff check` and `ruff format`.

### Fixed

- Fixed minor `ruff` lint issues in the source code.

### Removed

- Dropped support for Python 3.10. The minimum supported version is now
  Python 3.11.

## \[0.4.3\] - 2026-01-24

### Fixed

- Fixed compatibility with pandas 3.0 for categorical data handling in
  tests.
- Fixed XGBoost model support when `feature_names_in_` attribute is not
  available.

## \[0.4.2\] - 2026-01-23

### Fixed

- Adding predictor names to DMatrix when using XGBoost models to avoid
  issues with missing feature names.

## \[0.4.1\] - 2025-12-22

### Added

- **NEW**: `ShaprSummary` class for enhanced summary functionality
  - Dedicated class for handling summary results with improved display
    and access methods
  - Support for tab-completion in IPython/Jupyter environments
- Adjusted the unit tests to cover the new `ShaprSummary` class
  functionality

### Changed

- Update `class_exploration.py` example to demonstrate the new
  `ShaprSummary` class

## \[0.4.0\] - 2025-11-28

### Added

- **NEW**: `Shapr` class for exploration and analysis of explanation
  results
  - Object-oriented interface for working with Shapley value
    explanations
  - Methods for extracting results:
    [`get_results()`](https://norskregnesentral.github.io/shapr/reference/get_results.md),
    `get_explanation_dict()`, `get_r_object()`
  - Summary and printing functionality:
    [`summary()`](https://rdrr.io/r/base/summary.html),
    [`print()`](https://rdrr.io/r/base/print.html)
  - Integration with SHAP library via `to_shap()` method for plotting
- Enhanced example scripts demonstrating new class-based functionality
- Added comprehensive unit and snapshot tests covering all approaches
  and full functionality, including categorical features and causal
  ordering

### Changed

- **BREAKING**:
  [`explain()`](https://norskregnesentral.github.io/shapr/reference/explain.md)
  function now returns a `Shapr` object instead of a plain dictionary
- Restructured internal module organization with private `_explain.py`
  and `_rutils.py` modules
- Updated all example scripts, tests and README to demonstrate new
  class-based API
- Improved lazy loading for R package dependencies
- Cleaned up unused library components

### Fixed

- Enhanced R package import reliability by removing empty paths from
  library locations, effectively resolving confusing warnings from R on
  package load
- Fixed a bug in handling categorical features (R factors) during
  conversion to Python
- Fixed an issue with passing boolean vectors for the `confounding`
  argument

## \[0.3.0\] - 2025-08-27

### Added

- Publish `shaprpy` on PyPI
- Restructured the `shaprpy` package for improved modularity and
  maintainability
- Test suite with snapshot testing for output validation using syrupy
- GitHub Actions CI/CD pipeline for automated testing across Python
  versions
- Input validation tests for error handling

### Changed

- **BREAKING**: Minimum Python version requirement increased from 3.8+
  to 3.10+
- Updated package classifiers to reflect Python 3.10+ requirement

### Dependencies

- Added `tabulate>=0.8.10` dependency for markdown table formatting in
  DataFrame comparisons

### Documentation

- Added Python 3.10+ requirement notice in README
- Improved installation instructions
- Added this changelog for better version tracking

## \[0.2.1\] - Previous Release

### Known Issues

- Limited testing coverage
- Some numerical precision issues in certain environments
- Python 3.8-3.9 compatibility issues with newer type annotations
