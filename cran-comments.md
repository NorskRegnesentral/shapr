# shapr 1.0.8

This release (v1.0.8) includes some enhancements to plotting defaults, compatibility fixes for the `forecast` package's ARIMA class name changes, improved documentation terminology, and some minor documentation improvements.

## Test environments

The majority of our tests use snapshots to check results reproducibility.
These tests are run locally and remotely with GHA (see below) without errors.
The win-builder and R-hub tests are run without snapshots tests (to replicate CRAN testing).
Locally, tests are run both with and without suggested packages.

* Locally (Ubuntu 24.04.3 LTS), R-version 4.3.3
* GHA (ubuntu-latest), R-versions: devel, release, oldrel-1, oldrel-2
* GHA (windows-latest), R-version: release
* GHA (macOS-latest), R-version: release
* devtools-win-builder, R-versions: devel, release, oldrelease
* R-hub (ubuntu-latest): R-version: devel
* R-hub (windows-latest): R-version: devel
* R-hub (clang-asan): R-version: devel

## Current R CMD check results

There were no ERRORs, WARNINGs, or NOTES.

## Reverse dependencies
We checked 1 reverse dependencies (0 from CRAN + 1 from Bioconductor),
comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages
