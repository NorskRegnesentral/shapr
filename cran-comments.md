# shapr 1.0.4 (Patch release)

* Fix CRAN errors of the noSuggests type by using `testthat::skip_if_not_installed()` for all tests requiring suggested 
packages to ensure they are skipped gracefully when dependencies are unavailable.
* Some other new functionality and a few other bug fixes, see NEWS.md for details.

## Test environments

The majority of our tests rely uses snapshots to check results reproducibility.
These tests are run locally and remotely with GHA (see below) without errors.
The win-builder and R-hub tests are run without snapshots tests (to replicate CRAN testing).
Locally, tests are run both with and without suggested packages.

* Locally (Ubuntu 20.04.6), R-version 4.4.1 (with all packages and in a .libPath without suggested packages)

* GHA (ubuntu-latest), R-versions: devel, release, oldrel-1, oldrel-2
* GHA (windows-latest), R-version: release
* GHA (macOS-latest), R-version: release
* devtools-win-builder, R-versions: devel, release, oldrelease 
* devools-mac-builder, R-versions: release 
* R-hub (ubuntu-latest): R-version: devel
* R-hub (windows-latest): R-version: devel
* R-hub (mac-latest): R-version: devel
* R-hub (clang-asan): R-version: devel


## R CMD check results

There were no ERRORs, WARNINGs or NOTEs
