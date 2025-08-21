# shapr 1.0.5

This release (v1.0.5) adds new helper methods (`get_results()`, `summary.shapr()`), improves printing, 
output handling, and documentation, and fixes a bug affecting cases with many features.

## Test environments

The majority of our tests rely uses snapshots to check results reproducibility.
These tests are run locally and remotely with GHA (see below) without errors.
The win-builder and R-hub tests are run without snapshots tests (to replicate CRAN testing).
Locally, tests are run both with and without suggested packages.

* Locally (Windows 11 x64, build 26100), R-version 4.5.1

* GHA (ubuntu-latest), R-versions: devel, release, oldrel-1, oldrel-2
* GHA (windows-latest), R-version: release
* GHA (macOS-latest), R-version: release
* devtools-win-builder, R-versions: devel, release, oldrelease 
* devtools-mac-builder, R-versions: release 
* R-hub (ubuntu-latest): R-version: devel
* R-hub (windows-latest): R-version: devel
* R-hub (mac-latest): R-version: devel
* R-hub (clang-asan): R-version: devel


## R CMD check results

There were no ERRORs, WARNINGs or NOTEs
