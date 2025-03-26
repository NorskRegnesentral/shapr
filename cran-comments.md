# shapr 1.0.3 (Patch release)

* Fix CRAN errors of type `Expected <nn_module> but got object of type <NULL>` occurring after the recent `torch` update, causing the `approach='vaeac'` to break. 
* Fix documentation issues detected during shapr 1.0.2 release
* Change default seed from 1 to NULL.
* Used skip_on_cran() to reduce check time on CRAN for snapshot tests which where not tested anyway
* Other minor fixes.
See NEWS.md for details.

## Test environments

The majority of our tests rely uses snapshots to check results reproducibility.
These tests are run locally and remotely with GHA (see below) without errors.
The win-builder and R-hub tests are run without snapshots tests (to replicate CRAN testing)

* GHA (ubuntu-latest), R-version: devel, release, oldrel-1, oldrel-2
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
