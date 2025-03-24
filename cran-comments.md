# shapr 1.0.3 (Patch release)

* Fix CRAN errors of type `Expected <nn_module> but got object of type <NULL>` occurring after the recent `torch` update, causing the `approach='vaeac'` to break. 
* Fix documentation issues detected during shapr 1.0.2 release
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
* R-hub (clang-asan): R-version: devel


## R CMD check results

There were no ERRORs, WARNINGs

There were in total 1 NOTE

### NOTE (multiple platforms):

* checking installed package size ... NOTE
  installed size is  7.1Mb
  sub-directories of 1Mb or more:
    doc    4.2Mb
    libs   1.3Mb

> The package is growing in size, uses more complied code, and the documentation is comprehensive.

