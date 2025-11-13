# shapr 1.0.6

This release (v1.0.6) adds support for extracting the programming language via `get_results(what = "proglang")` and 
introduces a new print_ggplot argument to ensure `plot.shapr()` and related plot functions reliably display ggplots 
when sourced or called inside functions/loops.


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


## Current R CMD check results

There were no ERRORs, WARNINGs or NOTEs

## R CMD check results on CRAN servers:

* NOTE on package size on r-oldrel: The installed package is ~9.7 MB. 
  This does not occur during testing of the new release and on newer R versions.

* On r-release-macos-x86_64, the CRAN check log shows  
  `Error writing to connection: No space left on device`,  
  which causes spurious Rd “\title/\name must exist” warnings. 
  These do **not** appear on any other platform and do not occur locally or on CI. 
  This appears to be an issue with the check machine itself.


