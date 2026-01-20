# shapr 1.0.8

## Resubmission
This is a resubmission after the package was temporarily archived due to a missed notification about test failures.

This release (v1.0.7) is mainly a patch for the failing example script caught by CRAN.
Unfortunately, the email notification from CRAN about the failure got lost in our inbox,
so we only discovered it after the deadline (Dec 17th), causing our package to be temporarily archived on CRAN.

The example failure was caused by the recent update to `xgboost` which introduced breaking changes to their class structure,
which we unfortunately missed in our previous testing.
This patch updates the relevant code parts to ensure proper handling of the new `xgboost` model objects,
now supporting both the old and new class structures.
We have also updated our CRAN test suite to include tests for all models supported by `shapr`, ensuring better detection of such issues in the future.

While at it, we have also made some other minor package improvements (add summary class, set default in print method and additional tests).

## Test environments

The majority of our tests use snapshots to check results reproducibility.
These tests are run locally and remotely with GHA (see below) without errors.
The win-builder and R-hub tests are run without snapshots tests (to replicate CRAN testing).
Locally, tests are run both with and without suggested packages.

* Locally (Windows 11 x64, build 26100), R-version 4.5.1
* GHA (ubuntu-latest), R-versions: devel, release, oldrel-1, oldrel-2
* GHA (windows-latest), R-version: release
* GHA (macOS-latest), R-version: release
* devtools-win-builder, R-versions: devel, release, oldrelease
* R-hub (ubuntu-latest): R-version: devel
* R-hub (windows-latest): R-version: devel
* R-hub (mac-latest): R-version: devel
* R-hub (clang-asan): R-version: devel

## Current R CMD check results

There were no ERRORs, WARNINGs, or NOTES:


## Reverse dependencies
We checked 1 reverse dependencies (0 from CRAN + 1 from Bioconductor),
comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages
