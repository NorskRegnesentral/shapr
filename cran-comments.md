# shapr 1.0.7

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
* Locally (Windows 11 x64, build 26100), R-devel (2025-12-18 r89199 ucrt, with dev-version of data.table (1.17.99))

* GHA (ubuntu-latest), R-versions: devel, release, oldrel-1, oldrel-2
* GHA (windows-latest), R-version: release
* GHA (macOS-latest), R-version: release
* devtools-win-builder, R-versions: devel, release, oldrelease
* R-hub (ubuntu-latest): R-version: devel
* R-hub (windows-latest): R-version: devel
* R-hub (mac-latest): R-version: devel
* R-hub (clang-asan): R-version: devel

Note: devtools-mac-builder is currently down, and therefore not included in the testing.

## Current R CMD check results

There were 1 ERROR flavor, no WARNINGs, and 2 NOTES:

### ERROR (on all R-devel instances)

Multiple errors of this flavor:
Error in ``[.data.table`(dt, , `:=`(N, .N), coalition_size)`: attempt access index 4/4 in VECTOR_ELT

This is a known issue with the CRAN version of data.table (v 1.17.8) after the recent R-devel addition
(https://github.com/wch/r-source/commit/4d38d900bca09d2b1bbfd08f2ab28bbbfb1af07e).
This is already fixed in the development version of data.table (https://github.com/Rdatatable/data.table/pull/7485).

I have confirmed that R CMD CHECK passes with no ERRORS, WARNINGS or NOTES locally (Windows 11) with R-devel (2025-12-18 r89199 ucrt) and the dev-version of data.table (1.17.99 as of 2025-12-20).

**We kindly request that CRAN consider this submission despite the R-devel ERRORs, as they are caused by a known incompatibility in data.table v1.17.8 with recent R-devel changes, and not by our package code. The data.table team has already merged the fix, and have confirmed that it will be available in the next CRAN release of data.table.**


### NOTE (multiple devtools-win-builders)
Possibly misspelled words in DESCRIPTION:
  PyPI (10:69)
  Shapley (3:53, 6:15, 7:84, 9:72)

> False positive. Both words are correctly spelled (PyPI is the Python Package Index, Shapley refers to Shapley values).


### NOTE (devtools-win-builder-oldrelease)
Author field differs from that derived from Authors@R
  Author:    'Martin Jullum [cre, aut] (ORCID: <https://orcid.org/0000-0003-3908-5155>), Lars Henry Berge Olsen [aut] (ORCID: <https://orcid.org/0009-0006-9360-6993>), Annabelle Redelmeier [aut], Jon Lachmann [aut] (ORCID: <https://orcid.org/0000-0001-8396-5673>), Nikolai Sellereite [aut] (ORCID: <https://orcid.org/0000-0002-4671-0337>), Anders Løland [ctb], Jens Christian Wahl [ctb], Camilla Lingjærde [ctb], Norsk Regnesentral [cph, fnd]'
  Authors@R: 'Martin Jullum [cre, aut] (<https://orcid.org/0000-0003-3908-5155>), Lars Henry Berge Olsen [aut] (<https://orcid.org/0009-0006-9360-6993>), Annabelle Redelmeier [aut], Jon Lachmann [aut] (<https://orcid.org/0000-0001-8396-5673>), Nikolai Sellereite [aut] (<https://orcid.org/0000-0002-4671-0337>), Anders Løland [ctb], Jens Christian Wahl [ctb], Camilla Lingjærde [ctb], Norsk Regnesentral [cph, fnd]'

> False positive. The only difference is ORCID formatting (parentheses vs. angle brackets). The DESCRIPTION file contains only Authors@R, no Author field.

## Reverse dependencies
Since the package was temporarily archived, we checked reverse dependencies against the Bioconductor package that depends on shapr (XAItest).
No new problems were identified.
