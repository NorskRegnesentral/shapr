
# Minor release, shapr 0.2.0

* Adds the ctree approach from new paper
* Simplified supported for custom models
* Adds comprehensive check suite for feature consistency

## Test environments

* GitHub Actions (windows-latest): R 4.0
* GitHub Actions (ubuntu-16.04): R 4.0, 3.6, 3.5
* win-builder (x86_64-w64-mingw32): R 4.0, 3.6, R-devel
* local Ubuntu 18.04: R 3.6
* local Windows 10: R 4.0
* R-hub (windows-x86_64-devel): R-devel
* R-hub (ubuntu-gcc-release): R-release
* R-hub (fedora-gcc-devel): R-devel
* R-hub (macos-highsierra-release-cran): R-release

## R CMD check results

There were no ERRORs or WARNINGs.

There was 1 NOTE:

* checking CRAN incoming feasibility ... NOTE
Maintainer: ‘Martin Jullum <Martin.Jullum@nr.no>’

New submission

Package was archived on CRAN

Possibly mis-spelled words in DESCRIPTION:
  Aas (9:16)
  Jullum (9:21)
  Løland (9:32)
  Shapley (3:53, 6:15, 7:84, 10:72)

CRAN repository db overrides:
  X-CRAN-Comment: Archived on 2021-01-20 as check problems were not
    corrected in time.

> This is a patch for the version that was taken off CRAN.


## Downstream dependencies
There are currently no downstream dependencies for this package.
