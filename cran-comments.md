# shapr 1.0.2 (Patch release)

* Fix test error in prepare_data_gaussian_cpp seen on clang-UBSAN gcc-UBSAN compliers in CRAN checks. 
Turned out to be an actual bug for an edge case.
* Other minor fixes to GH repo, pkgdown site, python wrapper ++ unrelated to the actual shapr CRAN package.
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

There were in total 2 NOTES

### NOTE (multiple platforms):

* checking installed package size ... NOTE
  installed size is  7.1Mb
  sub-directories of 1Mb or more:
    doc    4.2Mb
    libs   1.3Mb

> The package is growing in size, uses more complied code, and the documentation is comprehensive.

### NOTE (win-builder):

* checking CRAN incoming feasibility ... [20s] NOTE
Maintainer: 'Martin Jullum <Martin.Jullum@nr.no>'

Days since last update: 6

> Correct. I was required to fix the issue by the CRAN team.

## Downstream dependencies
There is 2 downstream dependency (`PPtreeregViz`, `SEMdeep`) of `shapr`. 
In addition, the package `shapviz` provides additional plotting functionality without stating an explicit dependency 
(I believe shapr actually should have be listed under Suggests?)

The 2 packages fails on R CMD checks, but I have submitted PRs to fix them.

### PPtreeregViz

* Nov 26th 2024 I submitted a PR fixing the issue: https://github.com/sunsmiling/PPtreeregViz/pull/2
There has been no reaction from the maintainer.

### SEMdeep

* Nov 15th 2024 I submitted a PR fixing the issue: https://github.com/BarbaraTarantino/SEMdeep/pull/1
There has been no reaction from the maintainer.

### shapviz

*Passes the checks

