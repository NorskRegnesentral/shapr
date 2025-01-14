# shapr 1.0.1 (Major release)

* Complete rewrite of the package compared to the previous CRAN release. We moved from two main user functions 
`shapr()` and `explain()` to a single function `explain()` that includes both.
Thus, this change breaks essentially all existing code that uses the previous version of the package.
* A comprehensive list of changes and new functionality can be found in the NEWS.md file.
* We deem the package fully mature at this stage, and therefore release it as a stable, major version.

## Test environments

The majority of our tests rely uses snapshots to check results reproducibility.
These tests are run locally and remotely with GHA (see below) without errors.
The win-builder and R-hub tests are run without snapshots tests (to replicate CRAN testing)

* GHA (ubuntu-latest), R-version: devel, release, oldrel-1, oldrel-2
* GHA (windows-latest), R-version: release
* GHA (macOS-latest), R-version: release
* win-builder, R-versions: devel, release, oldrelease 
* R-hub (ubuntu-latest): R-version: devel
* R-hub (macos-13): R-version: devel
* R-hub (macos-latest): R-version: devel
* R-hub (windows-latest): R-version: devel

## R CMD check results

There were no ERRORs or WARNINGs

There were 2 NOTES

### NOTE 1 (on win-builder (oldrelease)):

Possibly misspelled words in DESCRIPTION:
  shaprpy (10:35)

> This refers to the Python wrapper of the package and is not misspelled.


### NOTE 2 (multiple platforms):

* checking installed package size ... NOTE
  installed size is  8.0Mb
  sub-directories of 1Mb or more:
    doc    4.4Mb
    libs   1.3Mb

> The package is growing in size, uses more complied code, and the documentation is comprehensive.


## Downstream dependencies
There is 2 downstream dependency (`PPtreeregViz`, `SEMdeep`) of `shapr`. 
In addition, the package `shapviz` provides additional plotting functionality without stating an explicit dependency 
(I believe shapr actually should have be listed under Suggests?)

The 3 packages fails on R CMD checks, but I have submitted PRs to fix all issues.

### PPtreeregViz

* Nov 26th 2024 I submitted a PR fixing the issue: https://github.com/sunsmiling/PPtreeregViz/pull/2
There has been no reaction from the maintainer.

### SEMdeep

* Nov 15th 2024 I submitted a PR fixing the issue: https://github.com/BarbaraTarantino/SEMdeep/pull/1
There has been no reaction from the maintainer.

### shapviz

* Nov 15th 2024 I submitted a PR fixing the issue: https://github.com/ModelOriented/shapviz/pull/162
The PR was merged to the GitHub version of `shapviz` on Nov 16th 2024. 
The CRAN version has naturally not been updated yet.
I will notify the maintainer of `shapvix` when the new version of `shapr` is released on CRAN.
