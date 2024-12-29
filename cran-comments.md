# shapr 1.0.1 

* Complete rewrite of the package compared to the previous CRAN release. We moved from two main user functions 
`shapr()` and `explain()` to a single function `explain()` that includes both.
Thus, this change breaks essentially all existing code that uses the previous version of the package.
* A comprehensive list of changes and new functionality can be found in the NEWS.md file.
* We deem the package fully mature at this stage, and therefore release it as a 1.0.0 version.


## Test environments

The majority of our tests rely uses snapshots. These tests are run locally and with GHA. 
The win-builder and R-hub tests are run without snapshots tests (to replicate CRAN testing)

* GitHub Actions (ubuntu-latest), R-version: devel, release, oldrel-1, oldrel-2
* GitHub Actions (windows-latest), R-version: release
* GitHub Actions (macOS-latest), R-version: release
* win-builder, R-versions: devel, release, oldrelease 
* R-hub (Fedora Linux): R-version: devel
* R-hub (Windows server 2022): R-version: devel

## R CMD check results

There were no ERRORs or WARNINGs

There were X NOTES

### NOTE 1 (on win-builder (oldrelease)):

Possibly misspelled words in DESCRIPTION:
  shaprpy (10:35)

> This refers to the Python wrapper of the package and is not misspelled.

## Downstream dependencies
There is 2 downstream dependency (PPtreeregViz, SEMdeep) of shapr. In additon, the package shapviz provides additional
plotting functionality without stating an explicit dependency (I believe shapr actually should have be listed under 
Suggests?)
All packages fails on R CMD checks, but I have submitted PRs to fix the issues.

### PPtreeregViz

* Nov 26th 2024 I submitted a PR fixing the issue: https://github.com/sunsmiling/PPtreeregViz/pull/2
There has been no reaction from the maintainer

### SEMdeep

* Nov 15th 2024 I submitted a PR fixing the issue: https://github.com/BarbaraTarantino/SEMdeep/pull/1
There has been no reaction from the maintainer.

### shapviz

* Nov 15th 2024 I submitted a PR fixing the issue: https://github.com/ModelOriented/shapviz/pull/162
The PR was merged to the GitHub version on Nov 16th 2024. The CRAN version has naturally not been updated yet.
I will notify the maintainer when the new version of shapr is released on CRAN.




################### TO BE DELETED IN THE END ###############

* checking installed package size ... NOTE
  installed size is  5.0Mb
  sub-directories of 1Mb or more:
    libs   4.0Mb

> Nothing has changed since the last submission.



### NOTE 2 (on R-hub (Fedora Linux)):

* checking HTML version of manual ... NOTE
Skipping checking HTML validation: no command 'tidy' found
Skipping checking math rendering: package 'V8' unavailable

> Missing packages on R-hubs Windows Server 2022 platform.

### NOTE 4 (on R-hub (Fedora Linux, Windows Server 2022)):

*Found the following (possibly) invalid URLs:
  URL: https://opensource.org/license/mit/
    From: README.md
    Status: 403
    Message: Forbidden

> I believe this is a false positive. Running 'urlchecker::url_check()' locally shows all URLs are correct.

### NOTE 5 (on R-hub (Windows Server 2022)):

* checking for detritus in the temp directory ... NOTE
Found the following files/directories:
  'lastMiKTeXException'

> As noted in [R-hub issue #503](https://github.com/r-hub/rhub/issues/503), this could be due to a bug/crash in MiKTeX and can likely be ignored.

## Downstream dependencies
There is 1 downstream dependency (PPtreeregViz) of shapr
I have also run R CMD check on that and it passed without errors, warnings or notes.


