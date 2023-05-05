
# Patch release, shapr 0.2.2

* Patch to fix failing CRAN-tests on R-devel due to changed behavior of `attach()`: Fixed by changing how we simluate adding a function to .GlobalEnv in the failing test. Actual package not affected.
* Exports also get_model_specs.default as per the following NOTE from initial CRAN submission 'Apparent methods for exported generics not registered: get_model_specs.default'

## Test environments

### With data.table from github master installed, data.table::update_dev_pkg()

* local Windows 10: R-devel (4.4.0)

### With cran version of data.table:

* GitHub Actions (ubuntu-latest), R-version: devel, release, oldrel-1, oldrel-2
* GitHub Actions (windows-latest), R-version: release
* GitHub Actions (macOS-latest), R-version: release
* win-builder, R-version: devel, release 
* R-hub (Fedora Linux): R-version: devel
* R-hub (Windows server 2022): R-version: devel

## R CMD check results

There were no ERRORs or WARNINGs

There were 5 NOTES

### NOTE 1 (on GitHub action, ubuntu-latest):

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
