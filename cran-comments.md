
# Patch release, shapr 0.2.1

* fix warning from development version of data.table due to the use of nomatch argument in merge()


## Test environments

### With data.table from github master installed, data.table::update_dev_pkg()

* local Ubuntu 20.04: R 4.1
* local Windows 10: R 4.2

### With cran version of data.table:

* GitHub Actions (ubuntu-latest), R-version: devel, release, oldrel-1, oldrel-2
* GitHub Actions (windows-latest), R-version: release
* GitHub Actions (macOS-latest), R-version: release
* win-builder, R-version: devel, release
* R-hub (Fedora Linux): R-version: devel
* R-hub (Windows server 2022): R-version: devel

## R CMD check results

There were no ERRORs, WARNINGs or NOTES

There was 6 NOTES 

### NOTE 1 (all R-devel instances):

* checking C++ specification ... NOTE
  Specified C++11: please drop specification unless essential

> I believe this is something specific to R-devel. Nothing was changed in the C++ code, and seems like a common note these days.

### NOTE 2 (on GitHub action, ubuntu-latest):

* checking installed package size ... NOTE
  installed size is  5.1Mb
  sub-directories of 1Mb or more:
    libs   4.1Mb

> Nothing has changed since the last submission.

### NOTE 3 (on R-hub (Fedora Linux)):

* checking HTML version of manual ... NOTE
Skipping checking HTML validation: no command 'tidy' found
Skipping checking math rendering: package 'V8' unavailable

> Missing packages on R-hubs Fedora Linux platform.

### NOTE 4 (on R-hub (Fedora Linux)):

* checking examples ... [11s/50s] NOTE
Examples with CPU (user + system) or elapsed time > 5s
              user system elapsed
explain      3.339  0.100  15.364
create_ctree 2.160  0.103  11.701

> Nothing has changed since the last submission. Probably slow CPU speed due to heavy load on R-hub.

### NOTE 5 (on R-hub (Fedora Linux)):

*Found the following (possibly) invalid URLs:
  URL: https://opensource.org/license/mit/
    From: README.md
    Status: 403
    Message: Forbidden

> I believe this is a false positive. Running 'urlchecker::url_check()' locally shows all URLs are correct.

### NOTE 6 (on R-hub (Windows Server 2022)):

* checking for detritus in the temp directory ... NOTE
Found the following files/directories:
  'lastMiKTeXException'

> As noted in [R-hub issue #503](https://github.com/r-hub/rhub/issues/503), this could be due to a bug/crash in MiKTeX and can likely be ignored.

## Downstream dependencies
There is 1 downstream dependency (PPtreeregViz) of shapr
I have also run R CMD check on that and it passed with errors, warnings or notes.
