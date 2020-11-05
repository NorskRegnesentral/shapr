
# Patch for shapr 0.1.2

* Fixes the installation error on Solaris for version 0.1.2 as described in
https://cran.r-project.org/web/checks/check_results_shapr.html. Checks using
rhub::check_on_solaris() indicates the issue is fixed in this release.

## Test environments

* GitHub Actions (macOS-latest): R 4.0.2
* GitHub Actions (windows-latest): R 4.0.2
* GitHub Actions (ubuntu-16.04): R 4.0.2, 3.6.3, 3.5.3
* win-builder (x86_64-w64-mingw32): R 4.0.2, 3.6.3, R-devel (2020-08-23 r79071)
* local Ubuntu 18.04: R 3.6.3
* local Windows 10: R 4.0.2, R-devel (2020-08-04 r78971)

## R CMD check results

There were no ERRORs or WARNINGs.

There was 2 NOTES:

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Martin Jullum <Martin.Jullum@nr.no>'

Days since last update: 0

> This is a patch for the current installation error on Solaris

* checking for future file timestamps ... NOTE
  unable to verify current time

> This is a known issue with the worldclockapi.com currently being down
  (https://stackoverflow.com/questions/63613301/r-cmd-check-note-unable-to-verify-current-time)

## Downstream dependencies
There are currently no downstream dependencies for this package.
