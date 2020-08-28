## Resubmission

This is a resubmission with the following changes:

# Revision round 2

* Revised what is specified as internal/non-internal functions, and added 
  examples such that all non-internal exported functions now have examples.
* I could not find any writing to disk in examples/vignettes/tests as suggested.
  There are scripts under inst/scripts which writes to disk. These scripts are 
  creating objects which are loaded in the tests and therefore handy for 
  package development. Note that we are also using testthat::expect_known_value
  in our tests, this function is overwriting .rds-objects if and only if
  the test fails. Please let me know more precisely what the issue is, if it
  persists after this re-submission.
* Deleted a call to rm(list=ls()) in a script under inst/scripts. I could not
  find any other modifications of .GlobalEnv in the vignette as suggested.

# Revision round 1


* Reduced title length
* Removed examples from unexported functions
* Changed one unexported function to exported and added test (not requested)
* Converted cat-printed console message to message()
* Added Norsk Regnesentral as "author"" (copyrightholder and funder)
* Could not find any packages being installed in functions, examples or 
  vignette as suggested. However, there was a script under inst/devel that 
  installed packages. This script is now deleted.
* Updated cran-comments.md and NEWS.md

## Test environments

* GitHub Actions (macOS-latest): R 4.0.2
* GitHub Actions (ubuntu-16.04): R 4.0.2, 3.6.3, 3.5.3
* win-builder (x86_64-w64-mingw32): R 4.0.2, 3.6.3, R-devel (2020-08-04 r78971)
* local Ubuntu 18.04: R 3.6.3
* local Windows 10: R 4.0.2, R-devel (2020-08-04 r78971)

## R CMD check results

There were no ERRORs or WARNINGs.

There was 2 NOTES:
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Martin Jullum <Martin.Jullum@nr.no>'
New submission

Possibly mis-spelled words in DESCRIPTION:
  Aas (10:16)
  Jullum (10:21)
  L�land (10:32)
  Shapley (4:30, 7:15, 8:84, 11:72)

> This is a new submission and the suggested mis-spellings are all names. 

* checking for future file timestamps ... NOTE
  unable to verify current time

> This is a known issue with the worldclockapi.com currently being down
  (https://stackoverflow.com/questions/63613301/r-cmd-check-note-unable-to-verify-current-time)

## Downstream dependencies
There are currently no downstream dependencies for this package.
