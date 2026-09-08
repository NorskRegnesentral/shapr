# shapr 1.1.0

This is a feature release adding adversarial random forests as a conditional
sampling approach and support for SAGE values. It also includes scalability,
robustness, documentation, and development-workflow improvements (see NEWS.md
for details).

## Test environments

The majority of our tests use snapshots to check result reproducibility.
These snapshot tests are excluded from CRAN-mode checks.
Our test environment includes both CRAN-mode and non-CRAN-mode checks.
Locally, tests are also run both with and without suggested packages.

### Checks with `--as-cran` (snapshot tests excluded)

* Local Ubuntu 24.04.4, R 4.6.1 (release)
* win-builder, R devel, release, and oldrelease
* R-hub Ubuntu 24.04.4, R devel
* R-hub Windows Server 2022, R devel
* R-hub macOS Sequoia 15.7.9, R devel
* R-hub Clang ASAN on Ubuntu 22.04.5, R devel


### Checks without `--as-cran`

These checks include snapshot tests.

* Local Ubuntu 24.04.4, R 4.6.1 (release)
* GHA Ubuntu, R devel, release, oldrel-1, and oldrel-2
* GHA Windows, R release
* GHA macOS, R release

## Current R CMD check results

There were no ERRORs or WARNINGs, and all snapshot tests passed locally and on
GHA. The local `R CMD check --as-cran` produced one NOTE caused by the
compiler-injected `-mno-omit-leaf-frame-pointer` flag on Ubuntu.

The GHA checks produced three environment-specific NOTEs: the Windows release
job downloaded the `torch` runtime while checking dependencies; the Ubuntu
oldrel-2 job reported an installed size of 11.5 MB and could not check Rd
cross-references against the unavailable suggested package `devtools`.

The win-builder and R-hub checks produced no ERRORs, WARNINGs, or NOTEs.

## Reverse dependencies
To be updated with the reverse-dependency check results before submission.
