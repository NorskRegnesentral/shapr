# shapr 1.1.0

This is a feature release adding adversarial random forests as a conditional
sampling approach and support for SAGE values. It also includes scalability,
robustness, documentation, and development-workflow improvements (see NEWS.md
for details).

## Test environments

The majority of our tests use snapshots to check result reproducibility.
Snapshot tests are excluded when checks run on CRAN.
Our test environments therefore include checks both with and without `--as-cran`,
as well as depends-only and sanitizer checks.

### Checks with `--as-cran`

Snapshot tests are excluded and suggested packages are included.

* Local Ubuntu 24.04.5, R 4.6.1 (release)
* win-builder, R devel, release, and oldrelease
* R-hub Ubuntu 24.04.4, R devel
* R-hub Windows Server 2022, R devel
* R-hub macOS Sequoia 15.7.9, R devel

### Checks without `--as-cran`

Snapshot tests and suggested packages are included.

* Local Ubuntu 24.04.5, R 4.6.1 (release)
* GHA Ubuntu, R devel, release, oldrel-1, and oldrel-2
* GHA Windows, R release
* GHA macOS, R release

### Additional checks

Both checks excluded snapshot tests without explicitly passing `--as-cran`.

* Local Ubuntu 24.04.5, R 4.6.1 (release): depends-only check without suggested packages
* R-hub Clang ASAN on Ubuntu 22.04.5, R devel: CRAN-like check with suggested packages

## Current R CMD check results

There were no ERRORs, WARNINGs or NOTEs, and all snapshot tests passed locally and on
GHA.

## Reverse dependencies

We checked all 5 reverse dependencies (`futurize`, `MLwrap`, `nestedcv`, and
`rtemis` from CRAN; `XAItest` from Bioconductor), comparing the CRAN release
(1.0.8) with this release (1.1.0).

* We saw 0 new problems
* We failed to check 0 packages

We also repeated the check for `nestedcv` after installing its unavailable
suggested package `CORElearn`, and the check for `XAItest` after installing
packages used by its check-time code but absent from its package metadata.
These supplemental checks found no new problems.
