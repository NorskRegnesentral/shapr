# shapr Agent Instructions

These instructions apply to the entire repository.

## Pre-PR Workflow

When asked to prepare a PR, check PR readiness, push work, or
create/update a PR, follow `dev/pr-workflow.md`. Use `dev/prepare-pr`
for readiness edits that may modify files, `dev/check-pr` for read-only
checks and a chat report, and `dev/publish-pr` only after explicit
approval to push or create/update a GitHub PR. **Always prompt the user
to confirm before pushing commits or creating/updating a PR** — never do
either automatically, even if the overall workflow was already approved.

## When to Use

- Adding a new feature, approach, or function to the R package
- Editing or extending the Python `shaprpy` wrapper
- Writing or updating tests
- Writing roxygen2 documentation
- Reviewing a diff for style compliance

## Overview

The `shapr` package has two components: - **R package** (`R/`, `src/`) —
core Shapley value computation - **Python wrapper**
(`python/src/shaprpy/`) — thin `rpy2`-based wrapper around the R package

Both must be **highly human-readable** and **computationally
efficient**. Do not sacrifice either.

------------------------------------------------------------------------

## R Package Conventions

### Naming

- All identifiers use `snake_case` — functions, variables, parameters,
  file names.
- Approach-specific parameters are prefixed with `approach_name.` (e.g.,
  `gaussian.mu`, `empirical.type`, `ctree.mincriterion`).
- Internal helper objects live inside the `internal` list under
  `internal$parameters`, `internal$data`, `internal$objects`,
  `internal$iter_list`.
- S3 generic + method pairs follow `generic.approach` (e.g.,
  `setup_approach.gaussian`, `prepare_data.copula`).

### Assignment and Spacing

- Use `<-` for assignment, never `=` (except in function argument
  defaults and named list elements).
- 2-space indentation, no tabs.
- Spaces around operators and after commas.
- No trailing whitespace.

### Function Structure

1.  Extract all needed values from `internal$*` at the **top** of the
    function body before any logic. This keeps the logic clean and
    reduces `internal$...` nesting throughout the body.
2.  Use explicit [`return()`](https://rdrr.io/r/base/function.html) at
    the end of every function.
3.  Keep functions focused; delegate sub-tasks to clearly named helpers.

``` r

# Good pattern — extract first, then use
my_function <- function(internal, ...) {
  n_explain    <- internal$parameters$n_explain
  n_features   <- internal$parameters$n_features
  x_train      <- internal$data$x_train

  # ... logic using extracted locals ...

  return(internal)
}
```

### Namespace Qualification

- Always qualify external package calls with `::` (e.g.,
  [`cli::cli_abort()`](https://cli.r-lib.org/reference/cli_abort.html),
  [`data.table::setkey()`](https://rdrr.io/pkg/data.table/man/setkey.html),
  [`future.apply::future_lapply()`](https://future.apply.futureverse.org/reference/future_lapply.html)).
- Never use [`library()`](https://rdrr.io/r/base/library.html) or
  [`require()`](https://rdrr.io/r/base/library.html) inside package
  code.

### Data Manipulation — `data.table` only

- Use `data.table` for **all** tabular data operations. Never use
  `dplyr`, `tidyr`, or the pipe `|>` / `%>%` in package code.
- Prefer in-place modification (`set*` functions, `:=`) over copying
  where performance matters.
- Use `.N`, `setkey()`, `setnames()`, `rbind(..., use.names = FALSE)`
  idioms.
- Column selection: use `dt[, ..col_vec]` notation for variable column
  names.

``` r

# Good
setkey(dt, id_coalition)
dt[, value := x * weight]

# Bad — do not use dplyr
dt %>% mutate(value = x * weight)
```

### Error and Warning Handling

- Use `cli::cli_abort(...)` for errors (never
  [`stop()`](https://rdrr.io/r/base/stop.html)).

- Use `cli::cli_warn(...)` for warnings (never
  [`warning()`](https://rdrr.io/r/base/warning.html)).

- Use `cli::cli_inform(...)` / `cli::cli_text(...)` for messages (never
  [`message()`](https://rdrr.io/r/base/message.html)).

- Structure multi-part messages as named character vectors per `cli`
  conventions:

  ``` r

  cli::cli_abort(c(
    "The argument {.arg foo} is invalid.",
    "i" = "Expected a positive integer, got {.val {foo}}."
  ))
  ```

### Verbosity / Progress

- All user-facing output is gated on `verbose` (passed through
  `internal$parameters$verbose`).
- Use `cli::cli_h1/h2/h3` for section headers,
  [`cli::cli_ul()`](https://cli.r-lib.org/reference/cli_ul.html) for
  bullet lists,
  [`cli::cli_progress_step()`](https://cli.r-lib.org/reference/cli_progress_step.html)
  for progress.
- Never print directly; always check `"basic" %in% verbose`,
  `"progress" %in% verbose`, etc.

### S3 Dispatch Pattern

New approaches must implement these S3 methods: -
`setup_approach.<approach_name>(internal, ...)` — stores approach
parameters into `internal$parameters`. -
`prepare_data.<approach_name>(internal, index_features, ...)` — returns
a `data.table` with MC samples.

Both must call
`insert_defaults(internal, mget(c("approach.param1", ...)))` to store
defaults.

``` r

setup_approach.myapproach <- function(internal,
                                      myapproach.param1 = default_val, ...) {
  defaults <- mget(c("myapproach.param1"))
  internal <- insert_defaults(internal, defaults)

  # validation ...

  return(internal)
}
```

### Performance

- Move computationally intensive inner loops to **Rcpp** (`src/`). The R
  function is a thin wrapper.
- Use
  [`future.apply::future_lapply()`](https://future.apply.futureverse.org/reference/future_lapply.html)
  for parallelisable batch operations (see `compute_vS.R`).
- Avoid redundant copies of large data objects; operate in-place where
  possible.
- Advance the RNG with `rnorm(1)` before sequential loops that mirror
  `future_lapply` (for reproducibility consistency — see
  `compute_vS.R`).

### Documentation (Roxygen2)

- Every exported function must have a full roxygen2 block.
- Internal-only functions get `#' @keywords internal` (and `@export`
  only if technically needed for S3 dispatch).
- Reuse parameter docs with `@inheritParams` pointing to `explain`,
  `default_doc_internal`, or `default_doc_export`.
- Add `@author First Last` when the contributor is not the package
  default author.
- Use `[function_name()]` for cross-references and `\href{url}{text}`
  for external links.
- Document all approach-specific parameters in the
  `@rdname setup_approach` block.

``` r
#' @rdname setup_approach
#'
#' @param myapproach.param1 Numeric. Description of param1. `NULL` means ...
#'
#' @inheritParams default_doc_export
#' @export
setup_approach.myapproach <- function(internal, myapproach.param1 = NULL, ...) {
```

### Formatting and Linting

The project enforces the **tidyverse style guide** via `styler` and
checks compliance with `lintr`.

- Run `styler::style_pkg()` before submitting any R code changes. A
  GitHub Actions workflow also runs this automatically on PRs.
- Run `lintr::lint_package()` and resolve all reported issues before
  submitting.
- The `.lintr` config sets a **120-character line limit** and disables
  `object_name_linter`, `object_usage_linter`, `commented_code_linter`,
  `indentation_linter`, and `return_linter`.
- Only restyle code that is directly related to your change — do not
  reformat unrelated lines.
- The following paths are excluded from linting: `dev`, `inst/devel`,
  `inst/scripts`, `inst/code_paper`, `inst/demo`, `vignettes`,
  `R/RcppExports.R`, `R/zzz.R`.

### Comments

- Use `#` for inline and block comments; use `####` dividers for major
  sections within a function.
- Comments explain *why*, not *what* — the code should be
  self-explanatory for the *what*.
- Mark non-obvious performance choices with a brief justification
  comment.

### Tests

- All tests use `testthat` with `expect_snapshot_rds()` for output
  regression.
- Each test file targets one area: separate `test-<area>-setup.R` and
  `test-<area>-output.R`.
- Place shared data and model setup in `helper-*.R` files (auto-loaded
  by testthat).
- Start every test file with `skip_on_cran()`.
- Set `testing = TRUE` in
  [`explain()`](https://norskregnesentral.github.io/shapr/reference/explain.md)
  calls inside tests to strip timing and random output.
- Use [`set.seed()`](https://rdrr.io/r/base/Random.html) in helper
  files, not inside individual tests.

### Local R Test Workflow

Local snapshot testing should use the repository’s development scripts
instead of `devtools::test()`. This is intentional: under local
`devtools::test()` /
[`pkgload::load_all()`](https://pkgload.r-lib.org/reference/load_all.html)
runs, `future` can emit serialization warnings such as
`'package:shapr' may not be available when loading`, and
`expect_snapshot()` records those warnings in the `.md` snapshots.
GitHub Actions does not have the same issue, so do not change package
defaults or the snapshot expectations just to silence this local-only
warning.

Use the VS Code tasks in `.vscode/tasks.json`: -
`R: Run all tests (snapshot-safe)` runs file-by-file tests via
`Rscript dev/snapshot-diff.R --run-tests` and is the default test
task. - `R: Run current test file (snapshot-safe)` runs the active test
file through
[`testthat::test_file()`](https://testthat.r-lib.org/reference/test_file.html). -
`R: Run and review snapshots (snapshot-safe)` runs the file-by-file
tests and then opens the `.md` and `.rds` review helpers. -
`R: Review MD snapshots` reviews only changed `.md` snapshots with
[`testthat::snapshot_review()`](https://testthat.r-lib.org/reference/snapshot_accept.html). -
`R: Review RDS snapshots` reviews only changed `.rds` snapshots with the
manual `waldo`-based workflow. -
`R: Inspect MD snapshot diffs (agent helper)` and
`R: Inspect RDS snapshot diffs (agent helper)` are read-only terminal
diff helpers intended mainly for agents investigating snapshot changes,
not the normal developer review flow. -
`R: Rebuild long-running vignettes` rebuilds precomputed vignettes from
`vignettes/*.Rmd.orig` and converts generated PNG figures to WebP. Run
it only when vignette source changes require it.

Equivalent terminal commands from the repository root:

``` sh
Rscript dev/snapshot-diff.R --run-tests
Rscript dev/snapshot-diff.R --review-md
Rscript dev/snapshot-diff.R --review-rds
Rscript dev/snapshot-diff.R --run-tests --review-md --review-rds
Rscript dev/snapshot-diff.R --run-tests --file tests/testthat/test-forecast-output.R
```

Snapshot review is intentionally split by file extension. Changed `.md`
snapshots use
[`testthat::snapshot_review()`](https://testthat.r-lib.org/reference/snapshot_accept.html);
changed `.rds` snapshots use the manual `waldo` comparison flow copied
from `snapshot_review_man()` in `.Rprofile`. Do not use a review command
that mixes `.md` and `.rds` files when the goal is to inspect only one
snapshot type. The `Inspect ... snapshot diffs (agent helper)` tasks
never accept snapshots; they only print differences to help agents
understand what changed before reporting back or choosing the correct
review task.

The developer’s original all-in-one script remains available for manual
use:

``` sh
Rscript inst/devel/test.R
```

The `devtools::test()` task remains available for checks that
specifically need to mirror that command, but it is not the preferred
local workflow for snapshot updates in this repository.

------------------------------------------------------------------------

## Python Wrapper (`shaprpy`) Conventions

### Naming

- `snake_case` for all identifiers (functions, variables, arguments).
- Mirror R argument names exactly where possible to keep the two APIs
  consistent.

### Type Hints

- All function signatures must have full type hints.
- Use `from __future__ import annotations` at the top of each file.
- Use `X | Y` union syntax (not `Union[X, Y]`).

### Docstrings

- Use **NumPy-style** docstrings (Parameters / Returns / Raises
  sections).
- Every public function and class must have a docstring.
- Mirror the language of the R documentation where applicable.

### Imports

- Group imports: stdlib → third-party → local, separated by blank lines.
- Use explicit imports; avoid `from module import *`.

### R Bridging (`rpy2`)

- Use `_importr()` from `shaprpy._rutils` (not bare
  `rpy2.robjects.packages.importr`) — it handles `lib_loc` correctly.
- Use `py2r()` / `r2py()` from `shaprpy.utils` for type conversion.
- Convert `None` → R `NULL` with `maybe_null()`.
- Keep all `rpy2` calls inside `_explain.py` or utility modules;
  higher-level code must not import `rpy2` directly.

### Classes

- Return structured objects (`Shapr`, `ShaprSummary`) — never raw dicts
  — from public API functions.
- Implement `__str__` and `__repr__` to mirror R’s print output.
- Prefix private attributes with `_`.

### Error Handling

- Raise `ValueError` for invalid arguments, `TypeError` for type
  mismatches.
- Provide informative messages that match the R error text where
  possible.

------------------------------------------------------------------------

## PR Readiness Checklist

Use this checklist with `dev/pr-workflow.md` before publishing a PR.
Apply only the sections relevant to the changed files.

### R Code

New or changed R identifiers use `snake_case` naming and `<-`
assignment.

External calls are qualified with `::` (e.g.,
[`cli::cli_abort`](https://cli.r-lib.org/reference/cli_abort.html),
[`data.table::setkey`](https://rdrr.io/pkg/data.table/man/setkey.html)).

Data manipulation uses `data.table` only — no `dplyr`, `tidyr`, `%>%`,
or `|>`.

Errors use
[`cli::cli_abort()`](https://cli.r-lib.org/reference/cli_abort.html),
warnings use
[`cli::cli_warn()`](https://cli.r-lib.org/reference/cli_abort.html), and
messages use
[`cli::cli_inform()`](https://cli.r-lib.org/reference/cli_abort.html) or
[`cli::cli_text()`](https://cli.r-lib.org/reference/cli_text.html).

New approach code implements both `setup_approach.<name>` and
`prepare_data.<name>`.

Approach parameters are stored via
`insert_defaults(internal, mget(...))`.

Computationally heavy loops are offloaded to Rcpp or `future.apply`
where appropriate.

### R Formatting, Documentation, And Tests

`styler::style_pkg()` or targeted `styler` formatting has run on changed
R files.

`lintr::lint_package()` passes with no new issues.

Exported functions have roxygen2 docs with `@inheritParams` where
applicable.

Internal documented functions have `@keywords internal`.

New test files follow the `test-<area>-setup.R` / `test-<area>-output.R`
pattern.

Tests use `expect_snapshot_rds()` and `testing = TRUE` where applicable.

### Python Wrapper

Python code has full type hints and NumPy-style docstrings.

Imports are explicit and grouped stdlib, third-party, local.

Python R-bridging goes through `_rutils._importr` and `utils.py2r` /
`r2py`.

Public API functions return structured objects, not raw dictionaries.

### Changelog And Versioning

`NEWS.md` has a brief entry under the current development version for
R-facing or developer-facing changes. Use `(branch: <branch-name>)` as a
placeholder at the end of each bullet when the PR number is not yet
known; replace with `([#NNN](url))` during the Publish PR step.

`python/CHANGELOG.md` has an entry for Python-facing changes.

`DESCRIPTION` version is incremented only when preparing package changes
for merge/release or when explicitly requested.

`python/pyproject.toml` version is incremented only when preparing a
Python package release or when explicitly requested.
