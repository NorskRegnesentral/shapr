# shapr Agent Instructions

These instructions apply to the entire repository.

## When to Use
- Adding a new feature, approach, or function to the R package
- Editing or extending the Python `shaprpy` wrapper
- Writing or updating tests
- Writing roxygen2 documentation
- Reviewing a diff for style compliance

## Overview

The `shapr` package has two components:
- **R package** (`R/`, `src/`) — core Shapley value computation
- **Python wrapper** (`python/src/shaprpy/`) — thin `rpy2`-based wrapper around the R package

Both must be **highly human-readable** and **computationally efficient**. Do not sacrifice either.

---

## R Package Conventions

### Naming
- All identifiers use `snake_case` — functions, variables, parameters, file names.
- Approach-specific parameters are prefixed with `approach_name.` (e.g., `gaussian.mu`, `empirical.type`, `ctree.mincriterion`).
- Internal helper objects live inside the `internal` list under `internal$parameters`, `internal$data`, `internal$objects`, `internal$iter_list`.
- S3 generic + method pairs follow `generic.approach` (e.g., `setup_approach.gaussian`, `prepare_data.copula`).

### Assignment and Spacing
- Use `<-` for assignment, never `=` (except in function argument defaults and named list elements).
- 2-space indentation, no tabs.
- Spaces around operators and after commas.
- No trailing whitespace.

### Function Structure
1. Extract all needed values from `internal$*` at the **top** of the function body before any logic. This keeps the logic clean and reduces `internal$...` nesting throughout the body.
2. Use explicit `return()` at the end of every function.
3. Keep functions focused; delegate sub-tasks to clearly named helpers.

```r
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
- Always qualify external package calls with `::` (e.g., `cli::cli_abort()`, `data.table::setkey()`, `future.apply::future_lapply()`).
- Never use `library()` or `require()` inside package code.

### Data Manipulation — `data.table` only
- Use `data.table` for **all** tabular data operations. Never use `dplyr`, `tidyr`, or the pipe `|>` / `%>%` in package code.
- Prefer in-place modification (`set*` functions, `:=`) over copying where performance matters.
- Use `.N`, `setkey()`, `setnames()`, `rbind(..., use.names = FALSE)` idioms.
- Column selection: use `dt[, ..col_vec]` notation for variable column names.

```r
# Good
setkey(dt, id_coalition)
dt[, value := x * weight]

# Bad — do not use dplyr
dt %>% mutate(value = x * weight)
```

### Error and Warning Handling
- Use `cli::cli_abort(...)` for errors (never `stop()`).
- Use `cli::cli_warn(...)` for warnings (never `warning()`).
- Use `cli::cli_inform(...)` / `cli::cli_text(...)` for messages (never `message()`).
- Structure multi-part messages as named character vectors per `cli` conventions:
  ```r
  cli::cli_abort(c(
    "The argument {.arg foo} is invalid.",
    "i" = "Expected a positive integer, got {.val {foo}}."
  ))
  ```

### Verbosity / Progress
- All user-facing output is gated on `verbose` (passed through `internal$parameters$verbose`).
- Use `cli::cli_h1/h2/h3` for section headers, `cli::cli_ul()` for bullet lists, `cli::cli_progress_step()` for progress.
- Never print directly; always check `"basic" %in% verbose`, `"progress" %in% verbose`, etc.

### S3 Dispatch Pattern
New approaches must implement these S3 methods:
- `setup_approach.<approach_name>(internal, ...)` — stores approach parameters into `internal$parameters`.
- `prepare_data.<approach_name>(internal, index_features, ...)` — returns a `data.table` with MC samples.

Both must call `insert_defaults(internal, mget(c("approach.param1", ...)))` to store defaults.

```r
setup_approach.myapproach <- function(internal,
                                      myapproach.param1 = default_val, ...) {
  defaults <- mget(c("myapproach.param1"))
  internal <- insert_defaults(internal, defaults)

  # validation ...

  return(internal)
}
```

### Performance
- Move computationally intensive inner loops to **Rcpp** (`src/`). The R function is a thin wrapper.
- Use `future.apply::future_lapply()` for parallelisable batch operations (see `compute_vS.R`).
- Avoid redundant copies of large data objects; operate in-place where possible.
- Advance the RNG with `rnorm(1)` before sequential loops that mirror `future_lapply` (for reproducibility consistency — see `compute_vS.R`).

### Documentation (Roxygen2)
- Every exported function must have a full roxygen2 block.
- Internal-only functions get `#' @keywords internal` (and `@export` only if technically needed for S3 dispatch).
- Reuse parameter docs with `@inheritParams` pointing to `explain`, `default_doc_internal`, or `default_doc_export`.
- Add `@author First Last` when the contributor is not the package default author.
- Use `[function_name()]` for cross-references and `\href{url}{text}` for external links.
- Document all approach-specific parameters in the `@rdname setup_approach` block.

```r
#' @rdname setup_approach
#'
#' @param myapproach.param1 Numeric. Description of param1. `NULL` means ...
#'
#' @inheritParams default_doc_export
#' @export
setup_approach.myapproach <- function(internal, myapproach.param1 = NULL, ...) {
```

### Formatting and Linting
The project enforces the **tidyverse style guide** via `styler` and checks compliance with `lintr`.

- Run `styler::style_pkg()` before submitting any R code changes. A GitHub Actions workflow also runs this automatically on PRs.
- Run `lintr::lint_package()` and resolve all reported issues before submitting.
- The `.lintr` config sets a **120-character line limit** and disables `object_name_linter`, `object_usage_linter`, `commented_code_linter`, `indentation_linter`, and `return_linter`.
- Only restyle code that is directly related to your change — do not reformat unrelated lines.
- The following paths are excluded from linting: `inst/devel`, `inst/scripts`, `inst/code_paper`, `inst/demo`, `vignettes`, `R/RcppExports.R`, `R/zzz.R`.

### Comments
- Use `#` for inline and block comments; use `####` dividers for major sections within a function.
- Comments explain *why*, not *what* — the code should be self-explanatory for the *what*.
- Mark non-obvious performance choices with a brief justification comment.

### Tests
- All tests use `testthat` with `expect_snapshot_rds()` for output regression.
- Each test file targets one area: separate `test-<area>-setup.R` and `test-<area>-output.R`.
- Place shared data and model setup in `helper-*.R` files (auto-loaded by testthat).
- Start every test file with `skip_on_cran()`.
- Set `testing = TRUE` in `explain()` calls inside tests to strip timing and random output.
- Use `set.seed()` in helper files, not inside individual tests.

---

## Python Wrapper (`shaprpy`) Conventions

### Naming
- `snake_case` for all identifiers (functions, variables, arguments).
- Mirror R argument names exactly where possible to keep the two APIs consistent.

### Type Hints
- All function signatures must have full type hints.
- Use `from __future__ import annotations` at the top of each file.
- Use `X | Y` union syntax (not `Union[X, Y]`).

### Docstrings
- Use **NumPy-style** docstrings (Parameters / Returns / Raises sections).
- Every public function and class must have a docstring.
- Mirror the language of the R documentation where applicable.

### Imports
- Group imports: stdlib → third-party → local, separated by blank lines.
- Use explicit imports; avoid `from module import *`.

### R Bridging (`rpy2`)
- Use `_importr()` from `shaprpy._rutils` (not bare `rpy2.robjects.packages.importr`) — it handles `lib_loc` correctly.
- Use `py2r()` / `r2py()` from `shaprpy.utils` for type conversion.
- Convert `None` → R `NULL` with `maybe_null()`.
- Keep all `rpy2` calls inside `_explain.py` or utility modules; higher-level code must not import `rpy2` directly.

### Classes
- Return structured objects (`Shapr`, `ShaprSummary`) — never raw dicts — from public API functions.
- Implement `__str__` and `__repr__` to mirror R's print output.
- Prefix private attributes with `_`.

### Error Handling
- Raise `ValueError` for invalid arguments, `TypeError` for type mismatches.
- Provide informative messages that match the R error text where possible.

---

## Checklist Before Submitting Changes

- [ ] All new R functions use `snake_case` naming and `<-` assignment.
- [ ] `styler::style_pkg()` run on changed files; only changed lines restyled.
- [ ] `lintr::lint_package()` passes with no new issues.
- [ ] External calls qualified with `::` (e.g., `cli::cli_abort`, `data.table::setkey`).
- [ ] Data manipulation uses `data.table` only — no `dplyr`/`tidyr`/pipes.
- [ ] Errors use `cli::cli_abort()`, warnings use `cli::cli_warn()`.
- [ ] New approach implements both `setup_approach.<name>` and `prepare_data.<name>`.
- [ ] Approach parameters stored via `insert_defaults(internal, mget(...))`.
- [ ] All exported functions have roxygen2 docs with `@inheritParams` where applicable.
- [ ] Internal functions have `@keywords internal`.
- [ ] Computationally heavy loops offloaded to Rcpp or `future.apply`.
- [ ] New test file follows `test-<area>-setup.R` / `test-<area>-output.R` pattern.
- [ ] Tests use `expect_snapshot_rds()` and `testing = TRUE`.
- [ ] Python code has full type hints and NumPy-style docstrings.
- [ ] Python R-bridging goes through `_rutils._importr` and `utils.py2r`/`r2py`.
- [ ] `NEWS.md` updated with a brief entry under the current development version describing the change.
