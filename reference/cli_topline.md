# Create a header topline with cli

Create a header topline with cli

## Usage

``` r
cli_topline(verbose, testing, init_time, type, is_python)
```

## Arguments

- verbose:

  String vector or NULL. Controls verbosity (printout detail level) via
  one or more of `"basic"`, `"progress"`, `"convergence"`, `"shapley"`
  and `"vS_details"`. `"basic"` (default) displays basic information
  about the computation and messages about parameters/checks.
  `"progress"` displays where in the calculation process the function
  currently is. `"convergence"` displays how close the Shapley value
  estimates are to convergence (only when `iterative = TRUE`).
  `"shapley"` displays intermediate Shapley value estimates and standard
  deviations (only when `iterative = TRUE`), and the final estimates.
  `"vS_details"` displays information about the v(S) estimates, most
  relevant for
  `approach %in% c("regression_separate", "regression_surrogate", "vaeac")`.
  `NULL` means no printout. Any combination can be used, e.g.,
  `verbose = c("basic", "vS_details")`.

- testing:

  Logical. Only used to remove random components, like timing, from the
  output when comparing with testthat. Defaults to `FALSE`.

- init_time:

  POSIXct. The time when the
  [`explain()`](https://norskregnesentral.github.io/shapr/reference/explain.md)
  function was called, as returned by
  [`Sys.time()`](https://rdrr.io/r/base/Sys.time.html). Used to
  calculate the total time of the
  [`explain()`](https://norskregnesentral.github.io/shapr/reference/explain.md)
  call.

- type:

  Character. Either "regular" or "forecast", matching the function the
  call originated from, and thus the type of explanation to generate.

- is_python:

  Logical. Indicates whether the function is called from the Python
  wrapper. Default is FALSE, which is never changed when calling the
  function via
  [`explain()`](https://norskregnesentral.github.io/shapr/reference/explain.md)
  in R. The parameter is later used to disallow running the AICc
  versions of the empirical method, as that requires data-based
  optimization, which is not supported in `shaprpy`.

## Value

No return value (but prints header with cli unless `verbose` is `NULL`)
