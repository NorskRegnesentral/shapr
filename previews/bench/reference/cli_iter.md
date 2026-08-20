# Print Messages in Iterative Procedure with CLI

Print Messages in Iterative Procedure with CLI

## Usage

``` r
cli_iter(verbose, internal, iter)
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

- internal:

  List. Not used directly, but passed through from
  [`explain()`](https://norskregnesentral.github.io/shapr/reference/explain.md).

- iter:

  Integer. The iteration number. Only used internally.

## Value

No return value (but prints iterative messages with cli)
