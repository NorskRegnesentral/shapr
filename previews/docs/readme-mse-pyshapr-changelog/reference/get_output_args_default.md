# Get the Default Values for the Output Arguments

Get the Default Values for the Output Arguments

## Usage

``` r
get_output_args_default(
  keep_samp_for_vS = FALSE,
  MSEv_uniform_comb_weights = TRUE,
  saving_path = tempfile("shapr_obj_", fileext = ".rds")
)
```

## Arguments

- keep_samp_for_vS:

  Logical. Indicates whether the samples used in the Monte Carlo
  estimation of `v(S)` should be returned (in `internal$output`). Not
  used for `approach="regression_separate"` or
  `approach="regression_surrogate"`.

- MSEv_uniform_comb_weights:

  Logical. If `TRUE` (default), then the function weights the coalitions
  uniformly when computing the MSEv criterion. If `FALSE`, then the
  function use the Shapley kernel weights to weight the coalitions when
  computing the MSEv criterion. Note that the Shapley kernel weights are
  replaced by the sampling frequency when not all coalitions are
  considered.

- saving_path:

  String. The path to the directory where the results of the iterative
  estimation procedure should be saved. Defaults to a temporary
  directory.

## Value

A list of default output arguments.

## Author

Martin Jullum
