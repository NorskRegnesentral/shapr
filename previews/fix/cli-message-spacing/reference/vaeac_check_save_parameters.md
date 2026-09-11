# Function that gives a warning about disk usage

Function that gives a warning about disk usage

## Usage

``` r
vaeac_check_save_parameters(
  save_data,
  epochs,
  save_every_nth_epoch,
  x_train_size,
  verbose
)
```

## Arguments

- save_data:

  Logical (default is `FALSE`). If `TRUE`, then the data is stored
  together with the model. Useful if one are to continue to train the
  model later using
  [`vaeac_train_model_continue()`](https://norskregnesentral.github.io/shapr/reference/vaeac_train_model_continue.md).

- epochs:

  Positive integer (default is `100`). The number of epochs to train the
  final vaeac model. This includes `epochs_initiation_phase`, where the
  default is `2`.

- save_every_nth_epoch:

  Positive integer (default is `NULL`). If provided, then the vaeac
  model after every `save_every_nth_epoch`th epoch will be saved.

- x_train_size:

  The object size of the `x_train` object.

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

## Value

The function does not return anything.

## Author

Lars Henry Berge Olsen
