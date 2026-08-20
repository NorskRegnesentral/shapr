# Function that checks provided epoch arguments

Function that checks provided epoch arguments

## Usage

``` r
vaeac_check_epoch_values(
  epochs,
  epochs_initiation_phase,
  epochs_early_stopping,
  save_every_nth_epoch,
  verbose
)
```

## Arguments

- epochs:

  Positive integer (default is `100`). The number of epochs to train the
  final vaeac model. This includes `epochs_initiation_phase`, where the
  default is `2`.

- epochs_initiation_phase:

  Positive integer (default is `2`). The number of epochs to run each of
  the `n_vaeacs_initialize` `vaeac` models before continuing to train
  only the best performing model.

- epochs_early_stopping:

  Positive integer (default is `NULL`). The training stops if there has
  been no improvement in the validation IWAE for `epochs_early_stopping`
  epochs. If the user wants the training process to be solely based on
  this training criterion, then `epochs` in
  [`explain()`](https://norskregnesentral.github.io/shapr/reference/explain.md)
  should be set to a large number. If `NULL`, then `shapr` will
  internally set `epochs_early_stopping = vaeac.epochs` such that early
  stopping does not occur.

- save_every_nth_epoch:

  Positive integer (default is `NULL`). If provided, then the vaeac
  model after every `save_every_nth_epoch`th epoch will be saved.

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
