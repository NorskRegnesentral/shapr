# Continue to Train the `vaeac` Model

Function that loads a previously trained vaeac model and continue the
training, either on new data or on the same dataset as it was trained on
before. If we are given a new dataset, then we assume that new dataset
has the same distribution and one_hot_max_sizes as the original dataset.

## Usage

``` r
vaeac_train_model_continue(
  explanation,
  epochs_new,
  lr_new = NULL,
  x_train = NULL,
  save_data = FALSE,
  verbose = NULL,
  seed = 1
)
```

## Arguments

- explanation:

  A
  [`explain()`](https://norskregnesentral.github.io/shapr/reference/explain.md)
  object and `vaeac` must be the used approach.

- epochs_new:

  Positive integer. The number of extra epochs to conduct.

- lr_new:

  Positive numeric. If we are to overwrite the old learning rate in the
  adam optimizer.

- x_train:

  A data.table containing the training data. Categorical data must have
  class names \\1,2,\dots,K\\.

- save_data:

  Logical (default is `FALSE`). If `TRUE`, then the data is stored
  together with the model. Useful if one are to continue to train the
  model later using `vaeac_train_model_continue()`.

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

- seed:

  Positive integer (default is `1`). Seed for reproducibility. Specifies
  the seed before any randomness based code is being run.

## Value

A list containing the training/validation errors and paths to where the
vaeac models are saved on the disk.

## References

- [Olsen, L. H., Glad, I. K., Jullum, M., & Aas, K. (2022). Using
  Shapley values and variational autoencoders to explain predictive
  models with dependent mixed features. Journal of machine learning
  research, 23(213),
  1-51](https://www.jmlr.org/papers/volume23/21-1413/21-1413.pdf)

## Author

Lars Henry Berge Olsen
