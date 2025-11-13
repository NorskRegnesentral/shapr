# Impute Missing Values Using `vaeac`

Impute Missing Values Using `vaeac`

## Usage

``` r
vaeac_impute_missing_entries(
  x_explain_with_NaNs,
  n_MC_samples,
  vaeac_model,
  checkpoint,
  sampler,
  batch_size,
  verbose = NULL,
  seed = NULL,
  n_explain = NULL,
  index_features = NULL
)
```

## Arguments

- x_explain_with_NaNs:

  A 2D matrix, where the missing entries to impute are represented by
  `NaN`.

- n_MC_samples:

  Integer. The number of imputed versions we create for each row in
  `x_explain_with_NaNs`.

- vaeac_model:

  An initialized `vaeac` model that we are going to use to generate the
  MC samples.

- checkpoint:

  List containing the parameters of the `vaeac` model.

- sampler:

  A sampler object used to sample the MC samples.

- batch_size:

  Positive integer (default is `64`). The number of samples to include
  in each batch during the training of the vaeac model. Used in
  [`torch::dataloader()`](https://torch.mlverse.org/docs/reference/dataloader.html).

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

- n_explain:

  Positive integer. The number of explicands.

- index_features:

  Optional integer vector. Used internally in shapr package to index the
  coalitions.

## Value

A data.table where the missing values (`NaN`) in `x_explain_with_NaNs`
have been imputed `n_MC_samples` times. The data table will contain
extra id columns if `index_features` and `n_explain` are provided.

## Details

Function that imputes the missing values in 2D matrix where each row
constitute an individual. The values are sampled from the conditional
distribution estimated by a vaeac model.

## Author

Lars Henry Berge Olsen
