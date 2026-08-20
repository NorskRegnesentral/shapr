# Function that samples data from the empirical marginal training distribution

Sample observations from the empirical distribution P(X) using the
training dataset.

## Usage

``` r
create_marginal_data_training(
  x_train,
  n_explain,
  Sbar_features,
  n_MC_samples = 1000,
  stable_version = TRUE
)
```

## Arguments

- x_train:

  Data.table with training data.

- Sbar_features:

  Vector of integers containing the features indices to generate
  marginal observations for. That is, if `Sbar_features` is `c(1,4)`,
  then we sample `n_MC_samples` observations from \\P(X_1, X_4)\\ using
  the empirical training observations (with replacements). That is, we
  sample the first and fourth feature values from the same training
  observation, so we do not break the dependence between them.

- stable_version:

  Logical. If `TRUE` and `n_MC_samples` \> `n_train`, then we include
  each training observation `n_MC_samples %/% n_train` times and then
  sample the remaining `n_MC_samples %% n_train samples`. Only the
  latter is done when `n_MC_samples < n_train`. This is done separately
  for each explicand. If `FALSE`, we randomly sample the from the
  observations.

## Value

Data table of dimension `n_MC_samples` \\\times\\
`length(Sbar_features)` with the sampled observations.

## Author

Lars Henry Berge Olsen
