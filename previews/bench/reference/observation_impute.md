# Generate permutations of training data using test observations

Generate permutations of training data using test observations

## Usage

``` r
observation_impute(
  W_kernel,
  S,
  x_train,
  x_explain,
  empirical.eta = 0.7,
  n_MC_samples = 1000
)
```

## Arguments

- W_kernel:

  Numeric matrix. Contains all non-scaled weights between training and
  test observations for all coalitions. The dimension equals
  `n_train x m`.

- S:

  Integer matrix of dimension `n_coalitions x m`, where `n_coalitions`
  and `m` equals the total number of sampled/non-sampled coalitions and
  the total number of unique features, respectively. Note that
  `m = ncol(x_train)`.

- x_train:

  Data.table with training data.

- x_explain:

  Data.table with the features of the observation whose predictions
  ought to be explained (test data).

## Value

data.table

## Author

Nikolai Sellereite
