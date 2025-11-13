# Generate data used for predictions and Monte Carlo integration

Generate data used for predictions and Monte Carlo integration

## Usage

``` r
prepare_data(internal, index_features = NULL, ...)

# S3 method for class 'categorical'
prepare_data(internal, index_features = NULL, ...)

# S3 method for class 'copula'
prepare_data(internal, index_features, ...)

# S3 method for class 'ctree'
prepare_data(internal, index_features = NULL, ...)

# S3 method for class 'empirical'
prepare_data(internal, index_features = NULL, ...)

# S3 method for class 'gaussian'
prepare_data(internal, index_features, ...)

# S3 method for class 'independence'
prepare_data(internal, index_features = NULL, ...)

# S3 method for class 'regression_separate'
prepare_data(internal, index_features = NULL, ...)

# S3 method for class 'regression_surrogate'
prepare_data(internal, index_features = NULL, ...)

# S3 method for class 'timeseries'
prepare_data(internal, index_features = NULL, ...)

# S3 method for class 'vaeac'
prepare_data(internal, index_features = NULL, ...)
```

## Arguments

- internal:

  List. Not used directly, but passed through from
  [`explain()`](https://norskregnesentral.github.io/shapr/reference/explain.md).

- index_features:

  Positive integer vector. Specifies the id_coalition to apply to the
  present method. `NULL` means all coalitions. Only used internally.

- ...:

  Currently not used.

## Value

A data.table containing simulated data used to estimate the contribution
function by Monte Carlo integration.

## Author

Martin Jullum

Annabelle Redelmeier and Lars Henry Berge Olsen

Lars Henry Berge Olsen

Martin Jullum,
