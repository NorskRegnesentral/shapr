# get_cov_mat

get_cov_mat

## Usage

``` r
get_cov_mat(x_train, min_eigen_value = 1e-06)
```

## Arguments

- x_train:

  Matrix or data.frame/data.table. Data used to estimate the
  (conditional) feature distributions needed to properly estimate the
  conditional expectations in the Shapley formula.

- min_eigen_value:

  Numeric Specifies the smallest allowed eigen value before the
  covariance matrix of `x_train` is assumed to not be positive definite,
  and [`Matrix::nearPD()`](https://rdrr.io/pkg/Matrix/man/nearPD.html)
  is used to find the nearest one.
