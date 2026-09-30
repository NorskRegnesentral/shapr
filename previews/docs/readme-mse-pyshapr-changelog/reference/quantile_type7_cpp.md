# Compute the quantiles using quantile type seven

Compute the quantiles using quantile type seven

## Usage

``` r
quantile_type7_cpp(x, probs)
```

## Arguments

- x:

  arma::vec. Numeric vector whose sample quantiles are wanted.

- probs:

  arma::vec. Numeric vector of probabilities with values between zero
  and one.

## Value

A vector of length `length(probs)` with the quantiles is returned.

## Details

Using quantile type number seven from stats::quantile in R.

## Author

Lars Henry Berge Olsen
