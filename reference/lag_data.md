# Lag a matrix of variables a specific number of lags for each variables.

Lag a matrix of variables a specific number of lags for each variables.

## Usage

``` r
lag_data(x, lags)
```

## Arguments

- x:

  The matrix of variables (one variable per column).

- lags:

  A numeric vector denoting how many lags each variable should have.

## Value

A list with two items

- A matrix, lagged with the lagged data.

- A list, group, with groupings of the lagged data per variable.
