# Treat factors as numeric values

Factors are given a numeric value above the highest numeric value in the
data. The value of the different levels are sorted by factor and then
level.

## Usage

``` r
process_factor_data(dt, factor_cols)
```

## Arguments

- dt:

  data.table to plot

- factor_cols:

  Columns that are factors or character

## Value

A list of a lookup table with each factor and level and its numeric
value, a data.table very similar to the input data, but now with numeric
values for factors, and the maximum feature value.
