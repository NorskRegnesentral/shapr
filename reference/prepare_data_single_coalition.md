# Compute the conditional probabilities for a single coalition for the categorical approach

The
[`prepare_data.categorical()`](https://norskregnesentral.github.io/shapr/reference/prepare_data.md)
function is slow when evaluated for a single coalition. This is a
bottleneck for Causal Shapley values which call said function a lot with
single coalitions.

## Usage

``` r
prepare_data_single_coalition(internal, index_features)
```

## Arguments

- internal:

  List. Holds all parameters, data, functions and computed objects used
  within
  [`explain()`](https://norskregnesentral.github.io/shapr/reference/explain.md)
  The list contains one or more of the elements `parameters`, `data`,
  `objects`, `iter_list`, `timing_list`, `main_timing_list`, `output`,
  and `iter_timing_list`.

## Author

Lars Henry Berge Olsen
