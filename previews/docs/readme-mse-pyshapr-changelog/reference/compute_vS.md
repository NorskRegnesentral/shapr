# Compute `v(S)` for All Feature Subsets `S`

Compute `v(S)` for All Feature Subsets `S`

## Usage

``` r
compute_vS(internal, model, predict_model)
```

## Arguments

- internal:

  List. Not used directly, but passed through from
  [`explain()`](https://norskregnesentral.github.io/shapr/reference/explain.md).

## Value

List of `v(S)` for different coalitions `S`, optionally including the
samples used to estimate `v(S)`.
