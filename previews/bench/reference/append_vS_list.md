# Append the New `vS_list` to the Previous `vS_list`

Append the New `vS_list` to the Previous `vS_list`

## Usage

``` r
append_vS_list(vS_list, internal)
```

## Arguments

- vS_list:

  List. Output from
  [`compute_vS()`](https://norskregnesentral.github.io/shapr/reference/compute_vS.md).

- internal:

  List. Not used directly, but passed through from
  [`explain()`](https://norskregnesentral.github.io/shapr/reference/explain.md).

## Value

The vS_list after being merged with previously computed vS_lists (stored
in internal)
