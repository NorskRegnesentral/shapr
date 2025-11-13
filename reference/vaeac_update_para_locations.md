# Move `vaeac` parameters to correct location

This function ensures that the main and extra parameters for the `vaeac`
approach is located at their right locations.

## Usage

``` r
vaeac_update_para_locations(parameters)
```

## Arguments

- parameters:

  List. The `internal$parameters` list created inside the
  [`explain()`](https://norskregnesentral.github.io/shapr/reference/explain.md)
  function.

## Value

Updated version of `parameters` where all `vaeac` parameters are located
at the correct location.

## Author

Lars Henry Berge Olsen
