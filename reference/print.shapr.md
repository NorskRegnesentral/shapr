# Print Method for Shapr Objects

Print Method for Shapr Objects

## Usage

``` r
# S3 method for class 'shapr'
print(
  x,
  what = c("shapley_est", "shapley_sd", "MSEv", "MSEv_explicand", "MSEv_coalition",
    "timing_summary"),
  digits = 3L,
  ...
)
```

## Arguments

- x:

  A shapr object

- what:

  Character. Which component to print. Options are "shapley_est",
  "shapley_sd", "MSEv", "MSEv_explicand", "MSEv_coalition", and
  "timing_summary". Defaults to "shapley_est". Only one component can be
  printed at a time. See the details section of
  [`get_results()`](https://norskregnesentral.github.io/shapr/reference/get_results.md)
  for details about each component.

- digits:

  Integer. Number of significant digits to display. Defaults to 3.

- ...:

  Further arguments passed to
  [`data.table::print.data.table()`](https://rdatatable.gitlab.io/data.table/reference/print.data.table.html).

## Value

The object is returned invisibly after printing selected output.
