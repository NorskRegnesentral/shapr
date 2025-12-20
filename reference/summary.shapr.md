# Summary Method for Shapr Objects

Provides a formatted summary of a shapr object and returns an object of
class `summary.shapr` containing the same information as returned by
[`get_results()`](https://norskregnesentral.github.io/shapr/reference/get_results.md).

## Usage

``` r
# S3 method for class 'shapr'
summary(object, digits = 2L, ...)
```

## Arguments

- object:

  A shapr object.

- digits:

  Integer. (Maximum) number of digits to be displayed after the decimal
  point. Defaults to 2.

- ...:

  Currently unused.

## Value

An object of class `summary.shapr`, which is a named list with the same
accessible components as returned by
[`get_results()`](https://norskregnesentral.github.io/shapr/reference/get_results.md).
See
[`get_results()`](https://norskregnesentral.github.io/shapr/reference/get_results.md)
for details about each component.
