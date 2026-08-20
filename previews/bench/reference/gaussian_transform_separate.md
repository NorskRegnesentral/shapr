# Transforms new data to standardized normal (dimension 1) based on other data transformations

Transforms new data to standardized normal (dimension 1) based on other
data transformations

## Usage

``` r
gaussian_transform_separate(yx, n_y)
```

## Arguments

- yx:

  Numeric vector. The first `n_y` items is the data that is transformed,
  and last part is the data with the original transformation.

- n_y:

  Positive integer. Number of elements of `yx` that belongs to the
  Gaussian data.

## Value

Vector of back-transformed Gaussian data

## Author

Martin Jullum
