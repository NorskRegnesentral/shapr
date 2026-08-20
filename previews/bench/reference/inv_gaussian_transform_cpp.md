# Transforms new data to a standardized normal distribution

Transforms new data to a standardized normal distribution

## Usage

``` r
inv_gaussian_transform_cpp(z, x)
```

## Arguments

- z:

  arma::mat. The data are the Gaussian Monte Carlos samples to
  transform.

- x:

  arma::mat. The data with the original transformation. Used to conduct
  the transformation of `z`.

## Value

arma::mat of the same dimension as `z`

## Author

Lars Henry Berge Olsen
