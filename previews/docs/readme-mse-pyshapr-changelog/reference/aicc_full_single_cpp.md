# Temp-function for computing the full AICc with several X's etc

Temp-function for computing the full AICc with several X's etc

## Usage

``` r
aicc_full_single_cpp(X, mcov, S_scale_dist, h, y)
```

## Arguments

- X:

  matrix.

- mcov:

  matrix The covariance matrix of X.

- S_scale_dist:

  logical. Indicating whether the Mahalanobis distance should be scaled
  with the number of variables

- h:

  numeric specifying the scaling (sigma)

- y:

  Vector Representing the (temporary) response variable

## Value

Scalar with the numeric value of the AICc formula.

## Author

Martin Jullum
