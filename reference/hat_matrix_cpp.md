# Computing single H matrix in AICc-function using the Mahalanobis distance

Computing single H matrix in AICc-function using the Mahalanobis
distance

## Usage

``` r
hat_matrix_cpp(X, mcov, S_scale_dist, h)
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

## Value

Matrix of dimension `ncol(X)*ncol(X)`

## Author

Martin Jullum
