# (Generalized) Mahalanobis distance

Used to get the Euclidean distance as well by setting `mcov` =
`diag(m)`.

## Usage

``` r
mahalanobis_distance_cpp(
  featureList,
  Xtrain_mat,
  Xexplain_mat,
  mcov,
  S_scale_dist
)
```

## Arguments

- featureList:

  List. Contains the vectors indicating all factor combinations that
  should be included in the computations. Assumes that the first one is
  empty.

- Xtrain_mat:

  Matrix Training data in matrix form

- Xexplain_mat:

  Matrix Explanation data in matrix form.

- mcov:

  matrix The covariance matrix of X.

- S_scale_dist:

  logical. Indicating whether the Mahalanobis distance should be scaled
  with the number of variables

## Value

Array of three dimensions. Contains the squared distance for between all
training and test observations for all feature combinations passed to
the function.

## Author

Martin Jullum
