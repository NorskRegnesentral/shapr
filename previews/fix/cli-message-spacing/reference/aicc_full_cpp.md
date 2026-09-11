# AICc formula for several sets, alternative definition

AICc formula for several sets, alternative definition

## Usage

``` r
aicc_full_cpp(h, X_list, mcov_list, S_scale_dist, y_list, negative)
```

## Arguments

- h:

  numeric specifying the scaling (sigma)

- X_list:

  List. Contains matrices with the appropriate features of the training
  data

- mcov_list:

  List. Contains the covariance matrices of the matrices in X_list

- S_scale_dist:

  Logical. Indicates whether Mahalanobis distance should be scaled with
  the number of variables.

- y_list:

  List. Contains the appropriate (temporary) response variables.

- negative:

  Logical. Whether to return the negative of the AICc value.

## Value

Scalar with the numeric value of the AICc formula

## Author

Martin Jullum
