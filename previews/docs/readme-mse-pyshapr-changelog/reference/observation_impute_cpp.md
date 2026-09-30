# Get imputed data

Get imputed data

## Usage

``` r
observation_impute_cpp(index_xtrain, index_s, x_train, x_explain, S)
```

## Arguments

- index_xtrain:

  Positive integer. Represents a sequence of row indices from `x_train`,
  i.e. `min(index_xtrain) >= 1` and
  `max(index_xtrain) <= nrow(x_train)`.

- index_s:

  Positive integer. Represents a sequence of row indices from `S`, i.e.
  `min(index_s) >= 1` and `max(index_s) <= nrow(S)`.

- x_train:

  Matrix. Contains the training data.

- x_explain:

  Matrix with 1 row. Contains the features of the observation for a
  single prediction.

- S:

  arma::mat. Matrix of dimension (`n_coalitions`, `n_features`)
  containing binary representations of the used coalitions. S cannot
  contain the empty or grand coalition, i.e., a row containing only
  zeros or ones. This is not a problem internally in shapr as the empty
  and grand coalitions are treated differently.

## Value

Numeric matrix

## Details

`S(i, j) = 1` if and only if feature `j` is present in feature
combination `i`, otherwise `S(i, j) = 0`. I.e. if `m = 3`, there are
`2^3 = 8` unique ways to combine the features. In this case
`dim(S) = c(8, 3)`. Let's call the features `x1, x2, x3` and take a
closer look at the combination represented by `s = c(x1, x2)`. If this
combination is represented by the second row, the following is true:
`S[2, 1:3] = c(1, 1, 0)`.

The returned object, `X`, is a numeric matrix where
`dim(X) = c(length(index_xtrain), ncol(x_train))`. If feature `j` is
present in the k-th observation, that is `S[index_[k], j] == 1`,
`X[k, j] = x_explain[1, j]`. Otherwise
`X[k, j] = x_train[index_xtrain[k], j]`.

## Author

Nikolai Sellereite
