# Check the `regression.surrogate_n_comb` parameter

Check that `regression.surrogate_n_comb` is either NULL or a valid
integer.

## Usage

``` r
regression.check_sur_n_comb(regression.surrogate_n_comb, n_coalitions)
```

## Arguments

- regression.surrogate_n_comb:

  Positive integer. Specifies the number of unique coalitions to apply
  to each training observation. The default is the number of sampled
  coalitions in the present iteration. Any integer between 1 and the
  default is allowed. Larger values requires more memory, but may
  improve the surrogate model. If the user sets a value lower than the
  maximum, we sample this amount of unique coalitions separately for
  each training observations. That is, on average, all coalitions should
  be equally trained.

- n_coalitions:

  Integer. The number of used coalitions (including the empty and grand
  coalition).

## Author

Lars Henry Berge Olsen
