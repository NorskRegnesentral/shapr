# A [`torch::nn_module()`](https://torch.mlverse.org/docs/reference/nn_module.html) Representing a `gauss_cat_loss`

The `gauss_cat_loss module` layer computes the log probability of the
`groundtruth` for each object given the mask and the distribution
parameters. That is, the log-likelihoods of the true/full training
observations based on the generative distributions parameters
`distr_params` inferred by the masked versions of the observations.

## Usage

``` r
gauss_cat_loss(one_hot_max_sizes, min_sigma = 1e-04, min_prob = 1e-04)
```

## Arguments

- one_hot_max_sizes:

  A torch tensor of dimension `n_features` containing the one hot sizes
  of the `n_features` features. That is, if the `i`th feature is a
  categorical feature with 5 levels, then `one_hot_max_sizes[i] = 5`.
  While the size for continuous features can either be `0` or `1`.

- min_sigma:

  For stability it might be desirable that the minimal sigma is not too
  close to zero.

- min_prob:

  For stability it might be desirable that the minimal probability is
  not too close to zero.

## Details

Note that the module works with mixed data represented as 2-dimensional
inputs and it works correctly with missing values in `groundtruth` as
long as they are represented by NaNs.

## Author

Lars Henry Berge Olsen
