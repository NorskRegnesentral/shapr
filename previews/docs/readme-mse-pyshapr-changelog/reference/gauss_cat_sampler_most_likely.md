# A [`torch::nn_module()`](https://torch.mlverse.org/docs/reference/nn_module.html) Representing a `gauss_cat_sampler_most_likely`

The `gauss_cat_sampler_most_likely` generates the most likely samples
from the generative distribution defined by the output of the vaeac.
I.e., the layer will return the mean and most probable class for the
Gaussian (continuous features) and categorical (categorical features)
distributions, respectively.

## Usage

``` r
gauss_cat_sampler_most_likely(
  one_hot_max_sizes,
  min_sigma = 1e-04,
  min_prob = 1e-04
)
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

## Value

A `gauss_cat_sampler_most_likely` object.

## Author

Lars Henry Berge Olsen
