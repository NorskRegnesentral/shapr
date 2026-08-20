# Creates Normal Distributions

Function that takes in the a tensor where the first half of the columns
contains the means of the normal distributions, while the latter half of
the columns contains the standard deviations. The standard deviations
are clamped with `min_sigma` to ensure stable results. If `params` is of
dimensions batch_size x 8, the function will create 4 independent normal
distributions for each of the observation (`batch_size` observations in
total).

## Usage

``` r
vaeac_normal_parse_params(params, min_sigma = 1e-04)
```

## Arguments

- params:

  Tensor of dimension `batch_size` x `2*n_featuers` containing the means
  and standard deviations to be used in the normal distributions for of
  the `batch_size` observations.

- min_sigma:

  For stability it might be desirable that the minimal sigma is not too
  close to zero.

## Value

A
[`torch::distr_normal()`](https://torch.mlverse.org/docs/reference/distr_normal.html)
distribution with the provided means and standard deviations.

## Details

Take a Tensor (e.g. neural network output) and return a
[`torch::distr_normal()`](https://torch.mlverse.org/docs/reference/distr_normal.html)
distribution. This normal distribution is component-wise independent,
and its dimensionality depends on the input shape. First half of
channels is mean (\\\mu\\) of the distribution, the softplus of the
second half is std (\\\sigma\\), so there is no restrictions on the
input tensor. `min_sigma` is the minimal value of \\\sigma\\. I.e., if
the above softplus is less than `min_sigma`, then \\\sigma\\ is clipped
from below with value `min_sigma`. This regularization is required for
the numerical stability and may be considered as a neural network
architecture choice without any change to the probabilistic model.

## Author

Lars Henry Berge Olsen
