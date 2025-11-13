# Compute the KL Divergence Between Two Gaussian Distributions.

Computes the KL divergence between univariate normal distributions using
the analytical formula, see
<https://en.wikipedia.org/wiki/Kullback%E2%80%93Leibler_divergence#Multivariate_normal_distributions>.

## Usage

``` r
vaeac_kl_normal_normal(p, q)
```

## Arguments

- p:

  A
  [`torch::distr_normal()`](https://torch.mlverse.org/docs/reference/distr_normal.html)
  object.

- q:

  A
  [`torch::distr_normal()`](https://torch.mlverse.org/docs/reference/distr_normal.html)
  object.

## Value

The KL divergence between the two Gaussian distributions.

## Author

Lars Henry Berge Olsen
