# A [`torch::nn_module()`](https://torch.mlverse.org/docs/reference/nn_module.html) Representing a `gauss_cat_parameters`

The `gauss_cat_parameters` module extracts the parameters from the
inferred generative Gaussian and categorical distributions for the
continuous and categorical features, respectively.

If `one_hot_max_sizes` is \\\[4, 1, 1, 2\]\\, then the inferred
distribution parameters for one observation is the vector \\\[p\_{00},
p\_{01}, p\_{02}, p\_{03}, \mu_1, \sigma_1, \mu_2, \sigma_2, p\_{30},
p\_{31}\]\\, where \\\operatorname{Softmax}(\[p\_{00}, p\_{01}, p\_{02},
p\_{03}\])\\ and \\\operatorname{Softmax}(\[p\_{30}, p\_{31}\])\\ are
probabilities of the first and the fourth feature categories
respectively in the model generative distribution, and Gaussian(\\\mu_1,
\sigma_1^2\\) and Gaussian(\\\mu_2, \sigma_2^2\\) are the model
generative distributions on the second and the third features.

## Usage

``` r
gauss_cat_parameters(one_hot_max_sizes, min_sigma = 1e-04, min_prob = 1e-04)
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

## Author

Lars Henry Berge Olsen
