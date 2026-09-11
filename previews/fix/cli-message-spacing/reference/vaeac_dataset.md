# Dataset used by the `vaeac` model

Convert a the data into a
[`torch::dataset()`](https://torch.mlverse.org/docs/reference/dataset.html)
which the vaeac model creates batches from.

## Usage

``` r
vaeac_dataset(X, one_hot_max_sizes)
```

## Arguments

- X:

  A torch_tensor contain the data of shape N x p, where N and p are the
  number of observations and features, respectively.

- one_hot_max_sizes:

  A torch tensor of dimension `n_features` containing the one hot sizes
  of the `n_features` features. That is, if the `i`th feature is a
  categorical feature with 5 levels, then `one_hot_max_sizes[i] = 5`.
  While the size for continuous features can either be `0` or `1`.

## Details

This function creates a
[`torch::dataset()`](https://torch.mlverse.org/docs/reference/dataset.html)
object that represent a map from keys to data samples. It is used by the
[`torch::dataloader()`](https://torch.mlverse.org/docs/reference/dataloader.html)
to load data which should be used to extract the batches for all epochs
in the training phase of the neural network. Note that a dataset object
is an R6 instance, see
<https://r6.r-lib.org/articles/Introduction.html>, which is classical
object-oriented programming, with self reference. I.e, `vaeac_dataset()`
is a subclass of type
[`torch::dataset()`](https://torch.mlverse.org/docs/reference/dataset.html).

## Author

Lars Henry Berge Olsen
