# Function to create the optimizer used to train `vaeac`

Only
[`torch::optim_adam()`](https://torch.mlverse.org/docs/reference/optim_adam.html)
is currently supported. But it is easy to add an additional option
later.

## Usage

``` r
vaeac_get_optimizer(vaeac_model, lr, optimizer_name = "adam")
```

## Arguments

- vaeac_model:

  A `vaeac` model created using
  [`vaeac()`](https://norskregnesentral.github.io/shapr/reference/vaeac.md).

- lr:

  Positive numeric (default is `0.001`). The learning rate used in the
  [`torch::optim_adam()`](https://torch.mlverse.org/docs/reference/optim_adam.html)
  optimizer.

- optimizer_name:

  String containing the name of the
  [`torch::optimizer()`](https://torch.mlverse.org/docs/reference/optimizer.html)
  to use.

## Value

A
[`torch::optim_adam()`](https://torch.mlverse.org/docs/reference/optim_adam.html)
optimizer connected to the parameters of the `vaeac_model`.

## Author

Lars Henry Berge Olsen
