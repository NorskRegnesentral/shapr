# A [`torch::nn_module()`](https://torch.mlverse.org/docs/reference/nn_module.html) Representing a skip connection

Skip connection over the sequence of layers in the constructor. The
module passes input data sequentially through these layers and then adds
original data to the result.

## Usage

``` r
skip_connection(...)
```

## Arguments

- ...:

  network modules such as, e.g.,
  [`torch::nn_linear()`](https://torch.mlverse.org/docs/reference/nn_linear.html),
  [`torch::nn_relu()`](https://torch.mlverse.org/docs/reference/nn_relu.html),
  and
  [`memory_layer()`](https://norskregnesentral.github.io/shapr/reference/memory_layer.md)
  objects. See
  [`vaeac()`](https://norskregnesentral.github.io/shapr/reference/vaeac.md)
  for more information.

## Author

Lars Henry Berge Olsen
