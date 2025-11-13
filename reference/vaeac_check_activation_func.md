# Function that checks the provided activation function

Function that checks the provided activation function

## Usage

``` r
vaeac_check_activation_func(activation_function)
```

## Arguments

- activation_function:

  An
  [`torch::nn_module()`](https://torch.mlverse.org/docs/reference/nn_module.html)
  representing an activation function such as, e.g.,
  [`torch::nn_relu()`](https://torch.mlverse.org/docs/reference/nn_relu.html)
  (default),
  [`torch::nn_leaky_relu()`](https://torch.mlverse.org/docs/reference/nn_leaky_relu.html),
  [`torch::nn_selu()`](https://torch.mlverse.org/docs/reference/nn_selu.html),
  or
  [`torch::nn_sigmoid()`](https://torch.mlverse.org/docs/reference/nn_sigmoid.html).

## Value

The function does not return anything.

## Author

Lars Henry Berge Olsen
