# A [`torch::nn_module()`](https://torch.mlverse.org/docs/reference/nn_module.html) Representing a Memory Layer

The layer is used to make skip-connections inside a
[`torch::nn_sequential()`](https://torch.mlverse.org/docs/reference/nn_sequential.html)
network or between several
[`torch::nn_sequential()`](https://torch.mlverse.org/docs/reference/nn_sequential.html)
networks without unnecessary code complication.

## Usage

``` r
memory_layer(id, shared_env, output = FALSE, add = FALSE, verbose = FALSE)
```

## Arguments

- id:

  A unique id to use as a key in the storage list.

- shared_env:

  A shared environment for all instances of memory_layer where the
  inputs are stored.

- output:

  Boolean variable indicating if the memory layer is to store input in
  storage or extract from storage.

- add:

  Boolean variable indicating if the extracted value are to be added or
  concatenated to the input. Only applicable when `output = TRUE`.

- verbose:

  Boolean variable indicating if we want to give printouts to the user.

## Details

If `output = FALSE`, this layer stores its input in the `shared_env`
with the key `id` and then passes the input to the next layer. I.e.,
when memory layer is used in the masked encoder. If `output = TRUE`,
this layer takes stored tensor from the storage. I.e., when memory layer
is used in the decoder. If `add = TRUE`, it returns sum of the stored
vector and an `input`, otherwise it returns their concatenation. If the
tensor with specified `id` is not in storage when the layer with
`output = TRUE` is called, it would cause an exception.

## Author

Lars Henry Berge Olsen
