# Function to load a `vaeac` model and set it in the right state and mode

Function to load a `vaeac` model and set it in the right state and mode

## Usage

``` r
vaeac_get_model_from_checkp(checkpoint, cuda, mode_train)
```

## Arguments

- checkpoint:

  List. This must be a loaded `vaeac` save object. That is,
  `torch::torch_load('vaeac_save_path')`.

- cuda:

  Logical (default is `FALSE`). If `TRUE`, then the `vaeac` model will
  be trained using cuda/GPU. If
  [`torch::cuda_is_available()`](https://torch.mlverse.org/docs/reference/cuda_is_available.html)
  is `FALSE`, we fall back to using the CPU. Using a GPU for smaller
  tabular dataset often do not improve the efficiency. See
  [`vignette("installation", package = "torch")`](https://torch.mlverse.org/docs/articles/installation.html)
  fo help to enable running on the GPU (only Linux and Windows).

- mode_train:

  Logical. If `TRUE`, the returned `vaeac` model is set to be in
  training mode. If `FALSE`, the returned `vaeac` model is set to be in
  evaluation mode.

## Value

A `vaeac` model with the correct state (based on `checkpoint`), sent to
the desired hardware (based on `cuda`), and in the right mode (based on
`mode_train`).

## Author

Lars Henry Berge Olsen
