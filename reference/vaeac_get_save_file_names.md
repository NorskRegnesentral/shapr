# Function that creates the save file names for the `vaeac` model

Function that creates the save file names for the `vaeac` model

## Usage

``` r
vaeac_get_save_file_names(
  model_description,
  n_features,
  n_train,
  depth,
  width,
  latent_dim,
  lr,
  epochs,
  save_every_nth_epoch,
  folder_to_save_model = NULL
)
```

## Arguments

- model_description:

  String (default is `make.names(Sys.time())`). String containing, e.g.,
  the name of the data distribution or additional parameter information.
  Used in the save name of the fitted model. If not provided, then a
  name will be generated based on
  [`base::Sys.time()`](https://rdrr.io/r/base/Sys.time.html) to ensure a
  unique name. We use
  [`base::make.names()`](https://rdrr.io/r/base/make.names.html) to
  ensure a valid file name for all operating systems.

- depth:

  Positive integer (default is `3`). The number of hidden layers in the
  neural networks of the masked encoder, full encoder, and decoder.

- width:

  Positive integer (default is `32`). The number of neurons in each
  hidden layer in the neural networks of the masked encoder, full
  encoder, and decoder.

- latent_dim:

  Positive integer (default is `8`). The number of dimensions in the
  latent space.

- lr:

  Positive numeric (default is `0.001`). The learning rate used in the
  [`torch::optim_adam()`](https://torch.mlverse.org/docs/reference/optim_adam.html)
  optimizer.

- epochs:

  Positive integer (default is `100`). The number of epochs to train the
  final vaeac model. This includes `epochs_initiation_phase`, where the
  default is `2`.

- save_every_nth_epoch:

  Positive integer (default is `NULL`). If provided, then the vaeac
  model after every `save_every_nth_epoch`th epoch will be saved.

- folder_to_save_model:

  String (default is
  [`base::tempdir()`](https://rdrr.io/r/base/tempfile.html)). String
  specifying a path to a folder where the function is to save the fitted
  vaeac model. Note that the path will be removed from the returned
  [`explain()`](https://norskregnesentral.github.io/shapr/reference/explain.md)
  object if `vaeac.save_model = FALSE`.

## Value

Array of string containing the save files to use when training the
`vaeac` model. The first three names corresponds to the best,
best_running, and last epochs, in that order.

## Author

Lars Henry Berge Olsen
