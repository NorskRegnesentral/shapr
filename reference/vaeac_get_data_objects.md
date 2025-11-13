# Function to set up data loaders and save file names

Function to set up data loaders and save file names

## Usage

``` r
vaeac_get_data_objects(
  x_train,
  log_exp_cont_feat,
  val_ratio,
  batch_size,
  paired_sampling,
  model_description,
  depth,
  width,
  latent_dim,
  lr,
  epochs,
  save_every_nth_epoch,
  folder_to_save_model,
  train_indices = NULL,
  val_indices = NULL
)
```

## Arguments

- x_train:

  A data.table containing the training data. Categorical data must have
  class names \\1,2,\dots,K\\.

- log_exp_cont_feat:

  Logical (default is `FALSE`). If we are to \\\log\\ transform all
  continuous features before sending the data to
  [`vaeac()`](https://norskregnesentral.github.io/shapr/reference/vaeac.md).
  The `vaeac` model creates unbounded Monte Carlo sample values. Thus,
  if the continuous features are strictly positive (as for, e.g., the
  Burr distribution and Abalone data set), it can be advantageous to
  \\\log\\ transform the data to unbounded form before using `vaeac`. If
  `TRUE`, then
  [`vaeac_postprocess_data()`](https://norskregnesentral.github.io/shapr/reference/vaeac_postprocess_data.md)
  will take the \\\exp\\ of the results to get back to strictly positive
  values when using the `vaeac` model to impute missing values/generate
  the Monte Carlo samples.

- val_ratio:

  Numeric (default is `0.25`). Scalar between `0` and `1` indicating the
  ratio of instances from the input data which will be used as
  validation data. That is, `val_ratio = 0.25` means that `75%` of the
  provided data is used as training data, while the remaining `25%` is
  used as validation data.

- batch_size:

  Positive integer (default is `64`). The number of samples to include
  in each batch during the training of the vaeac model. Used in
  [`torch::dataloader()`](https://torch.mlverse.org/docs/reference/dataloader.html).

- paired_sampling:

  Logical (default is `TRUE`). If `TRUE`, we apply paired sampling to
  the training batches. That is, the training observations in each batch
  will be duplicated, where the first instance will be masked by \\S\\
  while the second instance will be masked by \\\bar{S}\\. This ensures
  that the training of the `vaeac` model becomes more stable as the
  model has access to the full version of each training observation.
  However, this will increase the training time due to more complex
  implementation and doubling the size of each batch. See
  [`paired_sampler()`](https://norskregnesentral.github.io/shapr/reference/paired_sampler.md)
  for more information.

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

- train_indices:

  Numeric array (optional) containing the indices of the training
  observations. There are conducted no checks to validate the indices.

- val_indices:

  Numeric array (optional) containing the indices of the validation
  observations. \#' There are conducted no checks to validate the
  indices.

## Value

List of objects needed to train the `vaeac` model
