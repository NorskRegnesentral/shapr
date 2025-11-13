# Function used to train a `vaeac` model

This function can be applied both in the initialization phase when, we
train several initiated `vaeac` models, and to keep training the best
performing `vaeac` model for the remaining number of epochs. We are in
the former setting when `initialization_idx` is provided and the latter
when it is `NULL`. When it is `NULL`, we save the `vaeac` models with
lowest VLB, IWAE, running IWAE, and the epochs according to
`save_every_nth_epoch` to disk.

## Usage

``` r
vaeac_train_model_auxiliary(
  vaeac_model,
  optimizer,
  train_dataloader,
  val_dataloader,
  val_iwae_n_samples,
  running_avg_n_values,
  verbose,
  cuda,
  epochs,
  save_every_nth_epoch,
  epochs_early_stopping,
  epochs_start = 1,
  progressr_bar = NULL,
  vaeac_save_file_names = NULL,
  state_list = NULL,
  initialization_idx = NULL,
  n_vaeacs_initialize = NULL,
  train_vlb = NULL,
  val_iwae = NULL,
  val_iwae_running = NULL
)
```

## Arguments

- vaeac_model:

  A
  [`vaeac()`](https://norskregnesentral.github.io/shapr/reference/vaeac.md)
  object. The `vaeac` model this function is to train.

- optimizer:

  A
  [`torch::optimizer()`](https://torch.mlverse.org/docs/reference/optimizer.html)
  object. See
  [`vaeac_get_optimizer()`](https://norskregnesentral.github.io/shapr/reference/vaeac_get_optimizer.md).

- train_dataloader:

  A
  [`torch::dataloader()`](https://torch.mlverse.org/docs/reference/dataloader.html)
  containing the training data for the `vaeac` model.

- val_dataloader:

  A
  [`torch::dataloader()`](https://torch.mlverse.org/docs/reference/dataloader.html)
  containing the validation data for the `vaeac` model.

- val_iwae_n_samples:

  Positive integer (default is `25`). The number of generated samples
  used to compute the IWAE criterion when validating the vaeac model on
  the validation data.

- running_avg_n_values:

  running_avg_n_values Positive integer (default is `5`). The number of
  previous IWAE values to include when we compute the running means of
  the IWAE criterion.

- verbose:

  String vector or NULL. Controls verbosity (printout detail level) via
  one or more of `"basic"`, `"progress"`, `"convergence"`, `"shapley"`
  and `"vS_details"`. `"basic"` (default) displays basic information
  about the computation and messages about parameters/checks.
  `"progress"` displays where in the calculation process the function
  currently is. `"convergence"` displays how close the Shapley value
  estimates are to convergence (only when `iterative = TRUE`).
  `"shapley"` displays intermediate Shapley value estimates and standard
  deviations (only when `iterative = TRUE`), and the final estimates.
  `"vS_details"` displays information about the v(S) estimates, most
  relevant for
  `approach %in% c("regression_separate", "regression_surrogate", "vaeac")`.
  `NULL` means no printout. Any combination can be used, e.g.,
  `verbose = c("basic", "vS_details")`.

- cuda:

  Logical (default is `FALSE`). If `TRUE`, then the `vaeac` model will
  be trained using cuda/GPU. If
  [`torch::cuda_is_available()`](https://torch.mlverse.org/docs/reference/cuda_is_available.html)
  is `FALSE`, we fall back to using the CPU. Using a GPU for smaller
  tabular dataset often do not improve the efficiency. See
  [`vignette("installation", package = "torch")`](https://torch.mlverse.org/docs/articles/installation.html)
  fo help to enable running on the GPU (only Linux and Windows).

- epochs:

  Positive integer (default is `100`). The number of epochs to train the
  final vaeac model. This includes `epochs_initiation_phase`, where the
  default is `2`.

- save_every_nth_epoch:

  Positive integer (default is `NULL`). If provided, then the vaeac
  model after every `save_every_nth_epoch`th epoch will be saved.

- epochs_early_stopping:

  Positive integer (default is `NULL`). The training stops if there has
  been no improvement in the validation IWAE for `epochs_early_stopping`
  epochs. If the user wants the training process to be solely based on
  this training criterion, then `epochs` in
  [`explain()`](https://norskregnesentral.github.io/shapr/reference/explain.md)
  should be set to a large number. If `NULL`, then `shapr` will
  internally set `epochs_early_stopping = vaeac.epochs` such that early
  stopping does not occur.

- epochs_start:

  Positive integer (default is `1`). At which epoch the training is
  starting at.

- progressr_bar:

  A
  [`progressr::progressor()`](https://progressr.futureverse.org/reference/progressor.html)
  object (default is `NULL`) to keep track of progress.

- vaeac_save_file_names:

  Array of strings containing the save file names for the `vaeac` model.

- state_list:

  Named list containing the objects returned from
  [`vaeac_get_full_state_list()`](https://norskregnesentral.github.io/shapr/reference/vaeac_get_full_state_list.md).

- initialization_idx:

  Positive integer (default is `NULL`). The index of the current `vaeac`
  model in the initialization phase.

- n_vaeacs_initialize:

  Positive integer (default is `4`). The number of different vaeac
  models to initiate in the start. Pick the best performing one after
  `epochs_initiation_phase` epochs (default is `2`) and continue
  training that one.

- train_vlb:

  A
  [`torch::torch_tensor()`](https://torch.mlverse.org/docs/reference/torch_tensor.html)
  (default is `NULL`) of one dimension containing previous values for
  the training VLB.

- val_iwae:

  A
  [`torch::torch_tensor()`](https://torch.mlverse.org/docs/reference/torch_tensor.html)
  (default is `NULL`) of one dimension containing previous values for
  the validation IWAE.

- val_iwae_running:

  A
  [`torch::torch_tensor()`](https://torch.mlverse.org/docs/reference/torch_tensor.html)
  (default is `NULL`) of one dimension containing previous values for
  the running validation IWAE.

## Value

Depending on if we are in the initialization phase or not. Then either
the trained `vaeac` model, or a list of where the `vaeac` models are
stored on disk and the parameters of the model.

## Author

Lars Henry Berge Olsen
