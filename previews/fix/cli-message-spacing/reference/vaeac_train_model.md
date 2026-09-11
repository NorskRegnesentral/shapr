# Train the `vaeac` Model

Function that fits a vaeac model to the given dataset based on the
provided parameters, as described in [Olsen et al.
(2022)](https://www.jmlr.org/papers/volume23/21-1413/21-1413.pdf). Note
that all default parameters specified below origin from
[`setup_approach.vaeac()`](https://norskregnesentral.github.io/shapr/reference/setup_approach.md)
and
[`vaeac_get_extra_para_default()`](https://norskregnesentral.github.io/shapr/reference/vaeac_get_extra_para_default.md).

## Usage

``` r
vaeac_train_model(
  x_train,
  model_description,
  folder_to_save_model,
  cuda,
  n_vaeacs_initialize,
  epochs_initiation_phase,
  epochs,
  epochs_early_stopping,
  save_every_nth_epoch,
  val_ratio,
  val_iwae_n_samples,
  depth,
  width,
  latent_dim,
  lr,
  batch_size,
  running_avg_n_values,
  activation_function,
  skip_conn_layer,
  skip_conn_masked_enc_dec,
  batch_normalization,
  paired_sampling,
  masking_ratio,
  mask_gen_coalitions,
  mask_gen_coalitions_prob,
  sigma_mu,
  sigma_sigma,
  save_data,
  log_exp_cont_feat,
  which_vaeac_model,
  verbose,
  seed,
  ...
)
```

## Arguments

- x_train:

  A data.table containing the training data. Categorical data must have
  class names \\1,2,\dots,K\\.

- model_description:

  String (default is `make.names(Sys.time())`). String containing, e.g.,
  the name of the data distribution or additional parameter information.
  Used in the save name of the fitted model. If not provided, then a
  name will be generated based on
  [`base::Sys.time()`](https://rdrr.io/r/base/Sys.time.html) to ensure a
  unique name. We use
  [`base::make.names()`](https://rdrr.io/r/base/make.names.html) to
  ensure a valid file name for all operating systems.

- folder_to_save_model:

  String (default is
  [`base::tempdir()`](https://rdrr.io/r/base/tempfile.html)). String
  specifying a path to a folder where the function is to save the fitted
  vaeac model. Note that the path will be removed from the returned
  [`explain()`](https://norskregnesentral.github.io/shapr/reference/explain.md)
  object if `vaeac.save_model = FALSE`.

- cuda:

  Logical (default is `FALSE`). If `TRUE`, then the `vaeac` model will
  be trained using cuda/GPU. If
  [`torch::cuda_is_available()`](https://torch.mlverse.org/docs/reference/cuda_is_available.html)
  is `FALSE`, we fall back to using the CPU. Using a GPU for smaller
  tabular dataset often do not improve the efficiency. See
  [`vignette("installation", package = "torch")`](https://torch.mlverse.org/docs/articles/installation.html)
  fo help to enable running on the GPU (only Linux and Windows).

- n_vaeacs_initialize:

  Positive integer (default is `4`). The number of different vaeac
  models to initiate in the start. Pick the best performing one after
  `epochs_initiation_phase` epochs (default is `2`) and continue
  training that one.

- epochs_initiation_phase:

  Positive integer (default is `2`). The number of epochs to run each of
  the `n_vaeacs_initialize` `vaeac` models before continuing to train
  only the best performing model.

- epochs:

  Positive integer (default is `100`). The number of epochs to train the
  final vaeac model. This includes `epochs_initiation_phase`, where the
  default is `2`.

- epochs_early_stopping:

  Positive integer (default is `NULL`). The training stops if there has
  been no improvement in the validation IWAE for `epochs_early_stopping`
  epochs. If the user wants the training process to be solely based on
  this training criterion, then `epochs` in
  [`explain()`](https://norskregnesentral.github.io/shapr/reference/explain.md)
  should be set to a large number. If `NULL`, then `shapr` will
  internally set `epochs_early_stopping = vaeac.epochs` such that early
  stopping does not occur.

- save_every_nth_epoch:

  Positive integer (default is `NULL`). If provided, then the vaeac
  model after every `save_every_nth_epoch`th epoch will be saved.

- val_ratio:

  Numeric (default is `0.25`). Scalar between `0` and `1` indicating the
  ratio of instances from the input data which will be used as
  validation data. That is, `val_ratio = 0.25` means that `75%` of the
  provided data is used as training data, while the remaining `25%` is
  used as validation data.

- val_iwae_n_samples:

  Positive integer (default is `25`). The number of generated samples
  used to compute the IWAE criterion when validating the vaeac model on
  the validation data.

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

- batch_size:

  Positive integer (default is `64`). The number of samples to include
  in each batch during the training of the vaeac model. Used in
  [`torch::dataloader()`](https://torch.mlverse.org/docs/reference/dataloader.html).

- running_avg_n_values:

  running_avg_n_values Positive integer (default is `5`). The number of
  previous IWAE values to include when we compute the running means of
  the IWAE criterion.

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

- skip_conn_layer:

  Logical (default is `TRUE`). If `TRUE`, we apply identity skip
  connections in each layer, see
  [`skip_connection()`](https://norskregnesentral.github.io/shapr/reference/skip_connection.md).
  That is, we add the input \\X\\ to the outcome of each hidden layer,
  so the output becomes \\X + activation(WX + b)\\.

- skip_conn_masked_enc_dec:

  Logical (default is `TRUE`). If `TRUE`, we apply concatenate skip
  connections between the layers in the masked encoder and decoder. The
  first layer of the masked encoder will be linked to the last layer of
  the decoder. The second layer of the masked encoder will be linked to
  the second to last layer of the decoder, and so on.

- batch_normalization:

  Logical (default is `FALSE`). If `TRUE`, we apply batch normalization
  after the activation function. Note that if `skip_conn_layer = TRUE`,
  then the normalization is applied after the inclusion of the skip
  connection. That is, we batch normalize the whole quantity \\X +
  activation(WX + b)\\.

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

- masking_ratio:

  Numeric (default is `0.5`). Probability of masking a feature in the
  [`mcar_mask_generator()`](https://norskregnesentral.github.io/shapr/reference/mcar_mask_generator.md)
  (MCAR = Missing Completely At Random). The MCAR masking scheme ensures
  that `vaeac` model can do arbitrary conditioning as all coalitions
  will be trained. `masking_ratio` will be overruled if
  `mask_gen_coalitions` is specified.

- mask_gen_coalitions:

  Matrix (default is `NULL`). Matrix containing the coalitions that the
  `vaeac` model will be trained on, see
  [`specified_masks_mask_generator()`](https://norskregnesentral.github.io/shapr/reference/specified_masks_mask_generator.md).
  This parameter is used internally in `shapr` when we only consider a
  subset of coalitions, i.e., when `n_coalitions` \\\<
  2^{n\_{\text{features}}}\\, and for group Shapley, i.e., when `group`
  is specified in
  [`explain()`](https://norskregnesentral.github.io/shapr/reference/explain.md).

- mask_gen_coalitions_prob:

  Numeric array (default is `NULL`). Array of length equal to the height
  of `mask_gen_coalitions` containing the probabilities of sampling the
  corresponding coalitions in `mask_gen_coalitions`.

- sigma_mu:

  Numeric (default is `1e4`). One of two hyperparameter values in the
  normal-gamma prior used in the masked encoder, see Section 3.3.1 in
  [Olsen et al.
  (2022)](https://www.jmlr.org/papers/volume23/21-1413/21-1413.pdf).

- sigma_sigma:

  Numeric (default is `1e-4`). One of two hyperparameter values in the
  normal-gamma prior used in the masked encoder, see Section 3.3.1 in
  [Olsen et al.
  (2022)](https://www.jmlr.org/papers/volume23/21-1413/21-1413.pdf).

- save_data:

  Logical (default is `FALSE`). If `TRUE`, then the data is stored
  together with the model. Useful if one are to continue to train the
  model later using
  [`vaeac_train_model_continue()`](https://norskregnesentral.github.io/shapr/reference/vaeac_train_model_continue.md).

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

- which_vaeac_model:

  String (default is `best`). The name of the `vaeac` model (snapshots
  from different epochs) to use when generating the Monte Carlo samples.
  The standard choices are: `"best"` (epoch with lowest IWAE),
  `"best_running"` (epoch with lowest running IWAE, see
  `vaeac.running_avg_n_values`), and `last` (the last epoch). Note that
  additional choices are available if `vaeac.save_every_nth_epoch` is
  provided. For example, if `vaeac.save_every_nth_epoch = 5`, then
  `vaeac.which_vaeac_model` can also take the values `"epoch_5"`,
  `"epoch_10"`, `"epoch_15"`, and so on.

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

- seed:

  Positive integer (default is `1`). Seed for reproducibility. Specifies
  the seed before any randomness based code is being run.

- ...:

  List of extra parameters, currently not used.

## Value

A list containing the training/validation errors and paths to where the
vaeac models are saved on the disk.

## Details

The vaeac model consists of three neural networks, i.e., a masked
encoder, a full encoder, and a decoder. The networks have shared
`depth`, `width`, and `activation_function`. The encoders maps the
`x_train` to a latent representation of dimension `latent_dim`, while
the decoder maps the latent representations back to the feature space.
See [Olsen et al.
(2022)](https://www.jmlr.org/papers/volume23/21-1413/21-1413.pdf) for
more details. The function first initiates `n_vaeacs_initialize` vaeac
models with different randomly initiated network parameter values to
remedy poorly initiated values. After `epochs_initiation_phase` epochs,
the `n_vaeacs_initialize` vaeac models are compared and the function
continues to only train the best performing one for a total of `epochs`
epochs. The networks are trained using the ADAM optimizer with the
learning rate is `lr`.

## References

- [Olsen, L. H., Glad, I. K., Jullum, M., & Aas, K. (2022). Using
  Shapley values and variational autoencoders to explain predictive
  models with dependent mixed features. Journal of machine learning
  research, 23(213),
  1-51](https://www.jmlr.org/papers/volume23/21-1413/21-1413.pdf)

## Author

Lars Henry Berge Olsen
