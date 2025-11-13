# Specify the Extra Parameters in the `vaeac` Model

In this function, we specify the default values for the extra parameters
used in
[`explain()`](https://norskregnesentral.github.io/shapr/reference/explain.md)
for `approach = "vaeac"`.

## Usage

``` r
vaeac_get_extra_para_default(
  vaeac.model_description = make.names(Sys.time()),
  vaeac.folder_to_save_model = tempdir(),
  vaeac.pretrained_vaeac_model = NULL,
  vaeac.cuda = FALSE,
  vaeac.epochs_initiation_phase = 2,
  vaeac.epochs_early_stopping = NULL,
  vaeac.save_every_nth_epoch = NULL,
  vaeac.val_ratio = 0.25,
  vaeac.val_iwae_n_samples = 25,
  vaeac.batch_size = 64,
  vaeac.batch_size_sampling = NULL,
  vaeac.running_avg_n_values = 5,
  vaeac.skip_conn_layer = TRUE,
  vaeac.skip_conn_masked_enc_dec = TRUE,
  vaeac.batch_normalization = FALSE,
  vaeac.paired_sampling = TRUE,
  vaeac.masking_ratio = 0.5,
  vaeac.mask_gen_coalitions = NULL,
  vaeac.mask_gen_coalitions_prob = NULL,
  vaeac.sigma_mu = 10000,
  vaeac.sigma_sigma = 1e-04,
  vaeac.sample_random = TRUE,
  vaeac.save_data = FALSE,
  vaeac.log_exp_cont_feat = FALSE,
  vaeac.which_vaeac_model = "best",
  vaeac.save_model = TRUE
)
```

## Arguments

- vaeac.model_description:

  String (default is `make.names(Sys.time())`). String containing, e.g.,
  the name of the data distribution or additional parameter information.
  Used in the save name of the fitted model. If not provided, then a
  name will be generated based on
  [`base::Sys.time()`](https://rdrr.io/r/base/Sys.time.html) to ensure a
  unique name. We use
  [`base::make.names()`](https://rdrr.io/r/base/make.names.html) to
  ensure a valid file name for all operating systems.

- vaeac.folder_to_save_model:

  String (default is
  [`base::tempdir()`](https://rdrr.io/r/base/tempfile.html)). String
  specifying a path to a folder where the function is to save the fitted
  vaeac model. Note that the path will be removed from the returned
  [`explain()`](https://norskregnesentral.github.io/shapr/reference/explain.md)
  object if `vaeac.save_model = FALSE`. Furthermore, the model cannot be
  moved from its original folder if we are to use the
  [`vaeac_train_model_continue()`](https://norskregnesentral.github.io/shapr/reference/vaeac_train_model_continue.md)
  function to continue training the model.

- vaeac.pretrained_vaeac_model:

  List or String (default is `NULL`). 1) Either a list of class `vaeac`,
  i.e., the list stored in `explanation$internal$parameters$vaeac` where
  `explanation` is the returned list from an earlier call to the
  [`explain()`](https://norskregnesentral.github.io/shapr/reference/explain.md)
  function. 2) A string containing the path to where the `vaeac` model
  is stored on disk, for example,
  `explanation$internal$parameters$vaeac$models$best`.

- vaeac.cuda:

  Logical (default is `FALSE`). If `TRUE`, then the `vaeac` model will
  be trained using cuda/GPU. If
  [`torch::cuda_is_available()`](https://torch.mlverse.org/docs/reference/cuda_is_available.html)
  is `FALSE`, we fall back to using the CPU. Using a GPU for smaller
  tabular dataset often do not improve the efficiency. See
  [`vignette("installation", package = "torch")`](https://torch.mlverse.org/docs/articles/installation.html)
  fo help to enable running on the GPU (only Linux and Windows).

- vaeac.epochs_initiation_phase:

  Positive integer (default is `2`). The number of epochs to run each of
  the `vaeac.n_vaeacs_initialize` `vaeac` models before continuing to
  train only the best performing model.

- vaeac.epochs_early_stopping:

  Positive integer (default is `NULL`). The training stops if there has
  been no improvement in the validation IWAE for
  `vaeac.epochs_early_stopping` epochs. If the user wants the training
  process to be solely based on this training criterion, then
  `vaeac.epochs` in
  [`explain()`](https://norskregnesentral.github.io/shapr/reference/explain.md)
  should be set to a large number. If `NULL`, then `shapr` will
  internally set `vaeac.epochs_early_stopping = vaeac.epochs` such that
  early stopping does not occur.

- vaeac.save_every_nth_epoch:

  Positive integer (default is `NULL`). If provided, then the vaeac
  model after every `vaeac.save_every_nth_epoch`th epoch will be saved.

- vaeac.val_ratio:

  Numeric (default is `0.25`). Scalar between `0` and `1` indicating the
  ratio of instances from the input data which will be used as
  validation data. That is, `vaeac.val_ratio = 0.25` means that `75%` of
  the provided data is used as training data, while the remaining `25%`
  is used as validation data.

- vaeac.val_iwae_n_samples:

  Positive integer (default is `25`). The number of generated samples
  used to compute the IWAE criterion when validating the vaeac model on
  the validation data.

- vaeac.batch_size:

  Positive integer (default is `64`). The number of samples to include
  in each batch during the training of the vaeac model. Used in
  [`torch::dataloader()`](https://torch.mlverse.org/docs/reference/dataloader.html).

- vaeac.batch_size_sampling:

  Positive integer (default is `NULL`) The number of samples to include
  in each batch when generating the Monte Carlo samples. If `NULL`, then
  the function generates the Monte Carlo samples for the provided
  coalitions and all explicands sent to
  [`explain()`](https://norskregnesentral.github.io/shapr/reference/explain.md)
  at the time. The number of coalitions are determined by the
  `n_batches` used by
  [`explain()`](https://norskregnesentral.github.io/shapr/reference/explain.md).
  We recommend to tweak `extra_computation_args$max_batch_size` and
  `extra_computation_args$min_n_batches` rather than
  `vaeac.batch_size_sampling`. Larger batch sizes are often much faster
  provided sufficient memory.

- vaeac.running_avg_n_values:

  Positive integer (default is `5`). The number of previous IWAE values
  to include when we compute the running means of the IWAE criterion.

- vaeac.skip_conn_layer:

  Logical (default is `TRUE`). If `TRUE`, we apply identity skip
  connections in each layer, see
  [`skip_connection()`](https://norskregnesentral.github.io/shapr/reference/skip_connection.md).
  That is, we add the input \\X\\ to the outcome of each hidden layer,
  so the output becomes \\X + activation(WX + b)\\.

- vaeac.skip_conn_masked_enc_dec:

  Logical (default is `TRUE`). If `TRUE`, we apply concatenate skip
  connections between the layers in the masked encoder and decoder. The
  first layer of the masked encoder will be linked to the last layer of
  the decoder. The second layer of the masked encoder will be linked to
  the second to last layer of the decoder, and so on.

- vaeac.batch_normalization:

  Logical (default is `FALSE`). If `TRUE`, we apply batch normalization
  after the activation function. Note that if
  `vaeac.skip_conn_layer = TRUE`, then the normalization is applied
  after the inclusion of the skip connection. That is, we batch
  normalize the whole quantity \\X + activation(WX + b)\\.

- vaeac.paired_sampling:

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

- vaeac.masking_ratio:

  Numeric (default is `0.5`). Probability of masking a feature in the
  [`mcar_mask_generator()`](https://norskregnesentral.github.io/shapr/reference/mcar_mask_generator.md)
  (MCAR = Missing Completely At Random). The MCAR masking scheme ensures
  that `vaeac` model can do arbitrary conditioning as all coalitions
  will be trained. `vaeac.masking_ratio` will be overruled if
  `vaeac.mask_gen_coalitions` is specified.

- vaeac.mask_gen_coalitions:

  Matrix (default is `NULL`). Matrix containing the coalitions that the
  `vaeac` model will be trained on, see
  [`specified_masks_mask_generator()`](https://norskregnesentral.github.io/shapr/reference/specified_masks_mask_generator.md).
  This parameter is used internally in `shapr` when we only consider a
  subset of coalitions, i.e., when `n_coalitions` \\\<
  2^{n\_{\text{features}}}\\, and for group Shapley, i.e., when `group`
  is specified in
  [`explain()`](https://norskregnesentral.github.io/shapr/reference/explain.md).

- vaeac.mask_gen_coalitions_prob:

  Numeric array (default is `NULL`). Array of length equal to the height
  of `vaeac.mask_gen_coalitions` containing the probabilities of
  sampling the corresponding coalitions in `vaeac.mask_gen_coalitions`.

- vaeac.sigma_mu:

  Numeric (default is `1e4`). One of two hyperparameter values in the
  normal-gamma prior used in the masked encoder, see Section 3.3.1 in
  [Olsen et al.
  (2022)](https://www.jmlr.org/papers/volume23/21-1413/21-1413.pdf).

- vaeac.sigma_sigma:

  Numeric (default is `1e-4`). One of two hyperparameter values in the
  normal-gamma prior used in the masked encoder, see Section 3.3.1 in
  [Olsen et al.
  (2022)](https://www.jmlr.org/papers/volume23/21-1413/21-1413.pdf).

- vaeac.sample_random:

  Logical (default is `TRUE`). If `TRUE`, the function generates random
  Monte Carlo samples from the inferred generative distributions. If
  `FALSE`, the function use the most likely values, i.e., the mean and
  class with highest probability for continuous and categorical,
  respectively.

- vaeac.save_data:

  Logical (default is `FALSE`). If `TRUE`, then the data is stored
  together with the model. Useful if one are to continue to train the
  model later using
  [`vaeac_train_model_continue()`](https://norskregnesentral.github.io/shapr/reference/vaeac_train_model_continue.md).

- vaeac.log_exp_cont_feat:

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

- vaeac.which_vaeac_model:

  String (default is `best`). The name of the `vaeac` model (snapshots
  from different epochs) to use when generating the Monte Carlo samples.
  The standard choices are: `"best"` (epoch with lowest IWAE),
  `"best_running"` (epoch with lowest running IWAE, see
  `vaeac.running_avg_n_values`), and `last` (the last epoch). Note that
  additional choices are available if `vaeac.save_every_nth_epoch` is
  provided. For example, if `vaeac.save_every_nth_epoch = 5`, then
  `vaeac.which_vaeac_model` can also take the values `"epoch_5"`,
  `"epoch_10"`, `"epoch_15"`, and so on.

- vaeac.save_model:

  Boolean. If `TRUE` (default), the `vaeac` model will be saved either
  in a [`base::tempdir()`](https://rdrr.io/r/base/tempfile.html) folder
  or in a user specified location in `vaeac.folder_to_save_model`. If
  `FALSE`, then the paths to model and the model will will be deleted
  from the returned object from
  [`explain()`](https://norskregnesentral.github.io/shapr/reference/explain.md).

## Value

Named list of the default values `vaeac` extra parameter arguments
specified in this function call. Note that both
`vaeac.model_description` and `vaeac.folder_to_save_model` will change
with time and R session.

## Details

The `vaeac` model consists of three neural network (a full encoder, a
masked encoder, and a decoder) based on the provided `vaeac.depth` and
`vaeac.width`. The encoders map the full and masked input
representations to latent representations, respectively, where the
dimension is given by `vaeac.latent_dim`. The latent representations are
sent to the decoder to go back to the real feature space and provide a
samplable probabilistic representation, from which the Monte Carlo
samples are generated. We use the `vaeac` method at the epoch with the
lowest validation error (IWAE) by default, but other possibilities are
available by setting the `vaeac.which_vaeac_model` parameter. See [Olsen
et al. (2022)](https://www.jmlr.org/papers/volume23/21-1413/21-1413.pdf)
for more details.

## References

- [Olsen, L. H., Glad, I. K., Jullum, M., & Aas, K. (2022). Using
  Shapley values and variational autoencoders to explain predictive
  models with dependent mixed features. Journal of machine learning
  research, 23(213),
  1-51](https://www.jmlr.org/papers/volume23/21-1413/21-1413.pdf)

## Author

Lars Henry Berge Olsen
