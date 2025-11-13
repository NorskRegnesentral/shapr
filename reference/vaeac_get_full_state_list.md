# Function that extracts the state list objects from the environment

\#' @description The function extract the objects that we are going to
save together with the `vaeac` model to make it possible to train the
model further and to evaluate it. The environment should be the local
environment inside the
[`vaeac_train_model_auxiliary()`](https://norskregnesentral.github.io/shapr/reference/vaeac_train_model_auxiliary.md)
function.

## Usage

``` r
vaeac_get_full_state_list(environment)
```

## Arguments

- environment:

  The [`base::environment()`](https://rdrr.io/r/base/environment.html)
  where the objects are stored.

## Value

List containing the values of `norm_mean`, `norm_std`,
`model_description`, `folder_to_save_model`, `n_train`, `n_features`,
`one_hot_max_sizes`, `epochs`, `epochs_specified`,
`epochs_early_stopping`, `early_stopping_applied`,
`running_avg_n_values`, `paired_sampling`, `mask_generator_name`,
`masking_ratio`, `mask_gen_coalitions`, `mask_gen_coalitions_prob`,
`val_ratio`, `val_iwae_n_samples`, `n_vaeacs_initialize`,
`epochs_initiation_phase`, `width`, `depth`, `latent_dim`,
`activation_function`, `lr`, `batch_size`, `skip_conn_layer`,
`skip_conn_masked_enc_dec`, `batch_normalization`, `cuda`,
`train_indices`, `val_indices`, `save_every_nth_epoch`, `sigma_mu`,
`sigma_sigma`, `feature_list`, `col_cat_names`, `col_cont_names`,
`col_cat`, `col_cont`, `cat_in_dataset`, `map_new_to_original_names`,
`map_original_to_new_names`, `log_exp_cont_feat`, `save_data`,
`verbose`, `seed`, and `vaeac_save_file_names`.

## Author

Lars Henry Berge Olsen
