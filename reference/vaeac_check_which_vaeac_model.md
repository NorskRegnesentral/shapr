# Function that checks for valid `vaeac` model name

Function that checks for valid `vaeac` model name

## Usage

``` r
vaeac_check_which_vaeac_model(
  which_vaeac_model,
  epochs,
  save_every_nth_epoch = NULL
)
```

## Arguments

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

- epochs:

  Positive integer (default is `100`). The number of epochs to train the
  final vaeac model. This includes `epochs_initiation_phase`, where the
  default is `2`.

- save_every_nth_epoch:

  Positive integer (default is `NULL`). If provided, then the vaeac
  model after every `save_every_nth_epoch`th epoch will be saved.

## Value

The function does not return anything.

## Author

Lars Henry Berge Olsen
