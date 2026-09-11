# Function that extracts additional objects from the environment into the state list

The function extract the objects that we are going to save together with
the `vaeac` model to make it possible to train the model further and to
evaluate it. The environment should be the local environment inside the
[`vaeac_train_model_auxiliary()`](https://norskregnesentral.github.io/shapr/reference/vaeac_train_model_auxiliary.md)
function.

## Usage

``` r
vaeac_get_current_save_state(environment)
```

## Arguments

- environment:

  The [`base::environment()`](https://rdrr.io/r/base/environment.html)
  where the objects are stored.

## Value

List containing the values of `epoch`, `train_vlb`, `val_iwae`,
`val_iwae_running`, and the `state_dict()` of the vaeac model and
optimizer.

## Author

Lars Henry Berge Olsen
