# Function to printout a training summary for the `vaeac` model

Function to printout a training summary for the `vaeac` model

## Usage

``` r
vaeac_print_train_summary(best_epoch, best_epoch_running, last_state)
```

## Arguments

- best_epoch:

  Positive integer. The epoch with the lowest validation error.

- best_epoch_running:

  Positive integer. The epoch with the lowest running validation error.

- last_state:

  The state list (i.e., the saved `vaeac` object) of `vaeac` model at
  the epoch with the lowest IWAE.

## Value

This function only prints out a message.

## Author

Lars Henry Berge Olsen
