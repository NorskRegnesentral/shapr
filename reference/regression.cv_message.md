# Produce message about which batch prepare_data is working on

Produce message about which batch prepare_data is working on

## Usage

``` r
regression.cv_message(
  regression.results,
  regression.grid,
  n_cv = 10,
  current_comb
)
```

## Arguments

- regression.results:

  The results of the CV procedures.

- regression.grid:

  Object containing the hyperparameter values.

- n_cv:

  Integer (default is 10) specifying the number of CV hyperparameter
  configurations to print.

- current_comb:

  Integer vector. The current combination of features, passed to
  verbosity printing function.

## Author

Lars Henry Berge Olsen
