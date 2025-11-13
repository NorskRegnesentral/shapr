# Function that checks that the save folder exists and for a valid file name

Function that checks that the save folder exists and for a valid file
name

## Usage

``` r
vaeac_check_save_names(folder_to_save_model, model_description)
```

## Arguments

- folder_to_save_model:

  String (default is
  [`base::tempdir()`](https://rdrr.io/r/base/tempfile.html)). String
  specifying a path to a folder where the function is to save the fitted
  vaeac model. Note that the path will be removed from the returned
  [`explain()`](https://norskregnesentral.github.io/shapr/reference/explain.md)
  object if `vaeac.save_model = FALSE`.

- model_description:

  String (default is `make.names(Sys.time())`). String containing, e.g.,
  the name of the data distribution or additional parameter information.
  Used in the save name of the fitted model. If not provided, then a
  name will be generated based on
  [`base::Sys.time()`](https://rdrr.io/r/base/Sys.time.html) to ensure a
  unique name. We use
  [`base::make.names()`](https://rdrr.io/r/base/make.names.html) to
  ensure a valid file name for all operating systems.

## Value

The function does not return anything.

## Author

Lars Henry Berge Olsen
