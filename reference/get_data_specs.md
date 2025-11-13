# Fetches feature information from a given data set

Fetches feature information from a given data set

## Usage

``` r
get_data_specs(x)
```

## Arguments

- x:

  data.frame or data.table. The data to extract feature information
  from.

## Value

A list with the following elements:

- labels:

  character vector with the feature names to compute Shapley values for

- classes:

  a named character vector with the labels as names and the class types
  as elements

- factor_levels:

  a named list with the labels as names and character vectors with the
  factor levels as elements (NULL if the feature is not a factor)

## Details

This function is used to extract the feature information to be checked
against the corresponding information extracted from the model and other
data sets. The function is only called internally

## Author

Martin Jullum
