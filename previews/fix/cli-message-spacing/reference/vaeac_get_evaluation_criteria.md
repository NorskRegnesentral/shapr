# Extract the Training VLB and Validation IWAE from a List of Explanations Objects Using the `vaeac` Approach

Extract the Training VLB and Validation IWAE from a List of Explanations
Objects Using the `vaeac` Approach

## Usage

``` r
vaeac_get_evaluation_criteria(explanation_list)
```

## Arguments

- explanation_list:

  A list of
  [`explain()`](https://norskregnesentral.github.io/shapr/reference/explain.md)
  objects applied to the same data, model, and `vaeac` must be the used
  approach. If the entries in the list is named, then the function use
  these names. Otherwise, it defaults to the approach names (with
  integer suffix for duplicates) for the explanation objects in
  `explanation_list`.

## Value

A data.table containing the training VLB, validation IWAE, and running
validation IWAE at each epoch for each vaeac model.

## Author

Lars Henry Berge Olsen
