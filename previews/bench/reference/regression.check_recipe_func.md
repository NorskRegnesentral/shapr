# Check `regression.recipe_func`

Check that regression.recipe_func is a function that returns the RHS of
the formula for arbitrary feature name inputs.

## Usage

``` r
regression.check_recipe_func(regression.recipe_func, x_explain)
```

## Arguments

- regression.recipe_func:

  Either `NULL` (default) or a function that that takes in a
  [`recipes::recipe()`](https://recipes.tidymodels.org/reference/recipe.html)
  object and returns a modified
  [`recipes::recipe()`](https://recipes.tidymodels.org/reference/recipe.html)
  with potentially additional recipe steps. See the regression vignette
  for several examples. Note, to make it easier to call
  [`explain()`](https://norskregnesentral.github.io/shapr/reference/explain.md)
  from Python, the `regression.recipe_func` can also be a string
  containing an R function. For example,
  `"function(recipe) return(recipes::step_ns(recipe, recipes::all_numeric_predictors(), deg_free = 2))"`
  is also a valid input. It is essential to include the package prefix
  if the package is not loaded.

- x_explain:

  Data.table with the features of the observation whose predictions
  ought to be explained (test data).

## Author

Lars Henry Berge Olsen
