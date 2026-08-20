# Check the parameters that are sent to [`rsample::vfold_cv()`](https://rsample.tidymodels.org/reference/vfold_cv.html)

Check that `regression.vfold_cv_para` is either NULL or a named list
that only contains recognized parameters.

## Usage

``` r
regression.check_vfold_cv_para(regression.vfold_cv_para)
```

## Arguments

- regression.vfold_cv_para:

  Either `NULL` (default) or a named list containing the parameters to
  be sent to
  [`rsample::vfold_cv()`](https://rsample.tidymodels.org/reference/vfold_cv.html).
  See the regression vignette for several examples.

## Author

Lars Henry Berge Olsen
