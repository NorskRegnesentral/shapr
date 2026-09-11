# Function that checks for access to CUDA

Function that checks for access to CUDA

## Usage

``` r
vaeac_check_cuda(cuda, verbose)
```

## Arguments

- cuda:

  Logical (default is `FALSE`). If `TRUE`, then the `vaeac` model will
  be trained using cuda/GPU. If
  [`torch::cuda_is_available()`](https://torch.mlverse.org/docs/reference/cuda_is_available.html)
  is `FALSE`, we fall back to using the CPU. Using a GPU for smaller
  tabular dataset often do not improve the efficiency. See
  [`vignette("installation", package = "torch")`](https://torch.mlverse.org/docs/articles/installation.html)
  fo help to enable running on the GPU (only Linux and Windows).

- verbose:

  String vector or NULL. Controls verbosity (printout detail level) via
  one or more of `"basic"`, `"progress"`, `"convergence"`, `"shapley"`
  and `"vS_details"`. `"basic"` (default) displays basic information
  about the computation and messages about parameters/checks.
  `"progress"` displays where in the calculation process the function
  currently is. `"convergence"` displays how close the Shapley value
  estimates are to convergence (only when `iterative = TRUE`).
  `"shapley"` displays intermediate Shapley value estimates and standard
  deviations (only when `iterative = TRUE`), and the final estimates.
  `"vS_details"` displays information about the v(S) estimates, most
  relevant for
  `approach %in% c("regression_separate", "regression_surrogate", "vaeac")`.
  `NULL` means no printout. Any combination can be used, e.g.,
  `verbose = c("basic", "vS_details")`.

## Value

The function does not return anything.

## Author

Lars Henry Berge Olsen
