# capability.R — which approaches work on which datasets, and which extra
# packages each approach needs. Used to skip invalid / unavailable runs.

# Datasets:
#   numeric     -> all features numeric
#   mixed       -> numeric + factor features
#   categorical -> all features factor
#
# Factor-supporting approaches (from shapr::get_factor_approaches()):
#   arf, categorical, ctree, regression_separate, regression_surrogate, vaeac,
#   independence.
# Numeric-only approaches: gaussian, copula, empirical, timeseries.
# The categorical approach requires ALL features to be factors.
approach_capability <- function() {
  list(
    independence          = c("numeric", "mixed", "categorical"),
    gaussian              = c("numeric"),
    copula                = c("numeric"),
    empirical             = c("numeric"),
    timeseries            = c("numeric"),
    ctree                 = c("numeric", "mixed", "categorical"),
    arf                   = c("numeric", "mixed", "categorical"),
    vaeac                 = c("numeric", "mixed", "categorical"),
    regression_separate   = c("numeric", "mixed", "categorical"),
    regression_surrogate  = c("numeric", "mixed", "categorical"),
    categorical           = c("categorical")
  )
}

# TRUE if `approach` can run on `dataset`.
approach_supports <- function(approach, dataset) {
  caps <- approach_capability()
  if (is.null(caps[[approach]])) {
    return(FALSE)
  }
  return(dataset %in% caps[[approach]])
}

# Extra R packages required by each approach beyond shapr's base deps.
approach_dependencies <- function() {
  list(
    arf                  = "arf",
    ctree                = "partykit",
    vaeac                = "torch",
    regression_separate  = "parsnip",
    regression_surrogate = "parsnip"
  )
}

# Returns NA_character_ if all deps for `approach` are available, otherwise a
# short string naming the first missing requirement (used to mark runs skipped).
missing_dependency <- function(approach) {
  deps <- approach_dependencies()[[approach]]
  if (is.null(deps)) {
    return(NA_character_)
  }
  for (pkg in deps) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      return(pkg)
    }
    if (pkg == "torch" && !torch::torch_is_installed()) {
      return("torch-backend")
    }
  }
  return(NA_character_)
}
