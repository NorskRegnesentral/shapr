# capability.R — which approaches work on which datasets, and which extra
# packages each approach needs. Used to skip invalid / unavailable runs.
#
# Dataset FAMILIES (the capability/model/generation logic keys off the family):
#   numeric     -> all features numeric
#   mixed       -> numeric + factor features
#   categorical -> all features factor
#
# There are several concrete `mixed` datasets (mixed_fc_fl, mixed_fc_ml,
# mixed_mc_fl, mixed_mc_ml — few/many categorical features x few/many levels),
# all of which belong to the `mixed` family. `dataset_family()` maps a concrete
# dataset name to its family.

# Map a concrete dataset name to its family.
dataset_family <- function(dataset) {
  if (startsWith(dataset, "mixed")) {
    return("mixed")
  }
  return(dataset)
}

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

# TRUE if `approach` can run on `dataset` (concrete name; family is resolved).
approach_supports <- function(approach, dataset) {
  caps <- approach_capability()
  if (is.null(caps[[approach]])) {
    return(FALSE)
  }
  return(dataset_family(dataset) %in% caps[[approach]])
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
# `extra_pkgs` lets a caller add variant-specific deps (e.g. glmnet for a
# regression variant).
missing_dependency <- function(approach, extra_pkgs = character(0)) {
  deps <- c(approach_dependencies()[[approach]], extra_pkgs)
  if (length(deps) == 0) {
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
