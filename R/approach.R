#' Set up the framework chosen approach
#'
#' The different choices of `approach` takes different (optional) parameters,
#' which are forwarded from [explain()].
#'
#' @param internal List. Holds all parameters, data, functions and computed
#' objects used within [explain()].
#' The list contains one or more of the elements `parameters`, `data`, `objects`, `output`.
#'
#' @param ... `approach`-specific arguments. See below.
#'
#' @inheritParams default_doc_explain
#'
#' @export
setup_approach <- function(internal, ...) {
  approach <- internal$parameters$approach

  this_class <- ""

  if (length(approach) > 1) {
    class(this_class) <- "combined"
  } else {
    class(this_class) <- approach
  }

  UseMethod("setup_approach", this_class)
}

#' @inheritParams default_doc
#' @export
setup_approach.combined <- function(internal, ...) {
  org_approach <- internal$parameters$approach
  unique_approaches <- unique(org_approach)

  for (i in unique_approaches) {
    internal$parameters$approach <- i
    internal <- setup_approach(internal, ...)
  }
  internal$parameters$approach <- org_approach

  return(internal)
}

#' Generate data used for predictions and Monte Carlo integration
#'
#' @note
#' There is currently a warning when calling `roxygen2::roxygenise()`.
#' Warning message:
#' inheritParams failed in topic "prepare_data", "prepare_data.categorical", "prepare_data.copula",
#' "prepare_data.ctree", "prepare_data.empirical", "prepare_data.gaussian",
#' "prepare_data.independence", "prepare_data.timeseries", and "prepare_data.vaeac".
#' ✖ All parameters are already documented; none remain to be inherited.
#'
#' This occurred after Lars updated the code as he now documents all parameters here. This was not
#' the case before, as the roxygen had not been updated. Can solve this by removing, e.g.,
#' `param ... Currently not used.` below, or remove rdname from the subfunctions and rather
#  use inherateparams.
#'
#'
#' @param internal List. Holds all parameters, data, functions and computed objects used
#' within \code{\link[=explain]{explain()}}. The list contains one or more of the elements
#' `parameters`, `data`, `objects`, `output`.
#' @param index_features Positive integer vector. Specifies the indices of combinations to
#' apply to the present method. `NULL` means all combinations. Only used internally.
#'
#' @param ... Currently not used.
#'
#' @return A data.table containing simulated data used to estimate
#' the contribution function by Monte Carlo integration.
#'
#' @export
#' @keywords internal
prepare_data <- function(internal, index_features = NULL, ...) {

  # Extract the used approach(es)
  approach <- internal$parameters$approach

  # Auxiliary object such that we can use the `UseMethod` function
  this_class <- ""

  # Check if the user provided one or several approaches.
  if (length(approach) > 1) {
    # Figure out which approach we should use for the provided `index_features` (i.e., coalition indices).
    # We can use that the function `create_S_batch_new()` in setup_computation.R, which creates
    # the batches of coalitions, always ensures that the index_features (i.e., the coalitions) in
    # each batch of them are using the same approach. Meaning that we can figure out which approach
    # the first value in `index_features` is connected to, and then use that approach to generate
    # the Monte Carlo samples data.
    class(this_class) <- internal$objects$X[id_combination == index_features[1], approach]
  } else {
    # Only one approach for all coalitions sizes
    class(this_class) <- approach
  }

  # Use the `prepare_data` function for the correct approach
  UseMethod("prepare_data", this_class)
}

#' @keywords internal
insert_defaults <- function(internal, defaults) {
  par_names <- names(defaults)

  overwrite_names <- par_names[!(par_names %in% names(internal$parameters))]

  internal$parameters <- append(internal$parameters, defaults[overwrite_names])

  return(internal)
}

#' @keywords internal
get_factor_approaches <- function() {
  c("'independence' (not recommended)", "'ctree'", "'categorical'")
}
