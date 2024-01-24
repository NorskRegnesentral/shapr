#' Set up the framework chosen approach
#'
#' The different choices of `approach` takes different (optional) parameters,
#' which are forwarded from [explain()].
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
#' @param ... Currently not used.
#'
#' @return A data.table containing simulated data used to estimate
#' the contribution function by Monte Carlo integration.
#'
#' @inheritParams default_doc_explain
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
    # Picks the relevant approach from the internal$objects$X table which list the unique approach of the batch
    # matches by index_features
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
  c("'independence' (not recommended)", "'ctree'", "'vaeac'", "'categorical'")
}
