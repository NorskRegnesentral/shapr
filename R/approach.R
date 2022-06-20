#' @keywords internal
setup_approach <- function(internal,...){

  approach <- internal$parameters$approach

  this_class <- ""

  if (length(approach) > 1) {
    class(this_class) <- "combined"
  }  else {
    class(this_class) <- approach
  }

  UseMethod("setup_approach", this_class)

}

#' @export
setup_approach.combined <- function(internal,...){

  org_approach <- internal$parameters$approach
  unique_approaches <- unique(org_approach)

  for(i in unique_approaches){
    internal$parameters$approach <- i
    internal <- setup_approach(internal,...)
  }
  internal$parameters$approach <- org_approach

  return(internal)
}

#' Generate data used for predictions
#'
#' @param x Explainer object. See \code{\link{explain}} for more information.
#'
#' @param seed Positive integer. If \code{NULL} the seed will be inherited from the calling environment.
#'
#' @param index_features Positive integer vector. Specifies the indices of combinations to apply to the present method.
#' \code{NULL} means all combinations. Only used internally.
#'
#' @param ... Currently not used.
#'
#' @return A data.table containing simulated data passed to prediction()}.
#'
#' @export
#' @keywords internal
prepare_data <- function(internal, ...) {
  this_class <- ""
  class(this_class) <- internal$parameters$approach
  UseMethod("prepare_data", this_class)
}

#' @keywords internal
insert_defaults <- function(internal,defaults){
  par_names <- names(defaults)

  overwrite_names <- par_names[!(par_names %in% names(internal$parameters))]

  internal$parameters <- append(internal$parameters,defaults[overwrite_names])

  return(internal)
}





