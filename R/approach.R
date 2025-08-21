#' Set up the framework for the chosen approach
#'
#' Different choices of `approach` take different (optional) parameters,
#' which are forwarded from [explain()].
#' See the \href{https://norskregnesentral.github.io/shapr/articles/general_usage.html}{general usage vignette}
#' for more information about the different approaches.
#'
#' @param ... Arguments passed to specific classes. See below.
#'
#' @inheritParams default_doc_export
#'
#' @return Updated internal object with the approach set up.
#'
#' @export
#' @keywords internal
#' @author Martin Jullum
setup_approach <- function(internal, ...) {
  verbose <- internal$parameters$verbose

  approach <- internal$parameters$approach

  iter <- length(internal$iter_list)
  X <- internal$iter_list[[iter]]$X

  needs_X <- c("regression_surrogate", "vaeac")

  run_now <- (isFALSE(any(needs_X %in% approach)) && isTRUE(is.null(X))) ||
    (isTRUE(any(needs_X %in% approach)) && isFALSE(is.null(X)))

  if (isFALSE(run_now)) { # Do nothing
    return(internal)
  } else {
    if ("progress" %in% verbose) {
      cli::cli_progress_step("Setting up approach(es)")
    }
    if ("vS_details" %in% verbose) {
      if ("vaeac" %in% approach) {
        pretrained_provided <- internal$parameters$vaeac.extra_parameters$vaeac.pretrained_vaeac_model_provided
        if (isFALSE(pretrained_provided)) {
          cli::cli_h2("Extra info about training/tuning the vaeac model")
        } else {
          cli::cli_h2("Extra info about the pretrained vaeac model")
        }
      }
    }

    this_class <- ""

    if (length(approach) > 1) {
      class(this_class) <- "combined"
    } else {
      class(this_class) <- approach
    }

    UseMethod("setup_approach", this_class)

    internal$timing_list$setup_approach <- Sys.time()
  }
}

#' @inheritParams default_doc_internal
#' @rdname setup_approach
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
#' @inheritParams default_doc_export
#'
#' @export
#' @keywords internal
#' @author Martin Jullum
prepare_data <- function(internal, index_features = NULL, ...) {
  iter <- length(internal$iter_list)

  X <- internal$iter_list[[iter]]$X

  # Extract the used approach(es)
  approach <- internal$parameters$approach

  # Auxiliary object so we can use the `UseMethod` dispatch
  this_class <- ""

  # Check if the user provided one or several approaches.
  if (length(approach) > 1) {
    # Pick the relevant approach from X, which lists the unique approach of the batch
    # matched by index_features
    class(this_class) <- X[id_coalition == index_features[1], approach]
  } else {
    # Only one approach for all coalition sizes
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
  c(
    "'independence' (not recommended)", "'ctree'", "'vaeac'",
    "'categorical'", "'regression_separate'", "'regression_surrogate'"
  )
}
