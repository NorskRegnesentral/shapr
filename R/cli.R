#' Create a header topline with cli
#'
#' @inheritParams setup
#'
#' @return No return value (but prints header with cli unless `verbose` is `NULL`)
#'
#' @keywords internal
cli_topline <- function(verbose, testing, init_time, type, is_python) {
  msg00 <- ifelse(type == "regular", "Starting {.fn shapr::explain}", "Starting {.fn shapr::explain_forecast}")
  msg0 <- ifelse(is_python, paste0(msg00, " from Python"), msg00)
  msg <- ifelse(testing, msg0, paste0(msg0, " at {.val {round(init_time)}}"))
  if (!is.null(verbose)) {
    cli::cli_h1(msg)
  }
}


#' Printing startup messages with cli
#'
#' @param model_class String.
#' Class of the model as a string
#' @inheritParams default_doc_export
#' @inheritParams explain
#'
#' @return No return value (but prints startup messages with cli)
#'
#' @export
#' @keywords internal
cli_startup <- function(internal, model_class, verbose) {
  init_time <- internal$timing_list$init_time

  is_groupwise <- internal$parameters$is_groupwise
  approach <- internal$parameters$approach
  iterative <- internal$parameters$iterative
  n_shapley_values <- internal$parameters$n_shapley_values
  n_explain <- internal$parameters$n_explain
  saving_path <- internal$parameters$output_args$saving_path
  causal_ordering_names_string <- internal$parameters$causal_ordering_names_string
  max_n_coalitions_causal <- internal$parameters$max_n_coalitions_causal
  confounding_string <- internal$parameters$confounding_string


  feat_group_txt <- ifelse(is_groupwise, "group-wise", "feature-wise")
  iterative_txt <- ifelse(iterative, "iterative", "non-iterative")

  testing <- internal$parameters$testing
  asymmetric <- internal$parameters$asymmetric
  confounding <- internal$parameters$confounding


  line_vec <- "Model class: {.cls {model_class}}"
  line_vec <- c(line_vec, "Approach: {.emph {approach}}")
  line_vec <- c(line_vec, "Iterative estimation: {.emph {iterative}}")
  line_vec <- c(line_vec, "Number of {.emph {feat_group_txt}} Shapley values: {n_shapley_values}")
  line_vec <- c(line_vec, "Number of observations to explain: {n_explain}")
  if (isTRUE(asymmetric)) {
    line_vec <- c(line_vec, "Number of asymmetric coalitions: {max_n_coalitions_causal}")
  }
  if (isTRUE(asymmetric) || !is.null(confounding)) {
    line_vec <- c(line_vec, "Causal ordering: {causal_ordering_names_string}")
  }
  if (!is.null(confounding)) {
    line_vec <- c(line_vec, "Components with confounding: {confounding_string}")
  }
  if (isFALSE(testing)) {
    line_vec <- c(line_vec, "Computations (temporary) saved at: {.path {saving_path}}")
  }

  if ("basic" %in% verbose) {
    cli::cli_h2("Explanation overview")
    cli::cli_ul(line_vec)
  }

  if ("vS_details" %in% verbose) {
    if (any(c("regression_surrogate", "regression_separate") %in% approach)) {
      reg_desc <- paste0(capture.output(internal$parameters$regression.model), collapse = "\n")
      cli::cli_h3("Additional details about the regression model")
      cli::cli_text(reg_desc)
    }
  }

  if ("basic" %in% verbose) {
    if (isTRUE(iterative)) {
      msg <- "iterative computation started"
    } else {
      msg <- "Main computation started"
    }
    cli::cli_h2(cli::col_blue(msg))
  }
}

#' Printing messages in compute_vS with cli
#'
#' @inheritParams default_doc_export
#' @inheritParams explain
#'
#' @return No return value (but prints compute_vS messages with cli)
#'
#' @export
#' @keywords internal
cli_compute_vS <- function(internal) {
  verbose <- internal$parameters$verbose
  approach <- internal$parameters$approach

  if ("progress" %in% verbose) {
    cli::cli_progress_step("Computing vS")
  }
  if ("vS_details" %in% verbose) {
    if ("regression_separate" %in% approach) {
      tuning <- internal$parameters$regression.tune
      if (isTRUE(tuning)) {
        cli::cli_h2("Extra info about the tuning of the regression model")
      }
    }
  }
}

#' Printing messages in iterative procedure with cli
#'
#' @inheritParams default_doc_export
#' @inheritParams explain
#'
#' @return No return value (but prints iterative messages with cli)
#'
#' @export
#' @keywords internal
cli_iter <- function(verbose, internal, iter) {
  iterative <- internal$parameters$iterative
  asymmetric <- internal$parameters$asymmetric

  if (!is.null(verbose) && isTRUE(iterative)) {
    cli::cli_h1("Iteration {iter}")
  }

  if ("basic" %in% verbose) {
    new_coal <- internal$iter_list[[iter]]$new_n_coalitions
    tot_coal <- internal$iter_list[[iter]]$n_coalitions
    all_coal <- ifelse(asymmetric, internal$parameters$max_n_coalitions, 2^internal$parameters$n_shapley_values)

    extra_msg <- ifelse(iterative, ", {new_coal} new", "")

    msg <- paste0("Using {tot_coal} of {all_coal} coalitions", extra_msg, ". ")

    cli::cli_alert_info(msg)
  }
}
