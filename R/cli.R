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
  return(NULL)
}


#' Print Startup Messages with CLI
#'
#' @inheritParams default_doc_export
#' @inheritParams explain
#'
#' @return No return value (but prints startup messages with cli)
#'
#' @export
#' @keywords internal
cli_startup <- function(internal, verbose) {
  approach <- internal$parameters$approach
  iterative <- internal$parameters$iterative
  regression.model <- internal$parameters$regression.model

  # Get the basic shapr information to display
  formatted_line_vec <- format_info_basic(internal)

  if ("basic" %in% verbose) {
    cli::cli_h2("Explanation overview")
    cli::cli_ul(formatted_line_vec)
  }

  if ("vS_details" %in% verbose) {
    if (any(c("regression_surrogate", "regression_separate") %in% approach)) {
      reg_desc <- paste0(capture.output(regression.model), collapse = "\n")
      cli::cli_h3("Additional details about the regression model")
      cli::cli_text(reg_desc)
    }
  }

  if ("basic" %in% verbose) {
    if (isTRUE(iterative)) {
      msg <- "Iterative computation started"
    } else {
      msg <- "Main computation started"
    }
    cli::cli_h2(cli::col_blue(msg))
  }
}

#' Internal function to extract a vector with formatted info about the shapr call
#'
#' To be used by both [cli_startup()] and [shapr::summary.shapr()]
#'
#' @inheritParams default_doc_internal
#' @keywords internal
format_info_basic <- function(internal) {
  is_groupwise <- internal$parameters$is_groupwise
  model_class <- internal$parameters$model_class
  approach <- internal$parameters$approach
  iterative <- internal$parameters$iterative
  n_MC_samples <- internal$parameters$n_MC_samples
  regression <- internal$parameters$regression
  n_shapley_values <- internal$parameters$n_shapley_values
  n_explain <- internal$parameters$n_explain
  saving_path <- internal$parameters$output_args$saving_path
  causal_ordering_names_string <- internal$parameters$causal_ordering_names_string
  max_n_coalitions_causal <- internal$parameters$max_n_coalitions_causal
  confounding_string <- internal$parameters$confounding_string
  asymmetric <- internal$parameters$asymmetric
  confounding <- internal$parameters$confounding
  testing <- internal$parameters$testing
  group <- internal$parameters$group
  group_lags <- internal$parameters$group_lags

  feat_group_txt <- ifelse(is_groupwise, "group-wise", "feature-wise")
  iterative_txt <- ifelse(iterative, "Iterative", "Non-iterative")
  vS_est_class <- ifelse(regression, "Regression", "Monte Carlo integration")

  line_vec <- c()
  line_vec <- c(line_vec, "Model class: {.cls {model_class}}")
  line_vec <- c(line_vec, "v(S) estimation class: {.val {num_str(vS_est_class)}}")
  line_vec <- c(line_vec, "Approach: {.val {num_str(approach)}}")
  line_vec <- c(line_vec, "Procedure: {.val {num_str(iterative_txt)}}")

  if (isFALSE(regression)) {
    line_vec <- c(line_vec, "Number of Monte Carlo integration samples: {.val {n_MC_samples}}")
  }

  line_vec <- c(line_vec, "Number of {.emph {feat_group_txt}} Shapley values: {.val {n_shapley_values}}")
  if (isTRUE(is_groupwise) && !isTRUE(group_lags)) { # using !isTRUE since isFALSE(NULL)=FALSE
    # format the group list with name of each component followed by the string vector in curly braces
    string <- sapply(names(group), function(name) {
      paste0("{.emph ", name, "}: {{{{", paste0("{.val ", group[[name]], "}", collapse = ", "), "}}}}")
    })

    line_vec <- c(line_vec, paste0("Feature groups: ", paste0(string, collapse = "; ")))
  }
  line_vec <- c(line_vec, "Number of observations to explain: {.val {n_explain}}")

  if (isTRUE(asymmetric)) {
    line_vec <- c(line_vec, "Number of asymmetric coalitions: {.val {max_n_coalitions_causal}}")
  }
  if (isTRUE(asymmetric) || !is.null(confounding)) {
    line_vec <- c(line_vec, "Causal ordering: {.emph {causal_ordering_names_string}}")
  }
  if (!is.null(confounding)) {
    line_vec <- c(line_vec, "Components with confounding: {.emph {confounding_string}}")
  }
  if (isFALSE(testing)) {
    line_vec <- c(line_vec, "Computations (temporary) saved at: {.path {saving_path}}")
  }

  formatted_line_vec <- sapply(line_vec, cli::format_inline, .envir = environment(), USE.NAMES = FALSE)

  return(formatted_line_vec)
}

#' Internal function to extract some extra formatted info about the shapr call
#'
#' To be used in [shapr::summary.shapr()]
#'
#' @inheritParams default_doc_internal
#' @keywords internal
format_info_extra <- function(internal) {
  iter <- length(internal$iter_list)

  n_coalitions <- internal$iter_list[[iter]]$n_coalitions
  n_shapley_values <- internal$parameters$n_shapley_values

  msg <- "Number of coalitions used: {.val {n_coalitions}} (of total {.val {2^n_shapley_values}})"

  formatted_msg <- cli::format_inline(msg, .envir = environment())

  return(formatted_msg)
}

#' Print Messages in Compute_vS with CLI
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
    cli::cli_progress_step("Computing v(S)")
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

#' Print Messages in Iterative Procedure with CLI
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
    cli::cli_h1("Iteration {.val {iter}}")
  }

  if ("basic" %in% verbose) {
    new_coal <- internal$iter_list[[iter]]$new_n_coalitions
    tot_coal <- internal$iter_list[[iter]]$n_coalitions
    all_coal <- ifelse(asymmetric, internal$parameters$max_n_coalitions, 2^internal$parameters$n_shapley_values)

    extra_msg <- ifelse(iterative, ", {.val {new_coal}} new", "")

    msg <- paste0("Using {.val {tot_coal}} of {.val {all_coal}} coalitions", extra_msg, ". ")

    cli::cli_alert_info(msg)
  }
}

#' Internal function to extract formatted info about the (current) convergence state of the shapr call
#'
#' To be used by both [print_iter()] and [shapr::summary.shapr]
#'
#' @inheritParams default_doc_internal
#' @inheritParams default_doc_export
#' @keywords internal
format_convergence_info <- function(internal, iter) {
  converged <- internal$iter_list[[iter]]$converged
  converged_exact <- internal$iter_list[[iter]]$converged_exact
  converged_sd <- internal$iter_list[[iter]]$converged_sd
  converged_max_iter <- internal$iter_list[[iter]]$converged_max_iter
  converged_max_n_coalitions <- internal$iter_list[[iter]]$converged_max_n_coalitions
  overall_conv_measure <- internal$iter_list[[iter]]$overall_conv_measure
  n_coal_next_iter_factor <- internal$iter_list[[iter]]$n_coal_next_iter_factor
  current_n_coalitions <- internal$iter_list[[iter]]$n_sampled_coalitions + 2
  est_remaining_coal_samp <- internal$iter_list[[iter]]$est_remaining_coal_samp
  est_required_coal_samp <- internal$iter_list[[iter]]$est_required_coal_samp

  convergence_tol <- internal$parameters$iterative_args$convergence_tol

  conv_nice <- signif(overall_conv_measure, 2)
  n_coal_next_iter_factor_nice <- as.numeric(format(signif(n_coal_next_iter_factor * 100, 2), scientific = FALSE))

  tol_nice <- ifelse(
    !is.null(convergence_tol),
    as.numeric(format(signif(convergence_tol, 2), scientific = FALSE)),
    tol_nice <- NA
  )

  if (isFALSE(converged)) {
    next_new_n_coalitions <- internal$iter_list[[iter + 1]]$new_n_coalitions

    # No convergence
    msg <- "Not converged after {.val {current_n_coalitions}} coalitions:\n"

    if (!is.null(convergence_tol)) {
      msg <- paste0(
        msg,
        "Current convergence measure: {.val {conv_nice}} [needs {.val {tol_nice}}]\n",
        "Estimated remaining coalitions: {.val {est_remaining_coal_samp}}\n",
        "(Conservatively) adding about {.val {n_coal_next_iter_factor_nice}}% of that ",
        "({.val {next_new_n_coalitions}} coalitions) in the next iteration."
      )
    }
  } else {
    msg <- paste0(
      "Iterative Shapley value estimation stopped at {.val {current_n_coalitions}} coalitions ",
      "after {.val {iter}} iterations, due to:\n"
    )
    if (isTRUE(converged_exact)) {
      msg <- paste0(
        msg,
        "All ({.val {current_n_coalitions}}) coalitions used!\n"
      )
    }
    if (isTRUE(converged_sd)) {
      msg <- paste0(
        msg,
        "Standard deviation convergence threshold ({.val {tol_nice}}) reached: {.val {conv_nice}}!\n"
      )
    }
    if (isTRUE(converged_max_iter)) {
      msg <- paste0(
        msg,
        "Maximum number of iterations ({.val {iter}}) reached!\n"
      )
    }
    if (isTRUE(converged_max_n_coalitions)) {
      msg <- paste0(
        msg,
        "Maximum number of coalitions ({.val {current_n_coalitions}}) reached!\n"
      )
    }
  }
  formatted_msg <- cli::format_inline(msg, .envir = environment())
  return(formatted_msg)
}

#' Internal function to extract the formatted Shapley value table
#'
#' To be used by both [print_iter()] and [shapr::summary.shapr()]
#'
#' @inheritParams default_doc_internal
#' @inheritParams default_doc_export
#' @keywords internal
format_shapley_info <- function(internal, iter, digits = 2L) {
  converged_exact <- internal$iter_list[[iter]]$converged_exact

  dt_shapley_est0 <- internal$iter_list[[iter]]$dt_shapley_est
  dt_shapley_sd0 <- internal$iter_list[[iter]]$dt_shapley_sd

  shap_names_with_none <- c("none", internal$parameters$shap_names)
  other_cols <- setdiff(names(dt_shapley_est0), shap_names_with_none)

  dt_shapley_est <- dt_shapley_est0[, shap_names_with_none, with = FALSE]
  dt_shapley_sd <- dt_shapley_sd0[, shap_names_with_none, with = FALSE]

  # Printing the current Shapley values

  # Formatting with the custom format_round to avoid small number-issues and ensure OS-consistency
  dt_est_formatted <- dt_shapley_est[, lapply(.SD, format_round, digits = digits)]
  dt_sd_formatted <- dt_shapley_sd[, lapply(.SD, format_round, digits = digits)]

  if (converged_exact) {
    print_dt0 <- dt_est_formatted
  } else {
    print_dt0 <- dt_est_formatted[, Map(function(x, y) paste0(x, " (", y, ")"), .SD, dt_sd_formatted)]
  }

  print_dt <- cbind(
    dt_shapley_est0[, other_cols, with = FALSE],
    print_dt0
  )

  names(print_dt) <- names(dt_shapley_est0)

  output <- capture.output(print(print_dt[]))

  ret <- paste(output, collapse = "\n")

  return(ret)
}

#' Print Iterative Information
#'
#' @inheritParams default_doc_export
#'
#' @return No return value (but prints iterative information)
#'
#' @export
#' @keywords internal
print_iter <- function(internal) {
  verbose <- internal$parameters$verbose
  iter <- length(internal$iter_list) - 1 # This function is called after the preparation of the next iteration

  converged <- internal$iter_list[[iter]]$converged
  converged_exact <- internal$iter_list[[iter]]$converged_exact

  if ("convergence" %in% verbose) {
    formatted_convergence_info <- format_convergence_info(internal, iter)

    cli::cli_h3("Convergence info")
    if (isTRUE(converged)) {
      cli::cli_alert_success(formatted_convergence_info)
    } else {
      cli::cli_alert_info(formatted_convergence_info)
    }
  }

  if ("shapley" %in% verbose) {
    formatted_shapley_info <- format_shapley_info(internal, iter)

    if (isTRUE(converged)) {
      msg <- "Final "
    } else {
      msg <- "Current "
    }

    if (converged_exact) {
      msg <- paste0(msg, "estimated Shapley values")
    } else {
      msg <- paste0(msg, "estimated Shapley values (sd)")
    }

    # Send it to rlang::inform (bypassing cli-formatting) to print correctly
    # Cannot use print as it does not obey suppressMessages()
    rlang::inform(paste0("\n", msg, "\n", formatted_shapley_info))
  }
}


#' Convert a character to a numeric class
#'
#' To be used in cli calls like `cli::cli_text("{.val {shapr:::num_str('12.10')}}")` to format character strings
#' that typically represent a number as if it were numeric. May also be used with strings not representing a number.
#'
#' @param x Character. A single character that represents a number, or a vector of characters.
#'
#' @return A numeric class object with the value of the string.
#'
#' @keywords internal
num_str <- function(x) {
  structure(x, class = "numeric")
}


#' Format numbers with rounding
#'
#' @param x Numeric vector. The numbers to format.
#' @inheritParams default_doc_export
#'
#' @return Character vector. The formatted numbers.
#'
#' @keywords internal
format_round <- function(x, digits = 2L) {
  format(round_manual(x, digits = digits), justify = "right")
}


#' Round numbers to the specified number of decimal places
#'
#' This function rounds numbers to the specified number of decimal places
#' using a manual method that avoids the typical rounding issues in R which may vary
#' across different OS.
#' @param x Numeric vector. The numbers to round.
#' @param digits Integer. The number of digits to round to. Defaults 0.
#'
#' @return Numeric vector. The rounded numbers.
#'
#' @keywords internal
round_manual <- function(x, digits = 0L) {
  posneg <- sign(x)
  z <- abs(x) * 10^digits
  z <- z + 0.5 + sqrt(.Machine$double.eps)
  z <- trunc(z)
  z <- z / 10^digits
  z * posneg
}
