cli_startup <- function(internal, model, verbose) {
  init_time <- internal$timing_list$init_time

  is_groupwise <- internal$parameters$is_groupwise
  approach <- internal$parameters$approach
  adaptive <- internal$parameters$adaptive
  n_shapley_values <- internal$parameters$n_shapley_values
  n_explain <- internal$parameters$n_explain
  saving_path <- internal$parameters$adaptive_arguments$saving_path
  causal_ordering_names_string <- internal$parameters$causal_ordering_names_string
  max_n_coalitions_causal <- internal$parameters$max_n_coalitions_causal
  confounding_string <- internal$parameters$confounding_string


  feat_group_txt <- ifelse(is_groupwise, "group-wise", "feature-wise")
  adaptive_txt <- ifelse(adaptive, "adaptive", "non-adaptive")

  testing <- internal$parameters$testing
  asymmetric <- internal$parameters$asymmetric
  confounding <- internal$parameters$confounding


  line_vec <- "Model class: {.cls {class(model)}}"
  line_vec <- c(line_vec, "Approach: {.emph {approach}}")
  line_vec <- c(line_vec, "Adaptive estimation: {.emph {adaptive}}")
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
    if (isFALSE(testing)) {
      cli::cli_h1("Starting {.fn shapr::explain} at {round(init_time)}")
    }
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
    if (isTRUE(adaptive)) {
      msg <- "Adaptive computation started"
    } else {
      msg <- "Main computation started"
    }
    cli::cli_h2(cli::col_blue(msg))
  }
}


cli_iter <- function(verbose, internal, iter) {
  adaptive <- internal$parameters$adaptive
  asymmetric <- internal$parameters$asymmetric

  if (!is.null(verbose) && isTRUE(adaptive)) {
    cli::cli_h1("Iteration {iter}")
  }

  if ("basic" %in% verbose) {
    new_coal <- internal$iter_list[[iter]]$new_n_coalitions
    tot_coal <- internal$iter_list[[iter]]$n_coalitions
    all_coal <- ifelse(asymmetric, internal$parameters$max_n_coalitions, 2^internal$parameters$n_shapley_values)

    extra_msg <- ifelse(adaptive, ", {new_coal} new", "")

    msg <- paste0("Using {tot_coal} of {all_coal} coalitions", extra_msg, ". ")

    cli::cli_alert_info(msg)
  }
}
