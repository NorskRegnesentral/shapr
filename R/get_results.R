#' Extract Components from a Shapr Object
#'
#' @param x A `shapr` object
#' @param what Character vector specifying one or more components to extract.
#' Options:
#' "calling_function", "proglang", "approach", "shapley_est", "shapley_sd", "pred_explain",
#' "MSEv", "MSEv_explicand", "MSEv_coalition",
#' "iterative_info", "iterative_shapley_est", "iterative_shapley_sd",
#' "saving_path",
#' "timing_summary", "timing_details",
#' "parameters", "x_train", "x_explain",
#' "dt_vS", "dt_samp_for_vS",
#' "dt_used_coalitions", "dt_valid_causal_coalitions", "dt_coal_samp_info".
#' The default is to return all components. See details for what each component contains.
#' @param ... Not used
#' @details The function extracts a full suite of information related to the computation of the Shapley values from
#' a `shapr` object.
#' The allowed characters in `what` provides information as follows:
#' \describe{
#'   \item{`calling_function`}{Name of function called to create the `shapr` object,
#'   (`explain()` or `explain_forecast()`).}
#'   \item{`proglang`}{Programming language used to initiate the computations (`R` or `Python`).}
#'   \item{`approach`}{Approach used to estimate the conditional expectations.}
#'   \item{`shapley_est`}{data.table with the estimated Shapley values.}
#'   \item{`shapley_sd`}{data.table with the standard deviation of the Shapley values reflecting the uncertainty
#'   in the coalition sampling part of the kernelSHAP procedure.}
#'   \item{`pred_explain`}{Numeric vector with the predictions for the explained observations.}
#'   \item{`MSEv/MSEv_explicand/MSEv_coalition`}{Data.tables with MSEv evaluation criterion values overall/
#'   per explicand/per coalition.
#'   Smaller values indicate better estimates of `v(S)`.
#'   See the
#'   \href{https://norskregnesentral.github.io/shapr/articles/general_usage.html#msev-evaluation-criterion
#'   }{MSEv evaluation section in the general usage vignette for details}.}
#'   \item{`iterative_info`}{Data.table with information about the iterative estimation procedure.}
#'   \item{`iterative_shapley_est/iterative_shapley_sd`}{Data.tables with the estimated Shapley values/their
#'   standard deviation for each iteration (when using the iterative estimation procedure).}
#'   \item{`saving_path`}{Character string with the path where the (temporary) results are saved.}
#'   \item{`timing_summary`}{Data.table with one row and three columns: `init_time` and `end_time` give the time stamps
#'    for the start and end of the computation, respectively, while `total_time_secs` gives the total time in seconds
#'    for the full computation.}
#'   \item{`timing_details`}{List containing timing information for the different parts of the computation.
#'   `summary` contains the information from `timing_summary`.
#'   `overall_timing_secs` gives the time spent on the different parts of the explanation computation.
#'   `main_computation_timing_secs` further decomposes the main computation time into the different parts of the
#'   computation for each iteration of the iterative estimation routine, if used.}
#'   \item{`parameters`}{List with the parameters used in the computation.}
#'   \item{`x_train/x_explain`}{Data.tables with the training data used in the computation/observations to explain.}
#'   \item{`dt_vS`}{Data.table with the contribution function (`v(S)`) estimates for each coalition.}
#'   \item{`dt_samp_for_vS`}{Data.table with the samples used in the Monte Carlo estimation of the contribution function
#'   (`v(S)`).
#'   This is only available if `output_args_default$keep_samp_for_vS = TRUE` (defaults to FALSE) in [explain()].}
#'   \item{`dt_used_coalitions`}{Data.table with an overview of the coalitions used in the computation.}
#'   \item{`dt_valid_causal_coalitions`}{Data.table with the valid causal coalitions used in the computation.}
#'   \item{`dt_coal_samp_info`}{Data.table with information related to the coalition sampling procedure being used.}
#' }
#'
#' Note that the [shapr::summary.shapr()] function provides a nicely formatted printout with the most important
#' information, to then invisibly return the output of the present function.
#' The [shapr::print.shapr()] allows direct printing of the main results.
#' @return If a single component is requested, returns that object.
#' If multiple are requested, returns a named list.
#'
#' @export
get_results <- function(x, what = c(
                          "calling_function", "proglang", "approach",
                          "shapley_est", "shapley_sd",
                          "pred_explain",
                          "MSEv", "MSEv_explicand", "MSEv_coalition",
                          "iterative_info", "iterative_shapley_est", "iterative_shapley_sd",
                          "saving_path",
                          "timing_summary", "timing_details",
                          "parameters", "x_train", "x_explain",
                          "dt_vS", "dt_samp_for_vS",
                          "dt_used_coalitions", "dt_valid_causal_coalitions", "dt_coal_samp_info"
                        ), ...) {
  stopifnot(inherits(x, "shapr"))

  allowed <- c(
    "calling_function", "proglang", "approach",
    "shapley_est", "shapley_sd",
    "pred_explain",
    "MSEv", "MSEv_explicand", "MSEv_coalition",
    "iterative_info", "iterative_shapley_est", "iterative_shapley_sd",
    "saving_path",
    "timing_summary", "timing_details",
    "parameters", "x_train", "x_explain",
    "dt_vS", "dt_samp_for_vS",
    "dt_used_coalitions", "dt_valid_causal_coalitions", "dt_coal_samp_info"
  )

  unknown <- setdiff(what, allowed)
  if (length(unknown) > 0) {
    cli::cli_abort(c(
      "x" = "Unknown component{?s} {.val {unknown}} in argument {.arg what}!",
      "i" = "Allowed components are: {.val {allowed}}."
    ))
  }

  res <- lapply(what, function(w) {
    switch(w,
      calling_function = ifelse(x$internal$parameters$type == "regular", "explain", "explain_forecast"),
      proglang = ifelse(x$internal$parameters$is_python, "Python", "R"),
      approach = x$internal$parameters$approach,
      shapley_est = x$shapley_values_est,
      shapley_sd = x$shapley_values_sd,
      pred_explain = x$pred_explain,
      MSEv = x$MSEv$MSEv,
      MSEv_explicand = x$MSEv$MSEv_explicand,
      MSEv_coalition = x$MSEv$MSEv_coalition,
      iterative_info = x$iterative_results$iter_info_dt,
      iterative_shapley_est = x$iterative_results$dt_iter_shapley_est,
      iterative_shapley_sd = x$iterative_results$dt_iter_shapley_sd,
      saving_path = x$saving_path,
      timing_summary = x$timing$summary,
      timing_details = x$timing,
      parameters = x$internal$parameters,
      x_train = x$internal$data$x_train,
      x_explain = x$internal$data$x_explain,
      dt_vS = x$internal$output$dt_vS,
      dt_samp_for_vS = x$internal$output$dt_samp_for_vS,
      dt_used_coalitions = x$internal$objects$X,
      dt_valid_causal_coalitions = x$internal$objects$dt_valid_causal_coalitions,
      dt_coal_samp_info = x$internal$objects$dt_coal_samp_info
    )
  })
  names(res) <- what

  if (length(res) == 1) res[[1]] else res
}
