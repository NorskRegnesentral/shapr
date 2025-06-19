#' Extract components from a shapr object
#'
#' @param x A `shapr` object
#' @param what Character vector specifying one or more components to extract.
#' Options:
#' "shapley_est", "shapley_sd", "pred_explain",
#' "MSEv", "MSEv_explicand", "MSEv_coalition",
#' "iterative_info", "iterative_shapley_est", "iterative_shapley_sd",
#' "saving_path", "timing", "parameters",
#' "x_train", "x_explain", "model", "dt_vS", "dt_samp_for_vS"
#' @param ... Not used
#'
#' @return If a single component is requested, returns that object.
#' If multiple are requested, returns a named list.
#' @export
get_results <- function(x, what = c(
                          "shapley_est", "shapley_sd",
                          "pred_explain",
                          "MSEv", "MSEv_explicand", "MSEv_coalition",
                          "iterative_info", "iterative_shapley_est", "iterative_shapley_sd",
                          "saving_path",
                          "timing",
                          "parameters", "x_train", "x_explain", "model",
                          "dt_vS", "dt_samp_for_vS"
                        ), ...) {
  stopifnot(inherits(x, "shapr"))

  allowed <- c(
    "shapley_est", "shapley_sd",
    "pred_explain",
    "MSEv", "MSEv_explicand", "MSEv_coalition",
    "iterative_info", "iterative_shapley_est", "iterative_shapley_sd",
    "saving_path",
    "timing",
    "parameters", "x_train", "x_explain", "model",
    "dt_vS", "dt_samp_for_vS"
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
      shapley_est = x$shapley_values_est,
      shapley_sd = x$shapley_values_sd,
      pred_explain = x$pred_explain,
      MSEv = x$MSEv$MSEv,
      MSEv_explicand = x$MSEv$MSEv_explicand,
      MSEv_coalition = x$MSEv$MSEv_coalition,
      iterative_info = x$iterative_results$iter_info_dt,
      iterative_shapley_est = x$iterative_results$iterative_shapley_est,
      iterative_shapley_sd = x$iterative_results$iterative_shapley_sd,
      saving_path = x$internal$saving_path,
      timing = x$timing,
      parameters = x$internal$parameters,
      x_train = x$internal$data$x_train,
      x_explain = x$internal$data$x_explain,
      model = x$internal$model,
      dt_vS = x$internal$output$dt_vS,
      dt_samp_for_vS = x$internal$output$dt_samp_for_vS
    )
  })
  names(res) <- what

  if (length(res) == 1) res[[1]] else res
}
