format_info_iter <- function(iterative_info){

  n_iterations <- iterative_info[, .N]
  exact <- iterative_info[.N, exact]
  n_coalitions <- iterative_info[.N, n_coaltions]
  converged <- iterative_info[.N, converged]
  converged_sd <- iterative_info[.N, converged_sd]
  converged_max_iter <- iterative_info[.N, converged_max_iter]
  converged_max_n_coalitions  <- iterative_info[.N, converged_max_n_coalitions]

  overall_conv_measure <- iterative_info[.N, overall_conv_measure]

  n_coal_msg <- cli::format_inline("Number of coalitions used: {.val {n_coalitions}}", .envir = environment())

  conv_msg <- NULL
  if(isTRUE(converged)){
    conv_msg0 <- c("Iterative Shapley value estimation stopped after {.val {n_iterations}} iterations, due to: ")

    if(isTRUE(converged_sd)){
      conv_msg0 <- c(conv_msg0, "Standard deviation convergence criterion reached: {.val {overall_conv_measure}}")
    }
    if(isTRUE(converged_max_iter)){
      conv_msg0 <- c(conv_msg0, "Maxium number of iterations ({.val {n_iterations}}) reached. ")
    }
    if(isTRUE(converged_max_n_coalitions)){
      conv_msg0 <- c(conv_msg0, "Maxium number of coalitions ({.val {n_iterations}}) reached.")
    }

    conv_msg <- cli::format_inline(conv_msg0, .envir = environment())
  }

  return(list(n_coal_msg = n_coal_msg,
              conv_msg = conv_msg))
}


#' Summary method for shapr objects
#'
#' @param object A shapr object.
#' @param digits Integer. Number of digits to round numerical outputs.
#' @param ... Currently unused.
#'
#' @return Invisibly returns a named list of summary components.
#' @export
summary.shapr <- function(x, digits = 4, ...) {
  stopifnot(inherits(x, "shapr"))

  internal <- x$internal
  iter <- length(internal$iter_list)

  iterative <- internal$parameters$iterative
  converged_exact <- internal$iter_list[[iter]]$converged_exact

  cli::cli_h1("Summary of Shapley value explanation")

  # Retrieve all needed results
  results <- get_results(x)

  # Display basic shapr info
  formatted_info_basic <- format_info_basic(internal)

  cli::cli_ul(formatted_info_basic)

  # TODO: Should also add the number of coalitions here (also the max number)

  # TODO notes
  # make the convergence info more informative (see the format_info_iter func for inspiration)
  # Clean up the unused functions, and document those left
  # update explain_forecast with the new model_class stuff

  # Display convergence info
  formatted_convergence_info <- format_convergence_info(internal,iter)$formatted_msg

  cli::cli_h3("Convergence info")
  cli::cli_alert_success(formatted_convergence_info)

  # Display Shapley value res
  formatted_shapley_info <- format_shapley_info(internal, iter)

  if (converged_exact) {
    msg <- "Estimated Shapley values"
  } else {
    msg <- "Estimated Shapley values (sd in parantheses)"
  }

  cli::cli_h3(msg)

  # Using rlang::inform (bypassing cli-formatting) to print correctly
  # Cannot use print as it does not obey suppressMessages()
  rlang::inform(formatted_shapley_info)

  # TODO: Display MSE res???

  invisible(results)
}
