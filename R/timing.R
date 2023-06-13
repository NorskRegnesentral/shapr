compute_time <- function(output) {
  if (output$internal$parameters$timing) {
    timing_secs <- mapply(
      FUN = difftime,
      output$internal$timing[-1],
      output$internal$timing[-length(internal$timing)],
      units = "secs"
    )

    timing_list <- list(
      init_time = output$internal$timing$init,
      total_time_secs = sum(timing_secs),
      timing_secs = timing_secs
    )
  } else {
    timing_list <- NULL
  }

  output$internal$timing <- NULL

  output$internal$timing <- timing_list
}
