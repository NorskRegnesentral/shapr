compute_time <- function(timing_list) {
  timing_secs <- mapply(
    FUN = difftime,
    timing_list[-1],
    timing_list[-length(timing_list)],
    units = "secs"
  )

  timing_output <- list(
    init_time = timing_list$init,
    total_time_secs = sum(timing_secs),
    timing_secs = timing_secs
  )

  return(timing_output)
}
