#' Gather and Compute the Timing of the Different Parts of the Explain Function
#'
#' @inheritParams default_doc_export
#'
#' @return A list with reformatted timing information.
#'
#' @export
#' @keywords internal
compute_time <- function(internal) {
  verbose <- internal$parameters$verbose

  main_timing_list <- internal$main_timing_list
  iter_timing_list <- internal$iter_timing_list

  overall_timing_secs <- mapply(
    FUN = difftime,
    main_timing_list[-1],
    main_timing_list[-length(main_timing_list)],
    units = "secs"
  )

  total_time_secs <- main_timing_list[[length(main_timing_list)]] - main_timing_list[[1]]
  total_time_secs <- as.double(total_time_secs, units = "secs")

  timing_summary <- data.table(
    init_time = main_timing_list[[1]],
    end_time = main_timing_list[[length(main_timing_list)]],
    total_time_secs = total_time_secs
  )
  timing_summary[, total_time_str := get_nice_time(total_time_secs)]

  iter_timing_secs_list <- list()
  for (i in seq_along(iter_timing_list)) {
    iter_timing_secs_list[[i]] <- as.list(mapply(
      FUN = difftime,
      iter_timing_list[[i]][-1],
      iter_timing_list[[i]][-length(iter_timing_list[[i]])],
      units = "secs"
    ))
  }
  iter_timing_secs_dt <- data.table::rbindlist(iter_timing_secs_list, fill = TRUE)
  iter_timing_secs_dt[, total := rowSums(.SD, na.rm = TRUE)]
  iter_timing_secs_dt[, iter := .I]
  data.table::setcolorder(iter_timing_secs_dt, "iter")

  timing_output <- list(
    summary = timing_summary,
    overall_timing_secs = overall_timing_secs,
    main_computation_timing_secs = iter_timing_secs_dt[]
  )
  internal$main_timing_list <- internal$iter_timing_list <- NULL

  return(timing_output)
}


#' Reformat seconds into a human-readable format.
#'
#' @param secs Numeric vector of seconds to reformat.
#'
#' @return A character string representing the time in a human-readable format.
#'
#' @keywords internal
get_nice_time <- function(secs) {
  hours <- floor(secs / 3600)
  minutes <- floor((secs %% 3600) / 60)
  seconds <- round(secs %% 60, 1)

  parts <- c()

  if (hours >= 1) {
    parts <- c(parts, cli::pluralize("{hours} hour{?s}"))
  }
  if (minutes >= 1) {
    parts <- c(parts, cli::pluralize("{minutes} minute{?s}"))
  }
  parts <- c(parts, cli::pluralize("{seconds} second{?s}"))

  nice_time <- paste(parts, collapse = ", ")
  return(nice_time)
}
