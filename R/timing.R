#' Gathers and computes the timing of the different parts of the explain function.
#'
#' @inheritParams default_doc_export
#'
#' @export
#' @keywords internal
compute_time <- function(internal) {
  verbose <- internal$parameters$verbose

  main_timing_list <- internal$main_timing_list
  iter_timing_list <- internal$iter_timing_list


  main_timing_secs <- mapply(
    FUN = difftime,
    main_timing_list[-1],
    main_timing_list[-length(main_timing_list)],
    units = "secs"
  )

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

  total_time_secs <- main_timing_list[[length(main_timing_list)]] - main_timing_list[[1]]
  total_time_secs <- as.double(total_time_secs, units = "secs")


  timing_output <- list(
    init_time = main_timing_list[[1]],
    end_time = main_timing_list[[length(main_timing_list)]],
    total_time_secs = total_time_secs,
    overall_timing_secs = main_timing_secs,
    main_computation_timing_secs = iter_timing_secs_dt[]
  )
  internal$main_timing_list <- internal$iter_timing_list <- NULL

  return(timing_output)
}
