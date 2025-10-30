#' Save the Intermediate Results to Disk
#'
#' @inheritParams default_doc_export
#'
#' @return No return value (but saves the intermediate results to disk)
#'
#' @export
#' @keywords internal
save_results <- function(internal) {
  saving_path <- internal$parameters$output_args$saving_path

  # Modify name for the new file
  filename <- basename(saving_path)
  dirname <- dirname(saving_path)
  filename_copy <- paste0("new_", filename)
  saving_path_copy <- file.path(dirname, filename_copy)

  # Save the results to a new location, then delete old and rename for safe code interruption

  # Saving parameters and iter_list
  saveRDS(internal[c("parameters", "iter_list")], saving_path_copy)
  if (file.exists(saving_path)) file.remove(saving_path)
  file.rename(saving_path_copy, saving_path)
  NULL
}
