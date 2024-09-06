

save_results <- function(internal){

  saving_path <- internal$parameters$adaptive_arguments$saving_path


  # Modify name for the new file
  filename <- basename(saving_path)
  dirname <- dirname(saving_path)
  filename_copy <- paste0("new_",filename)
  saving_path_copy <- file.path(dirname,filename_copy)

  # Save the results to a new location, then delete old and rename for safe code interruption

  internal$data <- NULL # Not saving data due to potentially large file size
  saveRDS(internal, saving_path_copy)
  if (file.exists(saving_path)) file.remove(saving_path)
  file.rename(saving_path_copy, saving_path)


}
