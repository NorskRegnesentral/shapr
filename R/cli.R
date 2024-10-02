setup_message <- function(internal,model){
  is_groupwise <- internal$parameters$is_groupwise
  approach <- internal$parameters$approach
  adaptive <- internal$parameters$adaptive
  n_shapley_values <- internal$parameters$n_shapley_values
  n_explain <- internal$parameters$n_explain

  feat_group_txt <- ifelse(is_groupwise,"group-wise","feature-wise")
  adaptive_txt <- ifelse(adaptive,"adaptive","non-adaptive")



  msg_txt <- c("Explaining {n_explain} observations from model class {.cls {class(model)}} with ",
               "{n_shapley_values} {feat_group_txt} Shapley values, using the {.emph {approach}} approach{?es}, ",
               "{.emph {adaptive_txt}} estimation.\n",
               "All input parameters are OK.")
  cli::cli_alert_success(paste0(msg_txt,collapse = ""),wrap=TRUE)
}
