cli_basic_startup <- function(internal,model){

  init_time <- internal$timing_list$init_time

  is_groupwise <- internal$parameters$is_groupwise
  approach <- internal$parameters$approach
  adaptive <- internal$parameters$adaptive
  n_shapley_values <- internal$parameters$n_shapley_values
  n_explain <- internal$parameters$n_explain
  saving_path <- internal$parameters$adaptive_arguments$saving_path

  feat_group_txt <- ifelse(is_groupwise,"group-wise","feature-wise")
  adaptive_txt <- ifelse(adaptive,"adaptive","non-adaptive")

  testing <- internal$parameters$testing

  if (isFALSE(testing)) {
    cli::cli_h1("Starting {.fn shapr::explain} at {round(init_time)}")
  }

  line1 <- "Model class: {.cls {class(model)}}"
  line2 <- "Approach: {.emph {approach}}"
  line3 <- "Adaptive estimation: {.emph {adaptive}}"
  line4 <- "Number of {.emph {feat_group_txt}} Shapley values: {n_shapley_values}"
  line5 <- "Number of observations to explain: {n_explain}"
  line6 <- ifelse(isFALSE(testing),"Computations (temporary) saved at: {.path {saving_path}}","")

  cli::cli_ul(c(line1,line2,line3,line4,line5,line6))

  if(isTRUE(adaptive)){
    msg <- "Adaptive computation started"
  } else {
    msg <- "Main computation started"
  }
  cli::cli_h2(cli::col_blue(msg))

}


cli_iter <- function(verbose,internal,iter){
  adaptive <- internal$parameters$adaptive

  if(!is.null(verbose) && isTRUE(adaptive)){
    cli::cli_h1("Iteration {iter}")
  }

  if("basic" %in% verbose){
    new_coal <- internal$iter_list[[iter]]$new_n_coalitions
    tot_coal <- internal$iter_list[[iter]]$n_coalitions
    all_coal <- 2^internal$parameters$n_shapley_values

    extra_msg <- ifelse(adaptive,", {new_coal} new","")

    msg <- paste0("Using {tot_coal} of {all_coal} coalitions",extra_msg,". ")

    cli::cli_alert_info(msg)
  }
}

