
#' @export
compute_vS <- function(internal,model,method="lapply"){


  if(method=="lapply"){
    ret <- future.apply::future_lapply(X = internal$objects$S_batch,
                                       FUN = run_batch, #TODO: Change name on run_batch
                                       internal = internal,
                                       model = model,
                                       future.seed = internal$parameters$seed)
  }else{

    keep_samp_for_vS <- internal$parameters$keep_samp_for_vS
    ret <- list()

    for(i in seq_along(internal$objects$S_batch)){
      S <- internal$objects$S_batch[[i]]
      dt <- batch_prepare_vS(S = S,internal = internal) # Make it optional to store and return the dt_list
      compute_preds(dt,internal,model) # Updating dt by reference

      dt_vS <- compute_MCint(dt)

      if(keep_samp_for_vS){
        ret[[i]] <- list(dt_vS = dt_vS,dt_samp_for_vS=dt)
      } else {
        ret[[i]] <- copy(dt_vS)
      }
    }
  }

  return(ret)
}

#' @keywords internal
run_batch <- function(S,internal,model){

  keep_samp_for_vS <- internal$parameters$keep_samp_for_vS

  dt <- batch_prepare_vS(S = S,internal = internal) # Make it optional to store and return the dt_list
  compute_preds(dt,internal,model) # Updating dt by reference

  dt_vS <- compute_MCint(dt)

  if(keep_samp_for_vS){
    return(list(dt_vS = dt_vS,dt_samp_for_vS=dt))
  } else {
    return(dt_vS = dt_vS)
  }
}

#' @keywords internal
batch_prepare_vS <- function(S,internal){

  max_id_combination <- internal$parameters$n_combinations
  x_explain <- internal$data$x_explain
  n_explain <- internal$parameters$n_explain

  # TODO: Check what is the fastest approach to deal with the last observation.
  # Not doing this for the largest id combination (should check if this is faster or slower, actually)
  # An alternative would be to delete rows from the dt which is provided by prepare_data.
  if(!(max_id_combination %in% S)){
    dt <- prepare_data(internal, index_features = S) #TODO: Need to handle the need for model for the AIC-versions here (skip for Python)
  } else {
    S <- S[S!=max_id_combination]
    dt <- prepare_data(internal, index_features = S)
    dt_max <- data.table(x_explain,id_combination=max_id_combination,w=1,id=seq_len(n_explain))
    dt <- rbind(dt,dt_max)
    setkey(dt,id,id_combination)
  }
  return(dt)
}

#' @keywords internal
compute_preds <- function(dt, internal,model) {

  id_combination <- p_hat <- NULL # due to NSE notes in R CMD check

  # Setup
  feature_names <- internal$parameters$feature_list$labels
  prediction_zero <- internal$parameters$prediction_zero

  # Predictions
  if (!all(dt[, unique(id_combination)] == 1)) { # Avoid warnings when predicting with empty newdata
    dt[id_combination != 1, p_hat := internal$funcs$predict_model(model, newdata = .SD), .SDcols = feature_names]
  }
  dt[id_combination == 1, p_hat := prediction_zero]

  return(dt)
}

compute_MCint <- function(dt) {

  w <- k <- p_hat <- NULL # due to NSE notes in R CMD check

  # Calculate contributions
  dt_res <- dt[, .(k = sum((p_hat * w) / sum(w))), .(id, id_combination)]
  data.table::setkeyv(dt_res, c("id", "id_combination"))
  dt_mat <- data.table::dcast(dt_res, id_combination ~ id, value.var = "k")
  #dt_mat[, id_combination := NULL]

  dt_mat
}

