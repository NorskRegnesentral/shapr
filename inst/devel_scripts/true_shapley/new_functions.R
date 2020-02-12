#' Function to calculate conditional expectations of the cutoff jointly Normal random variables
#' for the x_test observations
#'
#' @description
#'
#' @param cond_list List. Calculated using the \code{cond_prob} function.
#' @param explainer explainer object from shapr package.
#' @param x_test Matrix. Consists of all the test observations. Has the same dimension
#' as the number of joint Normal random variables calculated in \code{sim_true_Normal} function.
#' @param cond_expec_dt data.table. Calculated using the \code{cond_expec} function.
#' @param prediction_zero Numeric. Number to assigned to phi_0 in Shapley framework.
#'
#' @return data.table
#'
#' @export




  cond_expec_new <- function(cond_list,explainer,x_test, prediction_zero,joint_prob_dt){

# This approach does not work...
#  cond_dt <- rbindlist(cond_list[-1],idcol = "cond_id",use.names = T)
#  cond_dt[,cond_id := cond_id +1] # To refer to row in S below
#  cond_dt <- merge(cond_dt,joint_prob_dt[,.(feat_comb_id,predict)],by="feat_comb_id")
#  cond_dt[, expected_value := predict * cond_prob]
#  cond_expec_dt_new <- cond_dt[,sum(expected_value),by=.(feat_comb_id,cond_id)]
#  setnames(cond_expec_dt_new, "V1", "cond_expec")

  feat_names <- colnames(explainer$x_train)
  dim <- length(feat_names)

  cond_expec_list <- list()
  cond_expec_list[[1]] <- NULL

  joint_prob_dt[,predict := predict_model(explainer$model, newdata = .SD), .SDcols = feat_names]

  for(i in 2:nrow(explainer$S)){
    col_names <- feat_names[as.logical(explainer$S[i, ])]
    #cond_list[[i]][, predict := predict_model(explainer$model, newdata = .SD), .SDcols = feat_names]
    cond_list[[i]] <- merge(cond_list[[i]],joint_prob_dt[,.(feat_comb_id,predict)],by="feat_comb_id") # 50 sec-> 30 sec in for dim 10
    cond_list[[i]][, expected_value := predict * cond_prob]
    cond_expec_list[[i]] <- cond_list[[i]][, sum(expected_value), by = col_names]
  }
  cond_expec_dt <- rbindlist(l = cond_expec_list, use.names = T,fill = TRUE,)
  setnames(cond_expec_dt, "V1", "cond_expec")
  setcolorder(cond_expec_dt, c(feat_names, "cond_expec")) # "conditioned_on"

  S_dt <- data.table(explainer$S)
  S_dt[, id := 0:(nrow(S_dt) - 1)]
  setnames(S_dt, c(feat_names, "id"))

  mat <- unique(x_test)
  mat <- mat[, lapply(.SD, as.factor), .SDcol = feat_names] # To be removed later
#  setkey(mat) # Just to simplify now, remove later
  mat[,rowid:=.I]

  cond_expec_dt[, colnum := col_fun(.SD, S_dt), .SDcol = feat_names]

  # Check if it is possible to do the join the other way around
  select_cols <- c(feat_names, "i.cond_expec", "i.colnum")
  tmp <- list()
  for(i in 2:nrow(explainer$S)){
    col_names <- feat_names[as.logical(explainer$S[i, ])]
    cond_expec_dt_sub <- subset(cond_expec_dt,subset = colnum==i-1)
    tmp[[i]] <- cond_expec_dt_sub[mat, .(rowid,cond_expec,colnum), on = col_names,allow.cartesian=TRUE]
  }
  tmp_dt <- rbindlist(tmp,use.names = T)

  final_dt <- dcast(tmp_dt, formula = "rowid~colnum", value.var = "cond_expec")
  x_test_id <- mat[x_test, on = feat_names]
  S_char_vec <- as.character(1:(nrow(explainer$S)-1))
  final_dt_x_test <- cbind("0"=prediction_zero, final_dt[x_test_id,..S_char_vec,on="rowid"])
  return(final_dt_x_test)
}
