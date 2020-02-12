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
#' @param joint_prob_dt data.table The first element in the list calculated using the \code{sim_true_Normal}.
#'
#' @return data.table
#'
#' @export

  cond_expec_new <- function(cond_list,explainer,x_test, prediction_zero,joint_prob_dt){

  feat_names <- colnames(explainer$x_train)
  dim <- length(feat_names)

  S_dt <- data.table(explainer$S)
  S_dt[, id := 0:(nrow(S_dt) - 1)]
  setnames(S_dt, c(feat_names, "id"))

  mat <- unique(x_test)
  mat <- mat[, lapply(.SD, as.factor), .SDcol = feat_names] # To be removed later
  mat[,rowid:=.I] # Adding identifyer to match on

  cond_expec_list <- list()
  cond_expec_list[[1]] <- NULL

  joint_prob_dt[,predict := predict_model(explainer$model, newdata = .SD), .SDcols = feat_names]

  for(i in 2:nrow(explainer$S)){
    col_names <- feat_names[as.logical(explainer$S[i, ])]
    cond_list[[i]] <- merge(cond_list[[i]],joint_prob_dt[,.(feat_comb_id,predict)],by="feat_comb_id")
    cond_list[[i]][, expected_value := predict * cond_prob]
    cond_expec_list[[i]] <- cond_list[[i]][, list(cond_expec=sum(expected_value)), by = col_names]
    tmp[[i]] <- cbind(cond_expec_list[[i]][mat, .(rowid,cond_expec), on = col_names,allow.cartesian=TRUE],
                      colnum = i-1)
  }
  tmp_dt <- rbindlist(tmp,use.names = T)

  final_dt <- dcast(tmp_dt, formula = "rowid~colnum", value.var = "cond_expec")
  x_test_id <- mat[x_test, on = feat_names]
  S_char_vec <- as.character(1:(nrow(explainer$S)-1))
  final_dt_x_test <- cbind("0"=prediction_zero, final_dt[x_test_id,..S_char_vec,on="rowid"])

  return(final_dt_x_test)
}
