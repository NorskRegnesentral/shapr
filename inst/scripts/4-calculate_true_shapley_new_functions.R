
marg_probNEW <- function(joint_prob_dt, explainer){

  feat_names <- colnames(explainer$x_train)

  ## compute all marginal probabilities
  marg_list <- list()
  marg_list[[1]] <- NA
  for(i in 2:nrow(explainer$S)){
    col_names <- feat_names[as.logical(explainer$S[i, ])]

    mat <- joint_prob_dt[, .(marg_prob = sum(joint_prob)), by = col_names]

    marg_list[[i]] <- mat
  }
  return(marg_list)
}

cond_probNEW <- function(marg_list, joint_prob_dt, explainer){

  feat_names <- colnames(explainer$x_train)

  cond_list <- list()
  cond_list[[1]] <- data.frame(marg_prob = 1, joint_prob = 1, id = joint_prob_dt$id, cond_prob = 1)

  for(i in 2:nrow(explainer$S)){
    col_names <- feat_names[as.logical(explainer$S[i, ])]

    mat0 <- marg_list[[i]]
    setkeyv(mat0, col_names)
    setkeyv(joint_prob_dt, col_names)
    cond_list[[i]] <- merge(mat0, joint_prob_dt, all.x = TRUE)
    cond_list[[i]][, cond_prob := joint_prob / marg_prob]
    cond_list[[i]][,(feat_names):= NULL] # To save memory
    setkeyv(cond_list[[i]], "id")
  }
  cond_dt <- rbindlist(cond_list, id = 'id_combination')

  joint_prob_dt0 <- copy(joint_prob_dt)
  joint_prob_dt0[, joint_prob := NULL]

  cond_dt <- cond_dt[joint_prob_dt0, on = 'id']

  return(cond_dt)
}

col_fun <- function(tbl, S_dt){
  dim <- ncol(tbl)
  v <- tbl[, 1:dim]
  v_S <- data.table(ifelse(is.na(v), 0, 1))
  colnum <- S_dt[v_S, .(id), on = names(v_S)]
  return(colnum)
}

cond_expecNEW <- function(cond_dt, explainer, x_test, prediction_zero){

  cnms <- colnames(explainer$x_train)
  setkeyv(cond_dt, c("id_combination", "id"))

  mat <- unique(x_test)
  mat <- mat[, lapply(.SD, as.factor), .SDcol = cnms] # To be removed later
  mat[, id := .I] # Adding identifyer to match on

  ## New
  S_dt <- data.table(explainer$S)
  S_dt[S_dt == 0] <- NA
  S_dt[, id_combination := 1:nrow(S_dt)]
  setnames(S_dt, c(paste0(cnms, "conditioned"), "id_combination"))

  cols <- c(cnms, "id_combination")
  cond_dt_sub <- cond_dt[, ..cols]
  cond_dt_charac <- cond_dt_sub[, lapply(.SD, as.character)]
  cond_dt_num <- cond_dt_charac[, lapply(.SD, as.numeric)]

  tmp <- cond_dt_num[S_dt, on = 'id_combination']

  cols2 <- paste0(cnms, "conditioned")
  tmp_comb <- tmp[, ..cnms] * tmp[, ..cols2]
  setnames(tmp_comb, cols2)

  setkeyv(cond_dt, "id_combination")
  cond_dt_new <- cbind(cond_dt, tmp_comb)

  ##
  cond_dt_new[, predict := predict_model(explainer$model, newdata = .SD), .SDcols = cnms]
  setkey(cond_dt_new, "id")

  cols3 <- c("id_combination", cols2)

  cond_expec_dt <- cond_dt_new[, .(cond_expec = sum(predict * cond_prob)), by = cols3]
  setkey(cond_expec_dt, "id_combination")

  cond_expec_comb = cond_dt_new[cond_expec_dt, on = cols3]

  cond_expec_sub = cond_expec_comb[cond_expec_comb[, id_combination != 1]]

  final_dt <- dcast(cond_expec_sub, formula = id ~ id_combination, value.var = "cond_expec")

  x_test_id <- mat[x_test, on = cnms]
  S_char_vec <- as.character(2:(nrow(explainer$S)))
  final_dt_x_test <- cbind("1" = prediction_zero, final_dt[x_test_id, ..S_char_vec, on = "id"])
  return(final_dt_x_test)

}

create_exact_joint_probNEW <- function(mu, Sigma, explainer, cutoff, algorithm = mvtnorm::GenzBretz(),
                                       mc.cores = 16){

  feat_names <- colnames(explainer$x_train)
  dim <- length(feat_names)
  no_categories <- length(cutoff) - 1

  all_x_list <- list()
  for(i in 1:dim){
    all_x_list[[i]] <- 1:no_categories
  }
  all_x_dt <- do.call(CJ, all_x_list)
  names(all_x_dt) <- feat_names

  all_x_dt[, (feat_names) := lapply(.SD, as.factor),.SDcols = feat_names]

  # Lists with vectors containing the lower and upper combinations
  upper_func <- function(x,cutoff){
    cutoff[as.numeric(x)+1]
  }

  lower_func <- function(x, cutoff){
    cutoff[as.numeric(x)]
  }

  upper_dt <- all_x_dt[, lapply(.SD, upper_func, cutoff = cutoff), .SDcols = feat_names]
  lower_dt <- all_x_dt[, lapply(.SD, lower_func, cutoff = cutoff), .SDcols = feat_names]

  upper_dt_list = as.list(as.data.table(t(upper_dt)))
  lower_dt_list = as.list(as.data.table(t(lower_dt)))

  corr <- cov2cor(Sigma)



  all_probs <- parallel::mcmapply(FUN = mvtnorm::pmvnorm,
                                  lower = lower_dt_list,
                                  upper = upper_dt_list,
                                  MoreArgs = list(mean = mu,
                                                  corr = corr,
                                                  algorithm = algorithm),
                                  mc.cores = mc.cores)

  all_probs <- all_probs/sum(all_probs)


  all_x_dt[, joint_prob := all_probs]

  all_x_dt[, id_all := .I]

  # setkeyv(all_x_dt, rev(feat_names)) # To get same ordering as previous version
  setkeyv(all_x_dt, "id_all")
  return(all_x_dt)
}


create_exact_joint_probNEW2 <- function(mu, Sigma, explainer, cutoff, algorithm = mvtnorm::GenzBretz(),
                                       mc.cores = 16){

  feat_names <- colnames(explainer$x_train)
  dim <- length(feat_names)
  no_categories <- length(cutoff) - 1

  all_x_list <- list()
  for(i in 1:dim){
    all_x_list[[i]] <- 1:no_categories
  }
  all_x_dt <- do.call(CJ, all_x_list)
  names(all_x_dt) <- feat_names

  all_x_dt[, (feat_names) := lapply(.SD, as.factor),.SDcols = feat_names]

  # Lists with vectors containing the lower and upper combinations
  upper_func <- function(x,cutoff){
    cutoff[as.numeric(x)+1]
  }

  lower_func <- function(x, cutoff){
    cutoff[as.numeric(x)]
  }

  upper_dt <- all_x_dt[, lapply(.SD, upper_func, cutoff = cutoff), .SDcols = feat_names]
  lower_dt <- all_x_dt[, lapply(.SD, lower_func, cutoff = cutoff), .SDcols = feat_names]

  upper_dt_list = as.list(as.data.table(t(upper_dt)))
  lower_dt_list = as.list(as.data.table(t(lower_dt)))

  corr <- cov2cor(Sigma)



  all_probs <- parallel::mcmapply(FUN = mvtnorm::pmvnorm,
                                  lower = lower_dt_list,
                                  upper = upper_dt_list,
                                  MoreArgs = list(mean = mu,
                                                  corr = corr,
                                                  algorithm = algorithm),
                                  mc.cores = mc.cores)

  all_probs <- all_probs/sum(all_probs)


  all_x_dt[, joint_prob := all_probs]

  all_x_dt[, id := .I] # THIS IS THE ONLY DIFFERENCE THEN ABOVE

  # setkeyv(all_x_dt, rev(feat_names)) # To get same ordering as previous version
  setkeyv(all_x_dt, "id") # AND HERE
  return(all_x_dt)
}
