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




cond_expec_new <- function(cond_list,explainer,x_test, prediction_zero){

  feat_names <- colnames(explainer$x_train)
  dim <- length(feat_names)


  cond_expec_list <- list()
  cond_expec_list[[1]] <- NULL

  # NOTE: Could speed up this by adding a group to each unique conditioning combination in cond_list
  # (should be done where cond_list is created, call this group1), and then do the prediction and just once beforehand,
  # merge that with an rbindlist of cond_list based on features, compute hte expected_value like below
  # and do the sum(expected_value) by (group1,group_id_from_rbindlist_of_cond_list)
  # Currently this takes 49 seconds  seconds for dim 10 --
  # Priority 2
  start = proc.time()
  for(i in 2:nrow(explainer$S)){
    col_names <- feat_names[as.logical(explainer$S[i, ])]
    cond_list[[i]][, predict := predict_model(explainer$model, newdata = .SD), .SDcols = feat_names]
    cond_list[[i]][, expected_value := predict * cond_prob]
    tmp <- cond_list[[i]][, sum(expected_value), by = col_names]
    # tmp[, conditioned_on := paste(col_names, collapse = ", ")]
    cond_expec_list[[i]] <- tmp
  }
  cond_expec_dt <- rbindlist(l = cond_expec_list, use.names = T,fill = TRUE,)
  setnames(cond_expec_dt, "V1", "cond_expec")
  setcolorder(cond_expec_dt, c(feat_names, "cond_expec")) # "conditioned_on"

  end = proc.time()
  end-start

  S_dt <- data.table(explainer$S)
  S_dt[, id := 0:(nrow(S_dt) - 1)]
  setnames(S_dt, c(feat_names, "id"))

  mat <- unique(x_test)
  mat <- mat[, lapply(.SD, as.factor), .SDcol = feat_names] # To be removed later
  setkey(mat) # Just to simplify now, remove later
  mat[,rowid:=.I]

  cond_expec_dt[, colnum := col_fun(.SD, S_dt), .SDcol = feat_names]


  # Check if it is possible to do the join the other way around
  select_cols <- c(feat_names, "i.cond_expec", "i.colnum")
  tmp <- list()
### MJ START TESTING ####
  tmp1 <- list()

  start <- proc.time()
  for(i in 2:nrow(explainer$S)){
    col_names <- feat_names[as.logical(explainer$S[i, ])]
    cond_expec_dt_sub <- subset(cond_expec_dt,subset = colnum==i-1)

    tmp1[[i]] <- mat[cond_expec_dt_sub, .(rowid,i.cond_expec,i.colnum), on = col_names,by=.EACHI]
  }
  end <- proc.time()
  end-start

  tmp2 <- list()
  start <- proc.time()
  for(i in 2:nrow(explainer$S)){
    col_names <- feat_names[as.logical(explainer$S[i, ])]
    cond_expec_dt_sub <- subset(cond_expec_dt,subset = colnum==i-1)

    tmp2[[i]] <- cond_expec_dt_sub[mat, .(rowid,cond_expec,colnum), on = col_names,by=.EACHI]
  }
  end <- proc.time()
  end-start

  tmp3 <- list()
  start <- proc.time()
  for(i in 2:nrow(explainer$S)){
    col_names <- feat_names[as.logical(explainer$S[i, ])]
    cond_expec_dt_sub <- subset(cond_expec_dt,subset = colnum==i-1)

    tmp3[[i]] <- cond_expec_dt_sub[mat, .(rowid,cond_expec,colnum), on = col_names,allow.cartesian=TRUE]
  }
  end <- proc.time()
  end-start

  tmp_dt <- rbindlist(tmp3,use.names = T)
  final_dt <- dcast(tmp_dt, formula = "rowid~colnum", value.var = "cond_expec")
  x_test_id <- mat[x_test,]
  S_char_vec <- as.character(1:(nrow(explainer$S)-1))

  final_dt_x_test <- cbind("0"=prediction_zero, final_dt[x_test_id,..S_char_vec,on="rowid"])



  tmp_dt <- rbindlist(tmp)
  tmp_dt <- na.omit(tmp_dt)
  tmp_dt <- merge(mat,tmp_dt,all.x=T,all.y=F)

  setnames(tmp_dt, c("i.cond_expec","i.colnum"), c("cond_expec","colnum"))

  final_dt <- dcast(tmp_dt, formula = paste0(paste0(feat_names, collapse = "+"), "~colnum"), value.var = "cond_expec")

  d

  for(i in 2:nrow(explainer$S)){
    col_names <- feat_names[as.logical(explainer$S[i, ])]
    cond_expec_dt_sub <- subset(cond_expec_dt,subset = colnum==i-1)

    tmp[[i]] <- mat[cond_expec_dt_sub, .(rowid,i.cond_expec,i.colnum), on = col_names,by=.EACHI]
  }



    aa[rowid==1,]

    bb <- cond_expec_dt_sub[mat, .(rowid,cond_expec,colnum), on = col_names,by=.EACHI]
    bb[rowid==1,]

    cc <- cond_expec_dt_sub[mat, .(rowid,cond_expec,colnum), on = col_names,allow.cartesian=TRUE]
    cc[rowid==1,]


    for(i in 2:nrow(explainer$S)){
    col_names <- feat_names[as.logical(explainer$S[i, ])]
    cond_expec_dt[mat, .(i.feat_1_,i.feat_2_,i.feat_3_,i.feat_4_,cond_expec,colnum), on = col_names,by=.EACHI]
    aa=mat[cond_expec_dt, .(feat_1_,feat_2_,feat_3_,feat_4_,i.cond_expec,i.colnum), on = col_names,by=.EACHI]


    cond_expec_dt_sub <- subset(cond_expec_dt,subset = colnum==i-1)

    aa=mat[cond_expec_dt_sub, .(rowid,i.cond_expec,i.colnum), on = col_names,by=.EACHI]
    aa[rowid==1,]

    bb <- cond_expec_dt_sub[mat, .(rowid,cond_expec,colnum), on = col_names,by=.EACHI]
    bb[rowid==1,]

    cc <- cond_expec_dt_sub[mat, .(rowid,cond_expec,colnum), on = col_names,allow.cartesian=TRUE]
    cc[rowid==1,]


# MJ END TESTING ####


    for (i in 1:nrow(cond_expec_dt)){ # THIS IS VERY SLOW FOR LARGE DIMS (BIG LOOP), CAN WE DO SOMETHING?
    on_cols <- feat_names[!is.na(subset(cond_expec_dt[i,], select = feat_names))]
    tmp[[i]] <- mat[cond_expec_dt[i, ], ..select_cols, on = on_cols]
  }

  # Faster ways to do the reduction below here faster. Very fast anyway... Do not prioritize
  tmp_dt <- rbindlist(tmp)
  tmp_dt <- na.omit(tmp_dt)
  tmp_dt <- merge(mat,tmp_dt,all.x=T,all.y=F)

  setnames(tmp_dt, c("i.cond_expec","i.colnum"), c("cond_expec","colnum"))

  final_dt <- dcast(tmp_dt, formula = paste0(paste0(feat_names, collapse = "+"), "~colnum"), value.var = "cond_expec")

  results_dt <- merge(x_test,final_dt,by=feat_names,sort=F)

  results_dt <- cbind("0"=prediction_zero,results_dt[,-(1:dim)])
  return(results_dt)
}
