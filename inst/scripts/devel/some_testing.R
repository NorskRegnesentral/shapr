dt <- cbind(x_train_red,x_excluded_here)
setcolorder(dt,names(x_train))

dt[, p_hat_1 := pred_mod_xgb(model, newdata = .SD), .SDcols = feature_names]

dt[,mean(p_hat_1)]


dt <- cbind(x_train)
setcolorder(dt,names(x_train))

dt[, p_hat_1 := pred_mod_xgb(model, newdata = .SD), .SDcols = feature_names]

dt[,mean(p_hat_1)]


dt <- cbind(samps_dt,x_excluded_here)
setcolorder(dt,names(x_train))

dt[, p_hat_1 := pred_mod_xgb(model, newdata = .SD), .SDcols = feature_names]

dt[,mean(p_hat_1)]


samps_list <- list()
for(i in 3:25){
  features_here <- i:25
  tree <- shapr:::create_ctree(features_here, x_train_red, ctree.mincriterion, ctree.minsplit, ctree.minbucket)

  x_explain_red_here <- x_explain_red[testObs,]

  samps_list[[i]] <- shapr:::sample_ctree(
    tree,
    n_samples = n_samples,
    x_explain = x_explain_red_here,
    x_train = x_train_red,
    n_features = length(x_train_red),
    sample = ctree.sample
  )
  samps_list[[i]][,testObs:=testObs]
  id_combination_here0 <- S_extended_comp[i,id_combination]
  samps_list[[i]][,id_combination:=id_combination_here0]

  x_excluded_here <- x_explain[testObs,..excluded_feature_cols]


  dt <- cbind(samps_list[[i]],x_excluded_here)
  setcolorder(dt,names(x_train))

  dt[, p_hat_1 := pred_mod_xgb(model, newdata = .SD), .SDcols = feature_names]

  print(c(i,dt[,mean(p_hat_1)]))
}


