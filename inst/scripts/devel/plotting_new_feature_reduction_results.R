
rm(list=ls())

val=1:3



MAE_list <- list()

for(kkk in val){
  MAE_list[[kkk]] <- list()

  load(paste0("MJ_testing_new_feature_reduction_code_val_0",kkk,".RData"))

  MAE_list[[kkk]]$meanMAE_obs_red <- rowMeans(abs(expl_full$shapley_values[,-c(1,2)]-red_shap_vals[,-c(1,2,3)]))

  MAE_list[[kkk]]$meanMAE_obs_std_redno_exact <- rowMeans(abs(expl_full$shapley_values[,-c(1,2)]-shap_vals_redno_exact[,-c(1,2)]))

  MAE_list[[kkk]]$meanMAE_obs_std_redno_exact_signif <- rowMeans(abs(expl_full$shapley_values[,3:8]-shap_vals_redno_exact[,3:8]))
  MAE_list[[kkk]]$meanMAE_obs_red_signif <- rowMeans(abs(expl_full$shapley_values[,3:8]-red_shap_vals[,4:9]))

  MAE_list[[kkk]]$MAE_std_redno_exact <- colMeans(abs(expl_full$shapley_values[,-c(1:2)]-shap_vals_redno_exact[,-c(1:2)]))
  MAE_list[[kkk]]$MAE_red <- colMeans(abs(expl_full$shapley_values[,-c(1:2)]-red_shap_vals[,-c(1:3)]))

  tot_used_coal <- sapply(ret_list, function(x) x$total_used_coal[length(x$total_used_coal)])

  MAE_list[[kkk]]$tot_used_coal <- tot_used_coal

  MAE_list[[kkk]]$no_feat_red <- sapply(ret_list,function(x) sum(1-is.na(tail(x$dropped_features,1))))


  print(kkk)
}


par(mfrow=c(2,3))
for(kkk in val){
  plot(MAE_list[[kkk]]$meanMAE_obs_std_redno_exact,MAE_list[[kkk]]$meanMAE_obs_red,xlim=c(0,0.4),ylim=c(0,0.4),
       main=paste0("Reduction threshold = 0.",kkk,""),
       xlab="meanMAE of regular method same #v(S)",ylab="meanMAE of reduction method",asp = 1)
  abline(a=0,b=1,col=2)
}

for(kkk in val){
  plot(MAE_list[[kkk]]$meanMAE_obs_std_redno_exact_signif,MAE_list[[kkk]]$meanMAE_obs_red_signif,xlim=c(0,0.4),ylim=c(0,0.4),
       main=paste0("Reduction threshold = 0.",kkk,"\n Only signif features",asp=1),
       xlab="meanMAE of regular method same #v(S)",ylab="meanMAE of reduction method")
  abline(a=0,b=1,col=2)
}


par(mfrow=c(1,3))
for(kkk in val){
  plot(MAE_list[[kkk]]$MAE_std_redno_exact,ylim=c(0,0.3),
       main=paste0("Reduction threshold = 0.",kkk),
       xlab="Feature number",ylab="MAE per feature (black:regular, red:reduction)",type="l")
  points(MAE_list[[kkk]]$MAE_std_redno_exact)
  lines(MAE_list[[kkk]]$MAE_red,col=2)
  points(MAE_list[[kkk]]$MAE_red,col=2)
}

par(mfrow=c(2,3))
for(kkk in val){
  hist(MAE_list[[kkk]]$tot_used_coal,ylim=c(0,50),main=paste0("Reduction threshold = 0.",kkk),
       xlab="# Coalitions used")
}

for(kkk in val){
  hist(MAE_list[[kkk]]$no_feat_red,ylim=c(0,80),main=paste0("Reduction threshold = 0.",kkk),
       xlab = "# Features removed")
}



MAE_list[[kkk]]


plot(MAE_std_redno_exact)
points(MAE_red,col=2)



head(expl_full$shapley_values)


load("MJ_testing_new_feature_reduction_code_val_02.RData")

