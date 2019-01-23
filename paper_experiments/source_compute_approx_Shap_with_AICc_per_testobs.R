
#### Computing the various Shapley approximations ###########

empirical_fixed_sigma.01_settings = list(type = "fixed_sigma",
                                         fixed_sigma_vec = 0.1,
                                         kernel_metric = "Gaussian")

empirical_AIC_each_k_settings = list(type = "AICc_each_k",
                                     AICc_optimize_every_testobs = T,
                                     AICc_no_samp_per_optim = 1000,
                                     AIC_optim_func = "nlminb",
                                     AIC_optim_max_eval = 20,
                                     AIC_optim_startval = 0.1,
                                     kernel_metric = "Gaussian",
                                     AICc_force_use_all_trainsamp_per_optim = T,
                                     AICc_combination_type = "alternative")

empirical_AIC_full_settings = list(type = "AICc_full",
                                   AICc_optimize_every_testobs = T,
                                   AICc_no_samp_per_optim = 1000,
                                   AIC_optim_func = "nlminb",
                                   AIC_optim_max_eval = 20,
                                   AIC_optim_startval = 0.1,
                                   kernel_metric = "Gaussian",
                                   AICc_force_use_all_trainsamp_per_optim = T,
                                   AICc_combination_type = "alternative")

empirical_independence_settings = list(type = "independence")


#### Performing various approximations methods #####

Shapley.approx = list()

Shapley.approx$empirical_sigma.01 = compute_kernelShap(model = model,
                                                       l = l,
                                                       w_threshold = w_threshold,
                                                       n_threshold = n_threshold,
                                                       cond_approach = "empirical",
                                                       empirical_settings = empirical_fixed_sigma.01_settings,
                                                       pred_zero=pred_zero)

if(X_dim==3){

    Shapley.approx$empirical_AIC_each_k = compute_kernelShap(model = model,
                                                             l = l,
                                                             w_threshold = w_threshold,
                                                             n_threshold = n_threshold,
                                                             cond_approach = "empirical",
                                                             empirical_settings = empirical_AIC_each_k_settings,
                                                             pred_zero=pred_zero)

    Shapley.approx$empirical_AIC_full = compute_kernelShap(model = model,
                                                           l = l,
                                                           w_threshold = w_threshold,
                                                           n_threshold = n_threshold,
                                                           cond_approach = "empirical",
                                                           empirical_settings = empirical_AIC_full_settings,
                                                           pred_zero=pred_zero)

} else{

    Shapley.approx$comb_sigma.01 = compute_kernelShap(model = model,
                                                      l = l,
                                                      w_threshold = w_threshold,
                                                      n_threshold = n_threshold,
                                                      cond_approach = list(empirical = 1:176,copula =177:1024),
                                                      empirical_settings = empirical_fixed_sigma.01_settings,
                                                      pred_zero=pred_zero)

    Shapley.approx$comb_independence = compute_kernelShap(model = model,
                                                          l = l,
                                                          w_threshold = w_threshold,
                                                          n_threshold = n_threshold,
                                                          cond_approach = list(empirical = 1:176,copula =177:1024),
                                                          empirical_settings = empirical_independence_settings,
                                                          pred_zero=pred_zero)


    Shapley.approx$comb_AIC_each_k = compute_kernelShap(model = model,
                                                        l = l,
                                                        w_threshold = w_threshold,
                                                        n_threshold = n_threshold,
                                                        cond_approach = list(empirical = 1:176,copula =177:1024),
                                                        empirical_settings = empirical_AIC_each_k_settings,
                                                        pred_zero=pred_zero)

    # Shapley.approx$comb_AIC_full = compute_kernelShap(model = model,
    #                                                   l,
    #                                                   w_threshold = w_threshold,
    #                                                   n_threshold = n_threshold,
    #                                                   cond_approach = list(empirical = 1:176,copula =177:1024),
    #                                                   empirical_settings = empirical_AIC_full_settings,
    #                                                   pred_zero=pred_zero)

}

Shapley.approx$empirical_independence = compute_kernelShap(model = model,
                                                           l = l,
                                                           w_threshold = w_threshold,
                                                           n_threshold = n_threshold,
                                                           cond_approach = "empirical",
                                                           empirical_settings = empirical_independence_settings,
                                                           pred_zero=pred_zero)

Shapley.approx$Gaussian = compute_kernelShap(model = model,
                                             l = l,
                                             w_threshold = w_threshold,
                                             n_threshold = n_threshold,
                                             cond_approach = "Gaussian",
                                             pred_zero=pred_zero)

Shapley.approx$copula = compute_kernelShap(model = model,
                                           l = l,
                                           w_threshold = w_threshold,
                                           n_threshold = n_threshold,
                                           cond_approach = "copula",
                                           pred_zero=pred_zero)






if(class(model)=="xgb.Booster"){
    tt <- proc.time()
    tmp= predict(model,as.matrix(Xtest),predcontrib=T)
    colnames(tmp) <- NULL
    Shapley.approx$treeSHAP <- list()
    Shapley.approx$treeSHAP$Kshap <- tmp[,c(ncol(Xtest)+1,1:ncol(Xtest))]
    Shapley.approx$treeSHAP$other_objects <- list()
    Shapley.approx$treeSHAP$other_objects$h_optim_mat <- matrix(NA,ncol=nrow(Xtest),nrow=2^ncol(Xtest))
    Shapley.approx$treeSHAP$other_objects$h_optim_DT <- NULL
    Shapley.approx$treeSHAP$other_objects$comp_time <- proc.time()-tt
}



