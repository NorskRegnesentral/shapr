
#### Computing the various Shapley approximations ###########

empirical_fixed_sigma.0001_settings = list(type = "fixed_sigma",
                                           fixed_sigma_vec = 0.001,
                                           kernel_metric = "Gaussian")

empirical_fixed_sigma.001_settings = list(type = "fixed_sigma",
                                          fixed_sigma_vec = 0.01,
                                          kernel_metric = "Gaussian")

empirical_fixed_sigma.01_settings = list(type = "fixed_sigma",
                                         fixed_sigma_vec = 0.1,
                                         kernel_metric = "Gaussian")

empirical_fixed_sigma.02_settings = list(type = "fixed_sigma",
                                         fixed_sigma_vec = 0.2,
                                         kernel_metric = "Gaussian")

empirical_fixed_sigma.03_settings = list(type = "fixed_sigma",
                                         fixed_sigma_vec = 0.3,
                                         kernel_metric = "Gaussian")


#
# empirical_AIC_each_k_settings = list(type = "AICc_each_k",
#                                      AICc_optimize_every_testobs = F,
#                                      AICc_no_samp_per_optim = 10000,
#                                      AIC_optim_func = "nlminb",
#                                      AIC_optim_max_eval = 20,
#                                      AIC_optim_startval = 0.1,
#                                      kernel_metric = "Gaussian")
#
# empirical_AIC_full_settings = list(type = "AICc_full",
#                                    AICc_optimize_every_testobs = F,
#                                    AICc_no_samp_per_optim = 10000,
#                                    AIC_optim_func = "nlminb",
#                                    AIC_optim_max_eval = 20,
#                                    AIC_optim_startval = 0.1,
#                                    kernel_metric = "Gaussian")

empirical_independence_settings = list(type = "independence")


#### Performing various approximations methods #####

Shapley.approx = list()

Shapley.approx$empirical_sigma.0001 = compute_kernelShap(model = model,
                                                       l = l,
                                                       w_threshold = w_threshold,
                                                       n_threshold = n_threshold,
                                                       cond_approach = "empirical",
                                                       empirical_settings = empirical_fixed_sigma.0001_settings,
                                                       pred_zero=pred_zero)

Shapley.approx$empirical_sigma.001 = compute_kernelShap(model = model,
                                                       l = l,
                                                       w_threshold = w_threshold,
                                                       n_threshold = n_threshold,
                                                       cond_approach = "empirical",
                                                       empirical_settings = empirical_fixed_sigma.001_settings,
                                                       pred_zero=pred_zero)

Shapley.approx$empirical_sigma.01 = compute_kernelShap(model = model,
                                                       l = l,
                                                       w_threshold = w_threshold,
                                                       n_threshold = n_threshold,
                                                       cond_approach = "empirical",
                                                       empirical_settings = empirical_fixed_sigma.01_settings,
                                                       pred_zero=pred_zero)

Shapley.approx$empirical_sigma.02 = compute_kernelShap(model = model,
                                                        l = l,
                                                        w_threshold = w_threshold,
                                                        n_threshold = n_threshold,
                                                        cond_approach = "empirical",
                                                        empirical_settings = empirical_fixed_sigma.02_settings,
                                                        pred_zero=pred_zero)
Shapley.approx$empirical_sigma.03 = compute_kernelShap(model = model,
                                                       l = l,
                                                       w_threshold = w_threshold,
                                                       n_threshold = n_threshold,
                                                       cond_approach = "empirical",
                                                       empirical_settings = empirical_fixed_sigma.03_settings,
                                                       pred_zero=pred_zero)

#
# Shapley.approx$empirical_AIC_each_k = compute_kernelShap(model = model,
#                                                          l = l,
#                                                          w_threshold = w_threshold,
#                                                          n_threshold = n_threshold,
#                                                          cond_approach = "empirical",
#                                                          empirical_settings = empirical_AIC_each_k_settings,
#                                                          pred_zero=pred_zero)
#
# Shapley.approx$empirical_AIC_full = compute_kernelShap(model = model,
#                                                        l = l,
#                                                        w_threshold = w_threshold,
#                                                        n_threshold = n_threshold,
#                                                        cond_approach = "empirical",
#                                                        empirical_settings = empirical_AIC_full_settings,
#                                                        pred_zero=pred_zero)

Shapley.approx$empirical_independence = compute_kernelShap(model = model,
                                                           l = l,
                                                           w_threshold = w_threshold,
                                                           n_threshold = n_threshold,
                                                           cond_approach = "empirical",
                                                           empirical_settings = empirical_independence_settings,
                                                           pred_zero=pred_zero)

Shapley.approx$Gaussian = compute_kernelShap(model = model,
                                             l,
                                             w_threshold = w_threshold,
                                             n_threshold = n_threshold,
                                             cond_approach = "Gaussian",
                                             pred_zero=pred_zero)

Shapley.approx$copula = compute_kernelShap(model = model,
                                           l,
                                           w_threshold = w_threshold,
                                           n_threshold = n_threshold,
                                           cond_approach = "copula",
                                           pred_zero=pred_zero)
if(class(model)=="xgb.Booster"){
    tmp= predict(model,as.matrix(Xtest),predcontrib=T)
    colnames(tmp) <- NULL
    Shapley.approx$treeSHAP <- list()
    Shapley.approx$treeSHAP$Kshap <- tmp[,c(ncol(Xtest)+1,1:ncol(Xtest))]
    Shapley.approx$treeSHAP$other_objects <- list()
    Shapley.approx$treeSHAP$other_objects$h_optim_mat <- matrix(NA,ncol=nrow(Xtest),nrow=2^ncol(Xtest))
}



