library(data.table)
library(shapr)

load("inst/groupSHAP/shapley_objects_new.RData")

predict_model.lasso_manual_model <- function(x, newdata) {
  z <- as.vector(x$alpha + as.matrix(newdata)[,x$these_nonzero_betas]%*%x$nonzero_beta_vals)
  exp(z)/(exp(z)+1)
}

phi0 <- mean(y_train[,binary_response])

set.seed(123)

future::plan("multisession", workers = 10) # Increase the number of workers for increased performance with many features
progressr::handlers(global = TRUE)
progressr::handlers("cli") # Using the cli package as backend (recommended for the estimates of the remaining time)


ex_indep <- shapr::explain(model=lasso_model,
                           x_explain = x_test,
                           x_train = x_train,
                           approach = "independence",
                           group=gene_groups,
                           predict_model = predict_model.lasso_manual_model,
                           max_n_coalitions = 100,
                           phi0 = phi0,
                           iterative = FALSE)

ex_emp <- shapr::explain(model=lasso_model,
                           x_explain = head(x_test),
                           x_train = x_train,
                           approach = "empirical",
                           group=gene_groups,
                           predict_model = predict_model.lasso_manual_model,
                           max_n_coalitions = 100,
                           phi0 = phi0,
                           iterative = FALSE)





explainer <- shapr(x_train,model = lasso_model,n_combinations = 1.5*10^3,group = gene_groups)

p <-


explanation_indep <- explain(
  x_test,
  approach = "independence",
  explainer = explainer,
  prediction_zero = p,
  n_batches = 100
)

saveRDS(explanation_indep,file = "colitis_explanation_independence_5.rds")

explanation_emp <- explain(
  x_test,
  approach = "empirical",
  explainer = explainer,
  prediction_zero = p,
  n_batches = 100
)

saveRDS(explanation_emp,file = "colitis_explanation_empirical_5.rds")
