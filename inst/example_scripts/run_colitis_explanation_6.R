library(data.table)
library(shapr)

load("/disk/home/jullum/Dropbox/Local_work/Projects/groupShapley/shapley_objects_new.RData")

predict_model.lasso_manual_model <- function(x, newdata) {
  z <- as.vector(x$alpha + as.matrix(newdata)[,x$these_nonzero_betas]%*%x$nonzero_beta_vals)
  exp(z)/(exp(z)+1)
}

set.seed(123)
explainer <- shapr(x_train,model = lasso_model,n_combinations = 8.2*10^3,group = gene_groups)

p <- mean(y_train[,binary_response])


explanation_emp <- explain(
  x_test,
  approach = "empirical",
  explainer = explainer,
  prediction_zero = p,
  n_batches = 100
)

saveRDS(explanation_emp,file = "colitis_explanation_empirical_6.rds")

