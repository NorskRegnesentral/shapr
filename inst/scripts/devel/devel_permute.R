set.seed(12345)

data <- data.table::as.data.table(airquality)

data_complete <- data[complete.cases(airquality), ]
data_complete <- data_complete[sample(seq_len(.N))] # Sh

y_var_numeric <- "Ozone"

x_var_numeric <- c("Solar.R", "Wind", "Temp", "Month", "Day")

data_train <- head(data_complete, -3)
data_explain <- tail(data_complete, 3)

x_train_numeric <- data_train[, ..x_var_numeric]

x_explain_numeric <- data_explain[, ..x_var_numeric]

lm_formula_numeric <- as.formula(paste0(y_var_numeric, " ~ ", paste0(x_var_numeric, collapse = " + ")))

model_lm_numeric <- lm(lm_formula_numeric, data = data_complete)

p0 <- data_train[, mean(get(y_var_numeric))]

test_kernel <-explain(model = model_lm_numeric,
                      x_explain = x_explain_numeric,
                      x_train = x_train_numeric,
                      approach = "empirical",
                      shap_approach = "kernel",
                      prediction_zero = p0)

test_permute <-explain(model = model_lm_numeric,
                       x_explain = x_explain_numeric,
                       x_train = x_train_numeric,
                       approach = "empirical",
                       shap_approach = "permutation",
                       prediction_zero = p0)


test_kernel$shapley_values-test_permute$shapley_values


permute_list <- kernel_list <- list()
for(i in 1:10){
  set.seed(1+i)

  test_kernel <-explain(model = model_lm_numeric,
                        x_explain = x_explain_numeric,
                        x_train = x_train_numeric,
                        approach = "empirical",
                        shap_approach = "kernel",
                        prediction_zero = p0,n_combinations = 2^5-3,seed = i)


  test_permute <-explain(model = model_lm_numeric,
                         x_explain = x_explain_numeric,
                         x_train = x_train_numeric,
                         approach = "empirical",
                         shap_approach = "permutation",
                         prediction_zero = p0,n_permutations = factorial(5)-3,seed=i)
  permute_list[[i]] <- test_permute$shapley_values
  kernel_list[[i]] <- test_kernel$shapley_values

  print(i)

}

permute_list <- lapply(permute_list,as.matrix)
kernel_list <- lapply(kernel_list,as.matrix)

# permute is way more precise than kernel when sampling

Reduce(`+`,permute_list)/length(permute_list)

Reduce(`+`,kernel_list)/length(kernel_list)

explain(model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        approach = "empirical",
        shap_approach = "kernel",
        prediction_zero = p0)


