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

test <-explain(model = model_lm_numeric,
               x_explain = x_explain_numeric,
               x_train = x_train_numeric,
               approach = "gaussian",
               shap_approach = "permutation",
               prediction_zero = p0)

debugonce(explain_linear)

test2 <-explain_linear(model = model_lm_numeric,
                       x_explain = x_explain_numeric,
                       x_train = x_train_numeric,
                       prediction_zero = p0)



