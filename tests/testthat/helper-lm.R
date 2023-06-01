options(digits = 5) # To avoid round off errors when printing output on different systems

set.seed(12345)

data <- data.table::as.data.table(airquality)
data[, Month_factor := as.factor(Month)]
data[, Ozone_sub30 := (Ozone < 30) * 1]
data[, Ozone_sub30_factor := as.factor(Ozone_sub30)]
data[, Solar.R_factor := as.factor(cut(Solar.R, 10))]
data[, Wind_factor := as.factor(round(Wind))]

data_complete <- data[complete.cases(airquality), ]
data_complete <- data_complete[sample(seq_len(.N))] # Sh

y_var_numeric <- "Ozone"
y_var_binary <- "Ozone_sub30"
y_var_binaryfactor <- "Ozone_sub30_factor"

x_var_numeric <- c("Solar.R", "Wind", "Temp", "Month", "Day")
x_var_mixed <- c("Solar.R", "Wind", "Temp", "Day", "Month_factor")
x_var_categorical <- c("Month_factor", "Ozone_sub30_factor", "Solar.R_factor", "Wind_factor")

data_train <- head(data_complete, -3)
data_explain <- tail(data_complete, 3)

x_train_numeric <- data_train[, ..x_var_numeric]
x_train_mixed <- data_train[, ..x_var_mixed]
x_train_categorical <- data_train[, ..x_var_categorical]

x_explain_numeric <- data_explain[, ..x_var_numeric]
x_explain_mixed <- data_explain[, ..x_var_mixed]
x_explain_categorical <- data_explain[, ..x_var_categorical]

lm_formula_numeric <- as.formula(paste0(y_var_numeric, " ~ ", paste0(x_var_numeric, collapse = " + ")))
lm_formula_mixed <- as.formula(paste0(y_var_numeric, " ~ ", paste0(x_var_mixed, collapse = " + ")))
lm_formula_interaction <- Ozone ~ Solar.R * Wind
# lm_formula_numeric_col_order <- as.formula(paste0(y_var_numeric, " ~ ",
#                                                   paste0(sort(x_var_numeric), collapse = " + ")))
lm_formula_categorical <- as.formula(paste0(y_var_numeric, " ~ ", paste0(x_var_categorical, collapse = " + ")))

model_lm_numeric <- lm(lm_formula_numeric, data = data_complete)
model_lm_categorical <- lm(lm_formula_categorical, data = data_complete)
model_lm_numeric_col_order <- lm(lm_formula_numeric, data = rev(data_complete))
model_lm_mixed <- lm(lm_formula_mixed, data = data_complete)
model_lm_interaction <- lm(lm_formula_interaction, data = data_complete)

p0 <- data_train[, mean(get(y_var_numeric))]
