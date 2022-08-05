library(data.table)

options(digits = 5) # To avoid round off errors when printing output on different systems

set.seed(1234)

data <- data.table::as.data.table(airquality)
data[, Month_factor := as.factor(Month)]
data[, Ozone_sub30 := (Ozone < 30) * 1]
data[, Ozone_sub30_factor := as.factor(Ozone_sub30)]

data_complete <- data[complete.cases(airquality), ]
data_complete <- data_complete[sample(1:.N)] # Sh

y_var_numeric <- "Ozone"
y_var_binary <- "Ozone_sub30"
y_var_binaryfactor <- "Ozone_sub30_factor"


x_var_numeric <- c("Solar.R", "Wind", "Temp", "Month", "Day")
x_var_mixed <- c("Solar.R", "Wind", "Temp", "Day", "Month_factor")

data_train <- head(data_complete, -2)
data_test <- tail(data_complete, 2)

x_train_numeric <- data_train[, ..x_var_numeric]
x_train_mixed <- data_train[, ..x_var_mixed]

x_test_numeric <- data_test[, ..x_var_numeric]
x_test_mixed <- data_test[, ..x_var_mixed]

lm_formula_numeric <- as.formula(paste0(y_var_numeric, " ~ ", paste0(x_var_numeric, collapse = " + ")))
lm_formula_mixed <- as.formula(paste0(y_var_numeric, " ~ ", paste0(x_var_mixed, collapse = " + ")))

model_lm_numeric <- lm(lm_formula_numeric, data = data_complete)
model_lm_mixed <- lm(lm_formula_mixed, data = data_complete)

p0 <- data_train[, mean(get(y_var_numeric))]

