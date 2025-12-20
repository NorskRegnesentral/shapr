library(xgboost)
library(shapr)

data("airquality")
data <- data.table::as.data.table(airquality)
data <- data[complete.cases(data), ]

x_var <- c("Solar.R", "Wind", "Temp", "Month")
y_var <- "Ozone"

ind_x_explain <- 1:6
x_train <- data[-ind_x_explain, ..x_var]
y_train <- data[-ind_x_explain, get(y_var)]
x_explain <- data[ind_x_explain, ..x_var]

# Look at the dependence between the features
cor(x_train)

# Fit a basic xgboost model to the training data
model <- xgboost(
  x = x_train,
  y = y_train,
  nround = 20,
  verbosity = 0
)

# Specify phi_0, i.e., the expected prediction without any features
p0 <- mean(y_train)

# Compute Shapley values with Kernel SHAP, accounting for feature dependence using
# the empirical (conditional) distribution approach with bandwidth parameter sigma = 0.1 (default)
explanation <- explain(
  model = model,
  x_explain = x_explain,
  x_train = x_train,
  approach = "empirical",
  phi0 = p0,
  seed = 1
)

# Print the Shapley values for the observations to explain.
print(explanation)

# Provide a formatted summary of the shapr object
aa=summary(explanation)

names(aa)

attributes(aa)

aa

names(aa)

aa$calling_function

aa$dt_valid_coalitions
aa$dt_used_coalitions

print(aa)

aa$proglang
names(aa)

class(aa$shapr_object)

aa$shapr_object$

names(aa)

aa$approach
aa$proglang
aa$parameters

class(aa)

aa

print(aa)

names(aa)
#################


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
x_var_numeric <- c("Solar.R", "Wind", "Temp", "Month", "Day")

lm_formula_numeric <- as.formula(paste0(y_var_numeric, " ~ ", paste0(x_var_numeric, collapse = " + ")))

mod <- lm(lm_formula_numeric, data = data_complete)

lmsum <- summary(mod)

lmsum

lmsum2 <- print(lmsum)

names(lmsum)

