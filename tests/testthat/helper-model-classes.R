# Helper data for model class tests
# Minimal datasets with 2 features for fast testing

set.seed(123)

# Data for regression models (lm, glm, ranger, xgboost, gam, workflows)
data_reg <- data.table::as.data.table(airquality)
data_reg <- data_reg[complete.cases(data_reg), ]

# Use only 2 features for speed
x_var_reg <- c("Solar.R", "Wind")
y_var_reg <- "Ozone"

x_train_reg <- data_reg[1:30, ..x_var_reg]
x_explain_reg <- data_reg[31:32, ..x_var_reg]
y_train_reg <- data_reg[1:30, get(y_var_reg)]
phi0_reg <- mean(y_train_reg)

# Data for classification models (glm binomial, ranger probability, xgboost binary)
data_class <- data.table::copy(data_reg)
data_class[, Ozone_binary := as.factor(ifelse(Ozone > median(Ozone), 1, 0))]

y_var_class <- "Ozone_binary"
x_train_class <- data_class[1:30, ..x_var_reg]
x_explain_class <- data_class[31:32, ..x_var_reg]
y_train_class <- data_class[1:30, get(y_var_class)]
phi0_class <- mean(as.numeric(y_train_class) - 1)

# Data for time series models (ar, Arima)
# Reuse data from helper-ar-arima
y_ts <- data_arima$Temp
train_idx_ts <- 3:50 # Start from 3 to accommodate max lag of 2
explain_idx_ts <- 51:52
phi0_ts <- mean(y_ts[train_idx_ts])
