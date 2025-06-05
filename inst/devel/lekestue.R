library(xgboost)

data("airquality")
data <- data.table::as.data.table(airquality)
data <- data[complete.cases(data), ]

x_var <- c("Solar.R", "Wind", "Temp", "Month")
y_var <- "Ozone"

x_train <- data[, ..x_var]
y_train <- data[, get(y_var)]


# Fitting a basic xgboost model to the training data
model <- xgboost(
  data = as.matrix(x_train),
  label = y_train,
  nround = 20,
  verbose = FALSE
)

p0 <- mean(y_train)

explanation <- explain(
  model = model,
  x_explain = x_train,
  x_train = x_train,
  approach = "gaussian",
  phi0 = p0
)

# #### SAGE ####
#
# full_loss <- mean((explanation$pred_explain-y_train)^2)
# zero_loss <- mean((p0-y_train)^2)
#
# # Decompose the difference between the zero and full loss:
# zero_loss - full_loss
#
# vS_SHAP <- explanation$internal$output$dt_vS[,-1]
#
# vS_SAGE <- zero_loss-colMeans((t(vS_SHAP)-y_train)^2)
#
#
# W <- explanation$internal$objects$W
#
#
# dt_SAGE <- data.table::as.data.table(t(W %*% as.matrix(vS_SAGE)))
# colnames(dt_SAGE) <- c("none", x_var)
#
# # The SAGE values
# dt_SAGE[,-1]
#
# sum(dt_SAGE)

