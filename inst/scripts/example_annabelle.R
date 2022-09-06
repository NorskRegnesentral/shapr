library(shapr)
library(MASS)

devtools::load_all()

# ------------------------------

Boston$rad <- as.factor(Boston$rad)
Boston$chas <- as.factor(Boston$chas)
x_var <- c("rad", "chas")
y_var <- "medv"

ind_x_test <- 1:4
train <- Boston[-ind_x_test, c(x_var, y_var)]
x_test <- Boston[ind_x_test, x_var]
x_train = train[, x_var]
head(train)

x_train = fread("../shapr/x_train.csv")
x_test = fread("../shapr/x_test.csv")

data = rbind(x_train, x_test)
data$feat_1_ = factor(data$feat_1_)
data$feat_2_ = factor(data$feat_2_)
data$feat_3_ = factor(data$feat_3_)

x_train = data[1:1000,]
x_test = data[-c(1:1000),]

joint_prob_dt = fread("../shapr/joint_prob_dt.csv")

p = -0.03051648

# data <- data.frame(rad = c(1, 1, 1, 2, 1, 1, 1, 1),
#                    chas = c(1, 2, 3, 4, 1, 2, 1, 2),
#                    medv = c(22.9, 27.1, 16.5, 18.9, 24, 21.6, 34.7, 33.4))
#
# data$rad = as.factor(data$rad)
# data$chas = as.factor(data$chas)
#
# train <- data[ind_x_test, c(x_var, y_var)]
# x_test <- data[-ind_x_test, x_var]
# x_train = data[, x_var]
# head(train)


# Fitting a basic xgboost model to the training data
model <- lm(medv ~ rad + chas, data = train)

# Specifying the phi_0, i.e. the expected prediction without any features
p <- mean(train$medv)

# Computing the actual Shapley values with kernelSHAP accounting for feature dependence using
# the empirical (conditional) distribution approach with bandwidth parameter sigma = 0.1 (default)

joint_probability_dt

temp = explain(
  x_train = x_train,
  x_explain = x_test,
  model = model,
  approach = "categorical", #  "ctree",
  prediction_zero = p,
  joint_probability_dt =
)
print(temp)
