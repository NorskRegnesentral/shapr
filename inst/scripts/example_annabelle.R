library(shapr)
library(MASS)

devtools::load_all()

# ------------------------------

# Boston$rad <- as.factor(Boston$rad)
# Boston$chas <- as.factor(Boston$chas)
# x_var <- c("rad", "chas")
# y_var <- "medv"
#
# ind_x_test <- 1:4
# train <- Boston[-ind_x_test, c(x_var, y_var)]
# x_test <- Boston[ind_x_test, x_var]
# x_train = train[, x_var]
# head(train)

data = fread("../shapr/data.csv")
data$feat_1_ = factor(data$feat_1_)
data$feat_2_ = factor(data$feat_2_)
data$feat_3_ = factor(data$feat_3_)

x_train = data[1:1000, c("feat_1_", "feat_2_", "feat_3_")]
x_test = data[1001:1005, c("feat_1_", "feat_2_", "feat_3_")]

joint_prob_dt = fread("../shapr/joint_prob_dt.csv")

p = -0.03051648

# Fitting a basic xgboost model to the training data
model <- lm(response ~ feat_1_ + feat_2_ + feat_3_, data = data[1:1000, ])

# Specifying the phi_0, i.e. the expected prediction without any features
p <- mean(data[1:1000,][['response']])

# Computing the actual Shapley values with kernelSHAP accounting for feature dependence using
# the empirical (conditional) distribution approach with bandwidth parameter sigma = 0.1 (default)

# joint_probability_dt

temp = explain(
  x_train = x_train,
  x_explain = x_test,
  model = model,
  approach = "categorical", #  "ctree",
  prediction_zero = p,
)
print(temp)
