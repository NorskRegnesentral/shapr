library(shapr)
library(MASS)

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

# ------------------------------
# To test the categorical method when we know the results

# data = fread("../shapr/data.csv")
# data$feat_1_ = factor(data$feat_1_)
# data$feat_2_ = factor(data$feat_2_)
# data$feat_3_ = factor(data$feat_3_)
#
# x_train = data[1:1000, c("feat_1_", "feat_2_", "feat_3_")]
# x_test = data[1001:1005, c("feat_1_", "feat_2_", "feat_3_")]
#
# joint_prob_dt = fread("../shapr/joint_prob_dt.csv")
#
# p <- mean(data[1:1000,][['response']])
#
# joint_prob_dt[, feat_1_ := as.factor(feat_1_)]
# joint_prob_dt[, feat_2_ := as.factor(feat_2_)]
# joint_prob_dt[, feat_3_ := as.factor(feat_3_)]

# model <- lm(response ~ feat_1_ + feat_2_ + feat_3_, data = data[1:1000, ])

# ------------------------------

model <- lm(medv ~ rad + chas, data = train)

temp = explain(
  x_train = x_train,
  x_explain = x_test,
  model = model,
  approach = "categorical",
  prediction_zero = p
  # joint_probability_dt = joint_prob_dt
)
print(temp)
#         none    rad   chas
# 1: -0.030511 13.231 10.887
# 2: -0.030511 15.709 11.035
# 3: -0.030511 15.709 11.035
# 4: -0.030511 16.624 10.883
