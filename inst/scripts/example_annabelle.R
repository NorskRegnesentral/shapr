library(shapr)
library(data.table)
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

model <- lm(medv ~ rad + chas, data = train)

# ------------------------------
# To test the categorical method when we know the results

data = fread("../shapr/data.csv")
data$feat_1_ = factor(data$feat_1_)
data$feat_2_ = factor(data$feat_2_)
data$feat_3_ = factor(data$feat_3_)

x_train = data[1:1000, c("feat_1_", "feat_2_", "feat_3_")]
x_test = data[1001:1005, c("feat_1_", "feat_2_", "feat_3_")]

joint_prob_dt = fread("../shapr/joint_prob_dt.csv")

p <- mean(data[1:1000,][['response']])

joint_prob_dt[, feat_1_ := as.factor(feat_1_)]
joint_prob_dt[, feat_2_ := as.factor(feat_2_)]
joint_prob_dt[, feat_3_ := as.factor(feat_3_)]

train = data[1:1000,]

model <- lm(response ~ feat_1_ + feat_2_ + feat_3_, data = train)

# ------------------------------

temp = explain(
  x_train = x_train,
  x_explain = x_test,
  model = model,
  approach = "categorical",
  prediction_zero = p,
  joint_probability_dt = joint_prob_dt
)
print(temp)
#         none    rad   chas
# 1: -0.030511 13.231 10.887
# 2: -0.030511 15.709 11.035
# 3: -0.030511 15.709 11.035
# 4: -0.030511 16.624 10.883

# Without joint prob dt
#         none feat_1_  feat_2_    feat_3_
# 1: -0.030516 0.20455  0.29895  0.1381985
# 2: -0.030516 0.23079  0.35300 -0.0480793
# 3: -0.030516 0.13084  0.32979 -0.8297798
# 4: -0.030516 0.23133 -0.88754  0.1923399
# 5: -0.030516 0.27954 -0.84447 -0.0049256

# With joint prob dt
# none   feat_1_    feat_2_       feat_3_
# 1: -0.03051645 0.2211416  0.3030599  0.1174976222
# 2: -0.03051648 0.2312988  0.3611456 -0.0567361622
# 3: -0.03051644 0.1437691  0.3371903 -0.8501081617
# 4: -0.03051647 0.2446707 -0.8627886  0.1542449764
# 5: -0.03051649 0.2140973 -0.7843376  0.0003764934

#      none Month_factor Ozone_sub30_factor Solar.R_factor Wind_factor
# 1: 40.752       6.1998             7.8422          2.852     70.2288
# 2: 40.752      -3.7270             9.8283          5.626      4.1224
