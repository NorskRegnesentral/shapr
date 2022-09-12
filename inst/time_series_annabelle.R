library(data.table)
library(shapr)

devtools::load_all()

set.seed(1)
n_train = 2
n_test = 2
n_features = 4
x = rnorm((n_train + n_test) * (n_features + 5), mean = 2, sd = 2)
# x = 1:((n_train + n_test +1) * n_features)
# x = matrix(x, nrow = n_train + n_test, byrow = T)

x = matrix(x, nrow = n_train + n_test, byrow = T)
x1 = t(apply(x, 1, cumsum))
x = data.table(x[, c(1:n_features, n_features + 5)])


Q1_days <- 1:(floor(n_features / 4))
Q2_days <- 1:(floor(n_features / 4)) + max(Q1_days)
Q3_days <- 1:(floor(n_features / 4)) + max(Q2_days)
Q4_days <- (max(Q3_days) + 1):n_features


group <- list(Q1 = paste0("V", Q1_days),
              Q2 = paste0("V", Q2_days),
              Q3 = paste0("V", Q3_days),
              Q4 = paste0("V", Q4_days))


par(mfrow=c(2,2))
plot(ts(matrix(x[1,])))
abline(v = c(5, 10, 15),
       col = c("blue", "red"),
       lty = c(1, 2), lwd = c(1, 3))
plot(ts(matrix(x[2,])))
abline(v = c(5, 10, 15),
       col = c("blue", "red"),
       lty = c(1, 2), lwd = c(1, 3))
# plot(ts(matrix(x[10,])))
# abline(v = c(5, 10, 15),
#        col = c("blue", "red"),
#        lty = c(1, 2), lwd = c(1, 3))
# plot(ts(matrix(x[101,])))
# abline(v = c(5, 10, 15),
#        col = c("blue", "red"),
#        lty = c(1, 2), lwd = c(1, 3))

response = paste0("V", n_features + 1)
formula = as.formula(paste0(response, "~ ", paste0("V", 1:n_features, collapse = " + ")))

model = lm(formula, data = x)
# (Intercept)           V1           V2           V3           V4
#       1.624       -0.134        0.755       -0.482           NA

x_all <- x[, 1:n_features]
y_all <- x[[response]]

all_pred <- predict(model, x_all)
mean((all_pred-y_all)^2)
# [1] 4.0676e-31

# ---------------

x_explain = x_all[-c(1:n_train), ]
x_train = x_all[1:n_train, ]

p0 <- mean(y_all[-c(1:n_train)])
# 1.4292

# ---------------

explanation_group <- explain(
  model = model,
  x_explain = x_explain,
  x_train = x_train,
  approach = "timeseries",
  prediction_zero = p0,
  group = group,
  timeseries.fixed_sigma_vec = 2
)

explanation_group

plot(explanation_group)

explanation_independence <- explain(
  model = model,
  x_explain = x_explain,
  x_train = x_train,
  approach = "empirical",
  prediction_zero = p0,
  empirical.fixed_sigma_vec = 0.1
)
# W

#       [,1]        [,2]        [,3]        [,4]        [,5]        [,6]        [,7]        [,8]        [,9]       [,10]       [,11]
# [1,]  1.00  6.8182e-08  6.8182e-08  6.8182e-08  6.8182e-08  2.2727e-08  2.2727e-08  2.2727e-08  2.2727e-08  2.2727e-08  2.2727e-08
# [2,] -0.25  2.5000e-01 -8.3333e-02 -8.3333e-02 -8.3333e-02  8.3333e-02  8.3333e-02  8.3333e-02 -8.3333e-02 -8.3333e-02 -8.3333e-02
# [3,] -0.25 -8.3333e-02  2.5000e-01 -8.3333e-02 -8.3333e-02  8.3333e-02 -8.3333e-02 -8.3333e-02  8.3333e-02  8.3333e-02 -8.3333e-02
# [4,] -0.25 -8.3333e-02 -8.3333e-02  2.5000e-01 -8.3333e-02 -8.3333e-02  8.3333e-02 -8.3333e-02  8.3333e-02 -8.3333e-02  8.3333e-02
# [5,] -0.25 -8.3333e-02 -8.3333e-02 -8.3333e-02  2.5000e-01 -8.3333e-02 -8.3333e-02  8.3333e-02 -8.3333e-02  8.3333e-02  8.3333e-02
#            [,12]       [,13]       [,14]       [,15]       [,16]
# [1,]  2.2727e-08  2.2727e-08  2.2727e-08  2.2727e-08 -2.0455e-07
# [2,]  8.3333e-02  8.3333e-02  8.3333e-02 -2.5000e-01  2.5000e-01
# [3,]  8.3333e-02  8.3333e-02 -2.5000e-01  8.3333e-02  2.5000e-01
# [4,]  8.3333e-02 -2.5000e-01  8.3333e-02  8.3333e-02  2.5000e-01
# [5,] -2.5000e-01  8.3333e-02  8.3333e-02  8.3333e-02  2.5000e-01

# (5 x 16) %*% 16 x 2

#   id_combination      1      2
# 1:              1 1.4292 1.4292
# 2:              2 3.5846 3.3787
# 3:              3 3.7709 2.1525
# 4:              4 3.3771 3.8606
# 5:              5 3.1516 3.1516
# 6:              6 2.1990 2.3796
# 7:              7 3.0740 2.1691
# 8:              8 3.5846 3.3787
# 9:              9 1.9915 0.9429
# 10:             10 3.7709 2.1525
# 11:             11 3.3771 3.8606
# 12:             12 1.6884 1.1700
# 13:             13 2.1990 2.3796
# 14:             14 3.0740 2.1691
# 15:             15 1.9915 0.9429
# 16:             16 1.6884 1.1700


explanation_independence = data.table(explanation_independence$shapley_values)

cols = group$Q1
rowSums(explanation_independence[, ..cols])[1] # -0.016181

cols = group$Q2
rowSums(explanation_independence[, ..cols])[1] # 0.0012247

cols =  group$Q3
rowSums(explanation_independence[, ..cols])[1] # 0.15777

cols = group$Q4
rowSums(explanation_independence[, ..cols])[1] # 0.033191

