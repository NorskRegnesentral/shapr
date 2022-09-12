library(data.table)
library(shapr)

devtools::load_all()

set.seed(1)
n_train = 1000
n_test = 6
n_features = 200
# x = rnorm((n_train + n_test) * (n_features + 5), mean = 1, sd = 2)
# x = matrix(x, nrow = n_train + n_test, byrow = T)
# x1 = t(apply(x, 1, cumsum))
# x = data.table(x[, c(1:n_features, n_features + 5)])

alpha <- 1
beta <- 0
theta <- 0.8

data = NULL
for(n in 1:(n_train + n_test)){
  set.seed(n)
  e <- rnorm(n_features + 6, mean = 0, sd = 1)

  m_1 <- 0
  for(i in 2:length(e)){
    m_1[i] <- alpha + beta * i + theta * m_1[i - 1] + e[i]
  }
  data = rbind(data, m_1)
}


x <- data[, c(2:(n_features + 1), n_features + 5)]
x <- data.table(x)

plot(ts((t(x)[,1])))
points(ts((t(x)[,1])), pch = 19)

Q1_days <- 1:(floor(n_features / 4))
Q2_days <- 1:(floor(n_features / 4)) + max(Q1_days)
Q3_days <- 1:(floor(n_features / 4)) + max(Q2_days)
Q4_days <- (max(Q3_days) + 1):n_features

group <- list(Q1 = paste0("V", Q1_days),
              Q2 = paste0("V", Q2_days),
              Q3 = paste0("V", Q3_days),
              Q4 = paste0("V", Q4_days))

response = paste0("V", n_features + 1)
formula = as.formula(paste0(response, "~ ", paste0("V", 1:n_features, collapse = " + ")))

model = lm(formula, data = x)

x_all <- x[, 1:n_features]
y_all <- x[[response]]

all_pred <- predict(model, x_all)
mean((all_pred-y_all)^2)
# [1] 1.8074

# ---------------

x_explain = x_all[-c(1:n_train), ]
x_train = x_all[1:n_train, ]

p0 <- mean(y_all[-c(1:n_train)])

# ---------------

explanation_group <- explain(
  model = model,
  x_explain = x_explain,
  x_train = x_train,
  approach = "timeseries",
  prediction_zero = p0,
  group = group,
  timeseries.fixed_sigma_vec = 2
  # timeseries.bounds = c(-1, 2)
)

explanation_group
#      none         Q1        Q2        Q3       Q4
# 1: 5.1217 -0.0019489  0.201396 -0.208099  0.74808
# 2: 5.1217  0.0164650 -0.148537  0.639499  0.38405
# 3: 5.1217 -0.4625373  0.564378 -0.281495  0.61380
# 4: 5.1217 -0.1859842 -0.121323  0.048696 -0.25682
# 5: 5.1217 -1.2290037 -0.896415  1.096474 -0.10961
# 6: 5.1217 -0.0435240 -0.049311  0.898789 -1.36716

plot(explanation_group, plot_phi0 = F)
