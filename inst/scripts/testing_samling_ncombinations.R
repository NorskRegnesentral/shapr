library(xgboost)
#library(shapr)
# remotes::install_github("NorskRegnesentral/shapr@devel")
library(shapr)
library(data.table)
n = c(100, 1000, 2000)
p = c(5, 10, 10)
n_combinations = c(20, 800, 800)

res = list()
for (i in seq_along(n)) {
  set.seed(123)
  cat("n =", n[i], "p =", p[i], "n_combinations =", n_combinations[i], "\n")
  x_train = data.table(matrix(rnorm(n[i]*p[i]), nrow = n[i], ncol = p[i]))
  x_test = data.table(matrix(rnorm(10*p[i]), nrow = 10, ncol = p[i]))
  beta = rnorm(p[i])
  y = rnorm(n[i], as.matrix(x_train) %*% beta)
  dt = data.table(cbind(x_train, data.table(y=y)))
  model = lm(y ~ ., data = dt)
  p_mean = mean(y)

  res[[i]] = bench::mark(
    shapr::explain(
      x_train,
      x_test,
      model = model,
      approach = "empirical",
      prediction_zero = p_mean,
      n_combinations = n_combinations[i]
    )
  )
}

devtools::load_all()
res2 = list()
for (i in seq_along(n)) {


  set.seed(123)
  cat("n =", n[i], "p =", p[i], "n_combinations =", n_combinations[i], "\n")
  x_train = data.table(matrix(rnorm(n[i] * p[i]), nrow = n[i], ncol = p[i]))
  x_test = data.table(matrix(rnorm(10 * p[i]), nrow = 10, ncol = p[i]))
  beta = rnorm(p[i])
  y = rnorm(n[i], as.matrix(x_train) %*% beta)
  dt = data.table(cbind(x_train, data.table(y = y)))
  model = lm(y ~ ., data = dt)
  p_mean = mean(y)

  res2[[i]] = bench::mark(
    explain(
      x_train,
      x_test,
      model = model,
      approach = "empirical",
      prediction_zero = p_mean,
      n_combinations = n_combinations[i]
    )
  )
}

saveRDS(res, "inst/scripts/testing_samling_ncombinations.rds")
saveRDS(res2, "inst/scripts/testing_samling_ncombinations2.rds")
