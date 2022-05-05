


explainer <- explain_setup(
  x_test,
  approach = c("empirical","empirical","gaussian","copula"),
  explainer = explainer,
  prediction_zero = p,
  n_batches = 4
)


explainer$approach = c("empirical","empirical","gaussian","copula")




explainer$X[,randomorder:=sample(.N)]
setorder(explainer$X,randomorder)

aa <- explainer$X[!is.na(approach)][order(randomorder)][order(shapley_weight),batch:=ceiling(.I/.N*n_batches_per_approach)]

aa <- explainer$X[!is.na(approach)][order(randomorder)][order(shapley_weight),batch:=ceiling(.I/.N*5)]


bb <- explainer$X[!is.na(approach)][rank(shapley_weight,ties.method = "random")]


explainer$X[]


n_batches <- max(1, floor(length(index_S) / no_samples * n_batches))


S_per_apprach <-

  findInterval(x, quantile(x,type=5), rightmost.closed=TRUE)

# It is fast
set.seed(1)
DT <- data.table(x=rep(rnorm(5),2))

library(microbenchmark)


microbenchmark(
  order = DT[order(x),bin:=ceiling(.I/.N*5)],
  findInterval = DT[, b2 :=findInterval(x, quantile(x,type=5), rightmost.closed=TRUE)],times=20 )
