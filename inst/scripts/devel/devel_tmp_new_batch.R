


explainer <- explain_setup(
  x_test,
  approach = c("empirical","empirical","gaussian","copula"),
  explainer = explainer,
  prediction_zero = p,
  n_batches = 4
)


explainer$approach


n_batches = 5


explainer$X[!(n_features %in% c(0,explainer$n_features)),approach:=explainer$approach[n_features]]
batch_count_dt <- explainer$X[!is.na(approach),list(n_batches_per_approach=pmax(1,round(.N/(explainer$n_combinations-2)*n_batches))),by=approach]
Xlist <- list()
approach_vec <- batch_count_dt[,approach]
n_batch_vec <- batch_count_dt[,n_batches_per_approach]
batch_counter <- 0
for(i in seq_along(approach_vec)){
  Xlist[[i]]=explainer$X[approach==approach_vec[i]][,randomorder:=sample(.N)][order(randomorder)][order(shapley_weight),batch:=ceiling(.I/.N*n_batch_vec[i])+batch_counter]
  batch_counter <- Xlist[[i]][,max(batch)]
}

explainer$X <- rbindlist(Xlist)[randomorder:=NULL]




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
