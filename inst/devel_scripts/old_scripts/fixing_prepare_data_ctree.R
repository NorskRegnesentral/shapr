
rm(list=ls())

load("inst/devel_scripts/old_scripts/MJ_testing_script_data_prepare_data.RData")

library(shapr)

set.seed(123)
all_trees <- parallel::mclapply(
  X = features,
  FUN = simulateAllTrees,
  x_train = x$x_train,
  comb_indici = x$comb_indici,
  comb_mincriterion = x$comb_mincriterion,
  mincriterion = x$mincriterion,
  minsplit = x$minsplit,
  minbucket = x$minbucket,
  mc.cores = mc_cores_simulateAllTrees,
  mc.set.seed = FALSE,
  use_partykit = "always"
)

mc_cores_sample_ctree <- 2
dt_l <- list()
for (i in seq(n_xtest)) {
  l <- parallel::mclapply(
    X = all_trees,
    FUN = shapr:::sample_ctree,
    n_samples = n_samples,
    x_test = x$x_test[i, , drop = FALSE],
    x_train = x$x_train,
    p = ncol(x$x_test),
    sample = x$sample,
    mc.cores = mc_cores_sample_ctree,
    mc.set.seed = FALSE
  )

  dt_l[[i]] <- data.table::rbindlist(l, idcol = "id_combination")
  dt_l[[i]][, w := 1 / n_samples]
  dt_l[[i]][, id := i]
}

dt <- data.table::rbindlist(dt_l, use.names = TRUE, fill = TRUE)
dt[id_combination %in% c(1, 2^ncol(x$x_test)), w := 1.0]

always <- copy(dt)
on_error <- copy(dt)
never <- copy(dt)

all.equal(always,on_error)
all.equal(always,never)
all.equal(never,on_error)

# Working within simulateAllTrees:

given_ind = features[[5]]
x_train = x$x_train
comb_indici = x$comb_indici
comb_mincriterion = x$comb_mincriterion
mincriterion = x$mincriterion
minsplit = x$minsplit
minbucket = x$minbucket
mc.cores = mc_cores_simulateAllTrees
mc.set.seed = FALSE


### working within sample_ctree #
tree = all_trees[[4]]
x_test = x$x_test[1, , drop = FALSE]
x_train = x$x_train
p = ncol(x$x_test)
sample = x$sample
mc.cores = mc_cores_sample_ctree


