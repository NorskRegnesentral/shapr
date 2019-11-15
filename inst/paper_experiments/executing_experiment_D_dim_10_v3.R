rm(list=ls())
library(parallel)
library(foreach)
library(doParallel)
cl <- parallel::makeCluster(1,outfile="")
registerDoParallel(cl)
seed.vec <- 7:10 + 1234 # We fix this seed
source.local <- TRUE


beta.scale.vec <- 1#c(1,2,5,10)
bb = foreach(beta.scale = beta.scale.vec, .errorhandling = 'pass') %dopar% {
    for (this.seed in seed.vec){
        source("paper_experiments/experiment_D_dim_10_PiecewiseConstant_XGBoost_GenHyp.R",local=source.local)
    }
    print(paste0("Just finished computation beta..scale = ",beta.scale," with seed = ",this.seed))
}


