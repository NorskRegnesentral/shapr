
##############
library(doSNOW)
library(foreach)
cl <- makeCluster(5,outfile="")
registerDoSNOW(cl)
seed.vec <- 1:10 + 1234 # We fix this seed
source.local <- TRUE


beta.scale.vec <- c(1,2,5,10)

progress <- function(n) cat(sprintf("task %d is complete\n", n))
opts <- list(progress=progress)

bb = foreach(this.seed = seed.vec,.options.snow = opts, .errorhandling = 'pass') %dopar% {
    for (beta.scale in beta.scale.vec){
        source("paper_experiments/experiment_D_dim_3_PiecewiseConstant_XGBoost_GenHyp.R",local = source.local)
    }
    paste0("Just finished computation beta..scale = ",beta.scale," with seed = ",this.seed)
}

