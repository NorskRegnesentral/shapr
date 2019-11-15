

library(parallel)
library(foreach)
library(doParallel)
cl <- parallel::makeCluster(5,outfile="")
registerDoParallel(cl)
seed.vec <- 1:10 + 1234 # We fix this seed
source.local <- TRUE

rho.vec <- seq(0,0.98,length.out=50)[c(seq(1,50,by=2),seq(2,50,by=2))]


bb = foreach(rho = rho.vec, .errorhandling = 'pass') %dopar% {
    for (this.seed in seed.vec){
        source("paper_experiments/experiment_B_dim_3_PiecewiseConstant_XGBoost_Gaussian.R",local = source.local)
    }
    print(paste0("Just finished computation for rho = ",rho," with seed ",this.seed))
}


