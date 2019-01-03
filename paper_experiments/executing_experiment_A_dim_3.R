
library(doSNOW)
library(foreach)
cl <- makeCluster(5,outfile="")
registerDoSNOW(cl)
seed.vec <- 8:10 + 1234 # We fix this seed
source.local <- TRUE

rho.vec <- seq(0,0.98,length.out=50)[c(seq(1,50,by=2),seq(2,50,by=2))]

progress <- function(n) cat(sprintf("task %d is complete\n", n))
opts <- list(progress=progress)

bb = foreach(rho = rho.vec, .options.snow = opts, .errorhandling = 'pass') %dopar% {
    for (this.seed in seed.vec){
        source("paper_experiments/experiment_A_dim_3_Linear_Linear_Gaussian.R",local = source.local)
    }
    paste0("Just finished computation for rho = ",rho," with seed ",this.seed)
}


