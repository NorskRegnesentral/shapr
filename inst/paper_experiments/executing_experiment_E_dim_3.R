
##############
library(parallel)
library(foreach)
library(doParallel)
cl <- parallel::makeCluster(5,outfile="")
registerDoParallel(cl)
seed.vec <- 1:10 + 1234 # We fix this seed
source.local <- TRUE


mu.scale.vec <- c(0.5,1,2,3,5,10)

bb = foreach(this.seed = seed.vec, .errorhandling = 'pass') %dopar% {
    for (mu.scale in mu.scale.vec){
        source("paper_experiments/experiment_E_dim_3_Linear_Linear_Gaussianmix.R",local = source.local)
    }
    paste0("Just finished computation mu.scale = ",mu.scale," with seed = ",this.seed)
}

