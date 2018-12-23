
library(doParallel)
cl <- makeCluster(5)
registerDoParallel(cl)
seed.vec <- 1:5 + 1234 # We fix this seed

rho.vec <- seq(0,0.95,length.out=20)[c(seq(1,20,by=2),seq(2,20,by=2))]

foreach (rho=rho.vec) %dopar%{
    for (this.seed in seed.vec){
        source("paper_experiments/experiment_A_dim_10_Linear_Linear_Gaussian.R")

        print(paste0("Just finished computation for rho = ",rho," with seed ",this.seed))
    }

}

stopCluster(cl)




