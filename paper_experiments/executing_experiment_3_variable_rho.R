
### Running experiment 3 for different rho values

rho.vec <- c(seq(0.1,0.9,0.1),seq(0.05,0.95,0.1))

for (rho in rho.vec){ # Should probably be paralellized
    source("paper_experiments/experiment_3_PiecewiseConstant_XGBoost_Gaussian.R")

    print(paste0("Just finished computation for rho = ",rho))
}



