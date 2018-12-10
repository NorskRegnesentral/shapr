
### Running experiment 3 for different rho values

rho.vec <- seq(0,0.9,0.1)

for (rho in rho.vec){ # Should probably be paralellized
    source("paper_experiments/experiment_3_PiecewiseConstant_XGBoost_Gaussian.R")

    print(paste0("Just finished computation for rho = ",rho))
}



