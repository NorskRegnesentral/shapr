### Running experiment 5 for 10 training sets

seed.vec <- 1:10 + 1234 # We fix this seed

for (this.seed in seed.vec){
    source("paper_experiments/experiment_5_PiecewiseConstant_XGBoost_GenHyp10dim_heavytails.R")

    print(paste0("Just finished computation with seed ",this.seed))
}


