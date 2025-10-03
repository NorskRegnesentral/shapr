
# Manual implementation of the explain function in shapr, allowing to pass
# coalition_list: A list of the coalitions to evaluate
# R_D: The weight matrix, which we multiply by the v(S) vector/matrix to compute the Shapley values (i.e. R_D[i,j] is the weight for coalition j for feature i)
explain_manual <- function(
    model,
    x_explain,
    x_train,
    approach,
    phi0,
    group = NULL,
    n_MC_samples = 1e3,
    seed = 1,
    coalition_list,
    R_D, # R_D in (7) of Aas et al. (2021)
    coalition_approach_dt = NULL, # EXPERIMENTAL: data.table with columns "coalitions_str" and "approach_new"
    ...
){

  internal <- shapr:::setup(
    x_train = x_train,
    x_explain = x_explain,
    approach = approach,
    phi0 = phi0,
    max_n_coalitions = NULL,
    group = group,
    n_MC_samples = n_MC_samples,
    seed = seed,
    feature_specs = shapr:::get_feature_specs(NULL, model),
    verbose = NULL,
    iterative = FALSE,
    ...
  )

  predict_model <- shapr:::get_predict_model(predict_model = NULL, model = model)

  # Overwrite internals

  internal <- setup_approach(internal, model = model, predict_model = predict_model)

  # Manual analogue to shapley_setup
  iter <- 1

  dt_coalitions <- data.table(coalitions = coalition_list)

  X <- shapr:::create_coalition_table(m = internal$parameters$n_shapley_values,
                                      exact = TRUE,
                                      approach0 = internal$parameters$approach,
                                      coal_feature_list = internal$objects$coal_feature_list,
                                      dt_valid_causal_coalitions = dt_coalitions)


  # EXPERIMENTAL: Overwriting the set approach using the coalition_approach_dt
  # which specifies the approach to use for each specific coalition
  if(!is.null(coalition_approach_dt)){
    X <- merge(X,coalition_approach_dt,by="coalitions_str",all.x=TRUE,all.y=FALSE)

    X[!is.na(approach_new),approach:=approach_new]

    X[,approach_new:=NULL]

    setorder(X,id_coalition)
    setkey(X,coalition_size)
  }

  ## Get feature matrix ---------
  S <- shapr:::coalition_matrix_cpp(
    coalitions = X[["features"]],
    m = internal$parameters$n_features
  )


  internal$iter_list[[iter]]$X <- X
  internal$iter_list[[iter]]$W <- R_D
  internal$iter_list[[iter]]$S <- S
  internal$iter_list[[iter]]$S_batch <- shapr:::create_S_batch(internal)

  # Compute the vS
  vS_list <- compute_vS(internal, model, predict_model)

  processed_vS_list <- shapr:::postprocess_vS_list(
    vS_list = vS_list,
    internal = internal
  )

  # Compute the Shapley values
  dt_shapley_est <- shapr:::compute_shapley(internal, processed_vS_list$dt_vS)

  return(dt_shapley_est[])
}

library(xgboost)
library(data.table)

data("airquality")
data <- data.table::as.data.table(airquality)
data <- data[complete.cases(data), ]

x_var <- c("Solar.R", "Wind", "Temp", "Month")
y_var <- "Ozone"

ind_x_explain <- 1:6
x_train <- data[-ind_x_explain, ..x_var]
y_train <- data[-ind_x_explain, get(y_var)]
x_explain <- data[ind_x_explain, ..x_var]

# Looking at the dependence between the features
cor(x_train)

# Fitting a basic xgboost model to the training data
model <- xgboost(
  data = as.matrix(x_train),
  label = y_train,
  nround = 20,
  verbose = FALSE
)

# Specifying the phi_0, i.e. the expected prediction without any features
p0 <- mean(y_train)


causal_ordering = list("Solar.R","Wind")#list("A", "B") # C is free

exp_gaussian_causal <- explain(
  model = model,
  x_explain = x_explain,
  x_train = x_train,
  approach = "gaussian",
  causal_ordering = causal_ordering,
  asymmetric = TRUE,
  phi0 = p0
)

print(exp_gaussian_causal)


### Manual calculation

# Assume we are given X, S and W

X <- exp_gaussian_causla$internal$objects$X
S <- exp_gaussian$internal$objects$S
W <- exp_gaussian$internal$objects$W

# Settings


approach <- "gaussian"
coalition_list <- list(numeric(0), c(1,3), c(1,3,4), c(1,2,4), c(4), c(3), c(1,2,3,4)) #exp_gaussian$internal$objects$X$coalitions[c(1,(c(6,8,11,3,2)),12)]
R_D <- W[,c(1,(c(6,8,11,3,2)),12)]
phi0  = p0


explain_manual(model = model,
               x_explain = x_explain,
               x_train = x_train,
               approach = approach,
               phi0 = p0,
               coalition_list = coalition_list,
               R_D = R_D
)

# Sanity check using all coaltions

# Using ctree without node sampling for full reproducability
exp_ctree_full <- explain(
  model = model,
  x_explain = x_explain,
  x_train = x_train,
  approach = "ctree",
  ctree.sample = FALSE,
  phi0 = p0
)


coalition_list <- list(numeric(0), c(1), c(2), c(3), c(4), c(1,2), c(1,3), c(1,4), c(2,3), c(2,4), c(3,4), c(1,2,3), c(1,2,4), c(1,3,4), c(2,3,4), c(1,2,3,4))
R_D <- exp_ctree_full$internal$objects$W

explain_manual(model = model,
               x_explain = x_explain,
               x_train = x_train,
               approach = "ctree",
               phi0 = p0,
               coalition_list = coalition_list,
               R_D = R_D,
               ctree.sample = FALSE
)

print(exp_ctree_full)

# Yes, identical

#### So what if consider groups of features

group <- list(A = c("Solar.R","Wind"),
              B = "Temp",
              C = "Month")

exp_ctree_full_group <- explain(
  model = model,
  x_explain = x_explain,
  x_train = x_train,
  approach = "ctree",
  ctree.sample = FALSE,
  group = group,
  phi0 = p0
)

coalition_list <- exp_ctree_full_group$internal$objects$X$coalitions #list(numeric(0), c(1), c(2), c(3), c(4), c(1,2), c(1,3), c(1,4), c(2,3), c(2,4), c(3,4), c(1,2,3), c(1,2,4), c(1,3,4), c(2,3,4), c(1,2,3,4))
R_D <- exp_ctree_full_group$internal$objects$W

explain_manual(model = model,
               x_explain = x_explain,
               x_train = x_train,
               approach = "ctree",
               phi0 = p0,
               coalition_list = coalition_list,
               R_D = R_D,
               ctree.sample = FALSE,
               group = group
)

print(exp_ctree_full_group)



