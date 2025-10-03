
# Manual implementation of the explain function in shapr, using a range of internal functions from the shapr package.
# This manual implementation allows us to pass
# coalition_list: A list of the coalitions to evaluate
# R_D: The weight matrix, which we multiply by the v(S) vector/matrix to compute the Shapley values (i.e. R_D[i+1,j] is the weight for coalition j for feature i)
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
library(shapr)

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

# Settings

# Sanity check using all coaltions

# Using ctree without node sampling for full reproducability
exp_ctree_full <- explain(
  model = model,
  x_explain = x_explain,
  x_train = x_train,
  approach = "ctree",
  ctree.sample = FALSE,
  phi0 = p0,
)


coalition_list <- list(numeric(0), c(1), c(2), c(3), c(4), c(1,2), c(1,3), c(1,4), c(2,3), c(2,4), c(3,4), c(1,2,3), c(1,2,4), c(1,3,4), c(2,3,4), c(1,2,3,4))
R_D <- exp_ctree_full$internal$objects$W

exp_ctree_full_man <- explain_manual(model = model,
                                     x_explain = x_explain,
                                     x_train = x_train,
                                     approach = "ctree",
                                     phi0 = p0,
                                     coalition_list = coalition_list,
                                     R_D = R_D,
                                     ctree.sample = FALSE
)

exp_ctree_full_man

# Checking equality
all.equal(exp_ctree_full_man,
          exp_ctree_full$shapley_values_est[,-1])

# Yes, identical

### Doing the same for groups:


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

# Note that coalitions group now refer to coaltions of the groups (where A=1, B=2, C=3 in the above example)
coalition_list <- exp_ctree_full_group$internal$objects$X$coalitions
R_D <- exp_ctree_full_group$internal$objects$W

exp_ctree_full_group_man <- explain_manual(model = model,
                                           x_explain = x_explain,
                                           x_train = x_train,
                                           approach = "ctree",
                                           phi0 = p0,
                                           coalition_list = coalition_list,
                                           R_D = R_D,
                                           ctree.sample = FALSE,
                                           group = group
)

exp_ctree_full_group_man

# Checking equality
all.equal(exp_ctree_full_group_man,
          exp_ctree_full_group$shapley_values_est[,-1])


### Let also check using just a few of the coalitions

exp_ctree_samp_group <- explain(
  model = model,
  x_explain = x_explain,
  x_train = x_train,
  approach = "ctree",
  ctree.sample = FALSE,
  group = group,
  phi0 = p0,
  max_n_coalitions = 4
)

coalition_list <- exp_ctree_samp_group$internal$objects$X$coalitions
R_D <- exp_ctree_samp_group$internal$objects$W

exp_ctree_samp_group_man <- explain_manual(model = model,
                                           x_explain = x_explain,
                                           x_train = x_train,
                                           approach = "ctree",
                                           phi0 = p0,
                                           coalition_list = coalition_list,
                                           R_D = R_D,
                                           ctree.sample = FALSE,
                                           group = group
)

exp_ctree_samp_group_man

# Checking equality
all.equal(exp_ctree_samp_group_man,
          exp_ctree_samp_group$shapley_values_est[,-1])

# Still identical

# Also checking that the order we provide the coalitons in does not matter (except that the empty and full set are first and last)

exp_ctree_samp_group$internal$objects$X$coalitions

coalition_list <- exp_ctree_samp_group$internal$objects$X$coalitions[c(1,3,2,4)]
R_D <- exp_ctree_samp_group$internal$objects$W[,c(1,3,2,4)]

exp_ctree_samp_group_man2 <- explain_manual(model = model,
                                           x_explain = x_explain,
                                           x_train = x_train,
                                           approach = "ctree",
                                           phi0 = p0,
                                           coalition_list = coalition_list,
                                           R_D = R_D,
                                           ctree.sample = FALSE,
                                           group = group
)

exp_ctree_samp_group_man2

# Checking equality
all.equal(exp_ctree_samp_group_man,
          exp_ctree_samp_group_man2)

# TRUE


### And finally a manual example

set.seed(123)


# Note that parallelization and progress bar are still supported
future::plan("multisession", workers = 2) # Increase the number of workers for increased performance with many features
progressr::handlers(global = TRUE)
progressr::handlers("cli") # Using the cli package as backend (recommended for the estimates of the remaining time)


coalition_list <- list(numeric(0), c(1,3), c(1,3,4), c(1,2,4), c(4), c(3), c(1,2,3,4))
R_D <- exp_ctree_full$internal$objects$W[,c(1,sample(2:15,5,replace = FALSE),16)]
phi0  = p0


explain_manual(model = model,
               x_explain = x_explain,
               x_train = x_train,
               approach = "gaussian",
               phi0 = p0,
               coalition_list = coalition_list,
               R_D = R_D
)





