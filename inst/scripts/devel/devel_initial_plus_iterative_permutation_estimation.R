

devtools::load_all()

library(xgboost)

data("airquality")
data <- data.table::as.data.table(airquality)
data <- data[complete.cases(data), ]

x_var <- c("Solar.R", "Wind", "Temp", "Month","Day")
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

#### Set parameters

#### parameters in explain
approach = "ctree"
prediction_zero = p0
shap_approach = "permutation"
paired_shap_sampling = FALSE
n_combinations = NULL
n_permutations = NULL
group = NULL
n_samples = 1e3
n_batches = NULL
seed = 1
keep_samp_for_vS = FALSE
predict_model = NULL
get_model_specs = NULL
MSEv_uniform_comb_weights = TRUE
timing = TRUE


# Gets and check feature specs from the model
feature_specs <- get_feature_specs(get_model_specs, model)


# Sets up and organizes input parameters
# Checks the input parameters and their compatability
# Checks data/model compatability
internal <- shapr:::setup(
  x_train = x_train,
  x_explain = x_explain,
  approach = approach,
  shap_approach = shap_approach,
  paired_shap_sampling = paired_shap_sampling,
  prediction_zero = prediction_zero,
  n_combinations = n_combinations,
  n_permutations = n_permutations,
  group = group,
  n_samples = n_samples,
  n_batches = n_batches,
  seed = seed,
  keep_samp_for_vS = keep_samp_for_vS,
  feature_specs = feature_specs,
  MSEv_uniform_comb_weights = MSEv_uniform_comb_weights,
  timing = timing
)

#### here I first call treeshap to get the initial scores
predict(model,as.matrix(x_explain))
predict(model,as.matrix(x_explain),predcontrib = TRUE)

# Here I run an alternative initial, very approximative estimation of the shapley values
# using the permutation approach with just one or two permutations in total

kernel_exact <-explain(model = model,
                       x_explain = x_explain,
                       x_train = x_train,
                       approach = "ctree",
                       shap_approach = "kernel",
                       prediction_zero = p0)


initial_permute <-explain(model = model,
                          x_explain = x_explain,
                          x_train = x_train,
                          approach = "ctree",
                          shap_approach = "permutation",
                          prediction_zero = p0,
                          n_permutations = 100)

### Here I will run a for loop, where I
# 1. Decide which feature to update the estimate of (based on either size of absolute shapley value
# potential for improvement in variance of another sample sd/n - sd/(n+1) or so)
# 2. sample a permutation (and also include the reverse) for that feature
# 3. compute the v(S) and v(S+i) for that feature (if they have not already been estimated)
# 4. then compute their difference, and update the estimate of the shapley value for that feature
# 5. finally I compute the variance of the shapley value of each feature
#



# For now we assume that we start from scracth estimating the Shapley values properly here (i.e., that the initial_permute is ONLY used to get the initial rankings, which may exclude some features
# entirely from the remaining computations.

# Maximum number of features to compute Shapley values for properly
top_k <- 3
m <- length(x_var)
current_ranks_list <- list()
n_explain <- nrow(x_explain)

# Generates the permutations






update_these_features <- names(sort(colMeans(abs(initial_permute$shapley_values[,-1])),decreasing = TRUE))[1:top_k]

no_computed_S <- 0
current_updates_per_features <- rep(0,m)

shapley_contrib_list <- list()
for(j in seq_len(m)){
  shapley_contrib_list[[j]] <- numeric(0)
}
shapley_dt0 <- data.table(matrix(as.numeric(NA),nrow = n_explain,ncol = m))
shapley_dt0_sd <- data.table(matrix(0,nrow = n_explain,ncol = m))
names(shapley_dt0) <- names(shapley_dt0_sd) <- x_var


current_feats <- NULL#list(integer(0),seq(m))
vS_all <- NULL
predict_model <- get_predict_model(
  predict_model = NULL,
  model = model
)

internal <- setup_approach(internal, model = model, predict_model = predict_model)

next_feature_update <- 1
t_convergence_threshold <- 0.01

set.seed(123)
#for(j in seq_len(m)){
converged <- FALSE
counter <- 0
while (converged == FALSE){
  j <- next_feature_update

  perm <- sample(seq_len(m))
  rev_perm <- rev(perm)

  pos_j <- which(perm==j)
  if(pos_j %in% c(1,m)){
    feats_perm <- list(j,integer(0),
                       seq(m),seq(m)[-j])
  } else {
    feats_perm <- list(sort(perm[seq(pos_j)]),sort(perm[seq(pos_j-1)]),
                       sort(perm[seq(pos_j,m)]),sort(perm[seq(pos_j+1,m)]))
  }

  current_feats <- unique(c(current_feats,feats_perm))

  X <- data.table(features = current_feats)
  S <- feature_matrix_cpp(
    features = c(list(integer(0)),X[["features"]]),
    m = m
  )[-1,]

  internal$objects$X <- X
  internal$objects$S <- S
  internal$parameters$n_combinations <- Inf # Need a large number for prepare_data not to mess up things with max_id_combinations

  no_new_S <- length(current_feats)-no_computed_S

  if(no_new_S > 0){
    index_features <- seq(no_computed_S+1,length(current_feats))
    new_vS=batch_compute_vS(
      S = index_features,
      internal = internal,
      model = model,
      predict_model = predict_model
    )
    new_vS[,id_combination:=NULL]

    empty_S <- which(sapply(current_feats[index_features],length)==0)
    if(length(empty_S)>0){
      new_vS[empty_S,] <- p0
    }

    vS_all <- rbind(vS_all,new_vS)
  }

  no_computed_S <- nrow(S)

  these_vS_all_rows <- match(feats_perm,current_feats)

  contrib_perm <- unlist(vS_all[these_vS_all_rows[1],]-vS_all[these_vS_all_rows[2],])
  contrib_revperm <- unlist(vS_all[these_vS_all_rows[3],]-vS_all[these_vS_all_rows[4],])
  shapley_contrib_list[[j]] <- rbind(shapley_contrib_list[[j]],contrib_perm,contrib_revperm)

  shapley_contrib_mean <- apply(shapley_contrib_list[[j]],2,mean)
  shapley_contrib_sd <- apply(shapley_contrib_list[[j]],2,sd)


  shapley_dt0[,j] <- shapley_contrib_mean
  shapley_dt0_sd[,j] <- shapley_contrib_sd

  matrix1 <- format(as.matrix(round(shapley_dt0,2)),width=2,justify = "right")
  matrix2 <- format(round(as.matrix(shapley_dt0_sd)/rep(sqrt(current_updates_per_features)),2),width=2,justify = "right")

  shapley_dt_with_sd <- as.data.table(matrix(paste(matrix1, " (", matrix2, ")", sep = ""), nrow = n_explain))
  names(shapley_dt_with_sd) <- x_var
  #print(shapley_dt_with_sd)

  #print(shapley_dt)

  current_updates_per_features[j] <- current_updates_per_features[j]+1

  if(any(current_updates_per_features == 0)){
    next_feature_update <- which(current_updates_per_features == 0)[1]
  } else {
    avg_sd <- colMeans(shapley_dt0_sd)
    avg_sd_scaled <- avg_sd/sqrt(current_updates_per_features)
    next_feature_update <- unname(which.max(avg_sd_scaled))
#    print(avg_sd_scaled)
#    Sys.sleep(1)

    range_per_obs <- apply(shapley_dt0,1,function(x)diff(range(x)))
    max_range <- max(range_per_obs)

    estimated_total_samples_per_feature <- (avg_sd/(t_convergence_threshold*max_range))^2
#    print(paste0("Estimated number of samples for highest variance variable: ",max(estimated_total_samples_per_feature)))

    #print(current_updates_per_features)

    if(max(avg_sd_scaled) < t_convergence_threshold*max_range){
      converged <- TRUE
    }

    if(no_computed_S > 100){
      converged <- TRUE
    }


  }
  counter <- counter + 1

  if (counter %% 100 == 0) {
    print(counter)
    print(shapley_dt_with_sd)
  }

}








internal <- setup_computation(internal, model, predict_model)

timing_list$setup_computation <- Sys.time()


# Compute the v(S):
# Get the samples for the conditional distributions with the specified approach
# Predict with these samples
# Perform MC integration on these to estimate the conditional expectation (v(S))
vS_list <- compute_vS(internal, model, predict_model)

timing_list$compute_vS <- Sys.time()


# Compute Shapley values based on conditional expectations (v(S))
# Organize function output
output <- finalize_explanation(
  vS_list = vS_list,
  internal = internal
)

timing_list$shapley_computation <- Sys.time()

if (timing == TRUE) {
  output$timing <- compute_time(timing_list)
}

# Temporary to avoid failing tests

output$internal$objects$id_combination_mapper_dt <- NULL
output$internal$objects$cols_per_horizon <- NULL
output$internal$objects$W_list <- NULL

return(output)


### TO BE DELETED ENDS ####
# Presample a large number og ranks here instead and use them one by one
tot_no_permutations <- factorial(m)
ranks = sample.int(tot_no_permutations, n_permutations, replace = FALSE)-1


S <- NULL
X_full <- data.table(id_combination = 1:2,
                     features = list(NULL,seq(m)),
                     n_features = c(0,m),
                     approach = as.character(rep(NA,2)),
                     batch = c(NA,1))

X_tmp <- data.table(features = list())

### TO BE DELETED ####

