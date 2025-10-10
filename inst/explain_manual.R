
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

parse_coalitions <- function(shap_names, coalition_str) {
  result <- vector("list", length(coalition_str))

  for (i in seq_along(coalition_str)) {
    if (coalition_str[i] == "Ø") {
      result[[i]] <- numeric()
    } else {
      components <- strsplit(coalition_str[i], "_")[[1]]
      result[[i]] <- match(components, shap_names)
    }
  }

  return(result)
}

### Jeroens functions ###



R_D_Matrix <- function(Genes,
                       Status,
                       Confounders,
                       Ordering_between = NULL,
                       Ordering_within = NULL,
                       verbose = TRUE) {


  ## Control Statements ##
  valid_groups <- c("Genes", "Status", "Confounders")

  if (!is.null(Ordering_between)) {
    if (!is.list(Ordering_between)) stop("`Ordering_between` must be a list or NULL.")
    if (length(Ordering_between) < 2) {
      stop("Ordering should contain at least one level of hierarchy,
           i.e. the list should have at least two elements")
    }
    all_elements <- unlist(Ordering_between)
    if (!all(all_elements %in% valid_groups)) {
      stop("`Ordering_between` can only contain 'Genes', 'Status', and 'Confounders'.")
    }
    if (any(duplicated(all_elements))) {
      dup_elems <- unique(all_elements[duplicated(all_elements)])
      stop(paste0("`ordering` contains duplicated group(s): ",
                  paste(dup_elems, collapse = ", "), "."))
    }
  }

  ## define the allowed subsets ##
  all_values <- c(Genes, Status, Confounders)
  group_map <- list(Genes = Genes, Status = Status, Confounders = Confounders)

  all_subsets <- function(x) {
    unlist(lapply(0:length(x), function(k) combn(x, k, simplify = FALSE)), recursive = FALSE)
  }

  subsets_all <- all_subsets(all_values)

  valid_subset <- function(subset) {
    if (is.null(Ordering_between)) return(TRUE)
    for (k in seq_along(Ordering_between)) {
      current_groups <- unlist(group_map[Ordering_between[[k]]])
      if (any(current_groups %in% subset)) {
        required_prior <- unlist(group_map[unlist(Ordering_between[seq_len(k - 1)])])
        if (length(required_prior) > 0 && !all(required_prior %in% subset))
          return(FALSE)
      }
    }
    TRUE
  }

  if (is.null(Ordering_between)) {
    Subsets_Constrained <- subsets_all
  } else {
    Ordering_between <- lapply(Ordering_between, as.character)
    Subsets_Constrained <- Filter(valid_subset, subsets_all)
  }

  subset_names <- sapply(Subsets_Constrained, function(s) {
    if (length(s) == 0) return("Ø")
    paste(s, collapse = "_")
  })

  ## define empty R_D matrix ##
  Q_df <- as.data.frame(matrix(NA, nrow = length(all_values), ncol = length(Subsets_Constrained),
                               dimnames = list(all_values, subset_names)))

  convert_map <- c(Genes = "M1", Status = "M2", Confounders = "M3")


  ## Compute Q-matrix ##
  for (var in all_values) {

    if (verbose) {
      message("\n===============================")
      message("Variable of interest: ", var)
      message("===============================")
    }

    for (j in seq_along(Subsets_Constrained)) {
      subset <- Subsets_Constrained[[j]]
      subset_name <- if (length(subset) == 0) "Ø" else paste(subset, collapse = "_")

      subset_minus_var <- setdiff(subset, var)
      subset_plus_var  <- union(subset, var)
      remaining_minus_var <- setdiff(all_values, union(subset, var))

      # check validity of subset_minus_var and subset_plus_var (weight -> 0 if invalid)
      valid_minus <- valid_subset(subset_minus_var)
      valid_plus  <- valid_subset(subset_plus_var)

      if (!valid_minus || !valid_plus) {
        if (verbose) {
          message("Subset ", subset_name, " → invalid (subset violates ordering), Q = 0")
        }
        Q_df[var, j] <- 0
        next
      }

      # Build M1/M2/M3 sets
      M1_in <- intersect(subset_minus_var, Genes)
      M2_in <- intersect(subset_minus_var, Status)
      M3_in <- intersect(subset_minus_var, Confounders)

      M1_out <- intersect(remaining_minus_var, Genes)
      M2_out <- intersect(remaining_minus_var, Status)
      M3_out <- intersect(remaining_minus_var, Confounders)

      # Ordering adjustment
      ordering_in <- NULL
      ordering_out <- NULL
      if (!is.null(Ordering_between)) {
        ordering_in <- Filter(function(grp) any(unlist(group_map[grp]) %in% subset_minus_var), Ordering_between)
        ordering_out <- Filter(function(grp) any(unlist(group_map[grp]) %in% remaining_minus_var), Ordering_between)
        ordering_in <- lapply(ordering_in, function(x) convert_map[x])
        ordering_out <- lapply(ordering_out, function(x) convert_map[x])
      }

      # Compute Tot_orders
      val_in <- .Tot_orders(M1_in, M2_in, M3_in, ordering = ordering_in)
      val_out <- .Tot_orders(M1_out, M2_out, M3_out, ordering = ordering_out)

      # Print debug info
      if (verbose) {
        message("Subset: {", subset_name, "}")
        message("  Inside subset  (subset_minus_var): {", paste(subset_minus_var, collapse = ", "), "}")
        message("  Remaining subset (remaining_minus_var): {", paste(remaining_minus_var, collapse = ", "), "}")
        message("  .Tot_orders in subset  = ", val_in)
        message("  .Tot_orders out subset = ", val_out)
        message("  => Product = ", val_in * val_out)
      }

      Q_df[var, j] <- val_in * val_out
      if (!(var %in% subset)) {
        Q_df[var, j] <- -1 * Q_df[var, j]
      }
    }
  }
  ordering_Convert <- lapply(Ordering_between, function(x) convert_map[x])
  TotOrders <- .Tot_orders(M1 = Genes, M2 = Status, M3 = Confounders, ordering = ordering_Convert)
  Q_df <- Q_df/TotOrders # normalize to weights
  Q_df <- rbind(intercept = c(1, rep(0, ncol(Q_df) - 1)), Q_df) # include intercept term to include average across all predictions

  return(Q_df)
}

######################## -- Helper Functions -- ########################

.Tot_orders <- function(M1, M2, M3, ordering = NULL) {

  valid_groups <- c("M1", "M2", "M3")
  group_sizes <- list(M1 = length(M1), M2 = length(M2), M3 = length(M3))
  p_tot <- sum(unlist(group_sizes))

  if (p_tot == 0) return(1)  # if no variables, only 1 ordering possible

  if (!is.null(ordering)) {
    if (!is.list(ordering)) stop("`ordering` must be a list or NULL.")
    all_elements <- unlist(ordering)
    if (!all(all_elements %in% valid_groups))
      stop("`ordering` can only contain 'M1', 'M2', and 'M3'.")
    if (any(duplicated(all_elements))) {
      dup_elems <- unique(all_elements[duplicated(all_elements)])
      stop(paste0("`ordering` contains duplicated group(s): ",
                  paste(dup_elems, collapse = ", "), "."))
    }
    ordering <- lapply(ordering, as.character)
  }

  # Case 0: No ordering restrictions
  if (is.null(ordering)) {
    return(exp(lfactorial(p_tot)))
  }

  # Case 1: Simple hierarchy (e.g., list("M1","M2"))
  if (length(ordering) == 2 &&
      all(sapply(ordering, function(x) length(x) == 1))) {
    sizes <- unlist(group_sizes[unlist(ordering)])
    return(exp(lfactorial(sizes[1]) + lfactorial(sizes[2]) +
                 lfactorial(p_tot) - lfactorial(sum(sizes))))
  }

  # Case 2: Full hierarchy (e.g., list("M1","M2","M3"))
  if (length(ordering) == 3 &&
      all(sapply(ordering, function(x) length(x) == 1))) {
    sizes <- unlist(group_sizes[unlist(ordering)])
    return(exp(sum(lfactorial(sizes))))
  }

  # Case 3: Partial block hierarchy (combined)
  if (length(ordering) == 2 &&
      any(sapply(ordering, length) > 1) &&
      any(sapply(ordering, length) == 1)) {
    block_sizes <- c(
      sum(unlist(group_sizes[ordering[[1]]])),
      sum(unlist(group_sizes[ordering[[2]]]))
    )
    return(exp(sum(lfactorial(block_sizes))))
  }

  # Default fallback (unrecognized ordering)
  return(exp(lfactorial(p_tot)))
}



library(xgboost)
library(data.table)
library(shapr)

data("airquality")
data <- data.table::as.data.table(airquality)
data <- data[complete.cases(data), ]

# Renaming for convenience
names(data)[2:5] <- c("G","S0","C1","C2")

x_var <- c("G","S0","C1","C2") #c("Solar.R", "Wind", "Temp", "Month")
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

### Jeroens code and example


# Parsing function to get coalition_list
parse_coalitions <- function(shap_names, coalition_str) {
  result <- vector("list", length(coalition_str))

  for (i in seq_along(coalition_str)) {
    if (coalition_str[i] == "Ø") {
      result[[i]] <- numeric()
    } else {
      components <- strsplit(coalition_str[i], "_")[[1]]
      result[[i]] <- match(components, shap_names)
    }
  }

  return(result)
}

# Example usage with the current data

library(xgboost)
library(data.table)
library(shapr)

data("airquality")
data <- data.table::as.data.table(airquality)
data <- data[complete.cases(data), ]

# Renaming for convenience
names(data)[2:5] <- c("G","S0","C1","C2")

x_var <- c("G","S0","C1","C2") #c("Solar.R", "Wind", "Temp", "Month")
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


Genes <- c("G")
Status <- c("S0")
Confounders <- c("C1","C2")

#### 1. hierarchy of interest: Genes before disease status ####
R_D <- R_D_Matrix(Genes, Status, Confounders,
                 Ordering_between = list("Genes", "Status"),
                 verbose = FALSE)

shap_names <- names(x_explain)

coalition_list <- parse_coalitions(shap_names, names(R_D))

explain_manual(model = model,
               x_explain = x_explain,
               x_train = x_train,
               approach = "gaussian",
               phi0 = p0,
               coalition_list = coalition_list,
               R_D = as.matrix(R_D)
)



