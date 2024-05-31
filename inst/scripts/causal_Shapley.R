pp <- explain(
  model = model,
  x_explain = x_explain,
  x_train = x_train,
  approach = "ctree",
  prediction_zero = p0,
  keep = TRUE,
  n_samples = 10,
  n_batches = 1,
  causal_ordering = list(c(1, 2), 3, 4),
  confounding = FALSE
)
pp$internal$output$dt_samp_for_vS[, .N, by = list(id, id_combination)]
pp$internal$output$dt_samp_for_vS[id == 1 & id_combination == 3]



m <- 10
n_samples <- 50
mu <- rep(1, m)
cov_mat <- cov(matrix(rnorm(n_samples * m), n_samples, m))
x_test <- matrix(MASS::mvrnorm(1, mu, cov_mat), nrow = 1)
cnms <- paste0("x", seq(m))
colnames(x_test) <- cnms
index_given <- c(4, 7)
causal_ordering <- list(c(1:3), c(4:6), c(7:10))
confounding <- c(TRUE, FALSE, TRUE)
r <- sample_causal(
  index_given, n_samples, mu, cov_mat, m, x_test,
  causal_ordering, confounding
)


x_train <- data.table(matrix(MASS::mvrnorm(100, mu, cov_mat), ncol = m))
S <- shapr::feature_matrix_cpp(unlist(lapply(0:m, utils::combn, x = m, simplify = FALSE), recursive = FALSE), m = m)
S <- shapr::feature_matrix_cpp(get_legit_causal_combinations(causal_ordering = causal_ordering), m = m)

get_S_causal_steps(S, causal_ordering, confounding, as_string = TRUE)[[38]]



features <- X$features

features


features[sapply(features, respects_order, causal_ordering = list(c(1, 2, 3), 4:m))]


index <- features[[34]]
index

causal_ordering







dt2 <- dt[check_coalitions_respect_order(features, causal_ordering)]


causal_ordering2 <- causal_ordering


causal_ordering <- explanation$internal$parameters$feature_names

causal_ordering <- list(c("Month", "Wind"), c("Solar.R", "Temp"))
causal_ordering_vec <- unlist(causal_ordering)


causal_ordering_vec_sort <- sort(unlist(causal_ordering))




causal_ordering_vec
internal$objects$feature_specs$labels
Position(causal_ordering_vec, function(x) internal$objects$feature_specs$labels)
Position(function(ith_component) ith_component %in% internal$objects$feature_specs$labels, causal_ordering_vec)
match(causal_ordering_vec, internal$objects$feature_specs$labels)


sapply(seq_along(causal_ordering_vec), function(i) Position(function(ith_component) internal$objects$feature_specs$labels %in% ith_component, causal_ordering_vec[i]))


group_list <- list(
  A = c("Temp", "Month"),
  B = c("Wind", "Solar.R")
)

group_list <- list(
  c("Temp", "Month"),
  c("Wind", "Solar.R")
)

# Use the empirical approach
explanation_group <- explain(
  model = model,
  x_explain = x_explain,
  x_train = x_train,
  approach = "empirical",
  prediction_zero = p0,
  group = group_list
)


group_names <- names(explanation_group$internal$objects$group_num)




causal_ordering_vec_sort <- sort(unlist(causal_ordering))

if (explanation_group$internal$parameters$is_groupwise) {
  # Group-wise Shapley values

  # Want to check if `causal_ordering` is the group names or feature

  n_groups <- internal$parameters$n_groups
  group_names <- names(explanation_group$internal$parameters$group)

  if (is.character(causal_ordering_vec_sort)) {
    # Check that all feature names are included
    if (length(causal_ordering_vec_sort) != n_groups || any(causal_ordering_vec_sort != sort(feature_names))) {
      stop(paste0(
        "When the `causal_ordering` list contains strings, then it most contain all group names (`",
        paste0(group_names, collapse = "`, `"), "`) once."
      ))
    }
  } else if (is.integer(causal_ordering_vec_sort)) {
    # Check that the we have m elements and that they are 1 through m (i.e., no duplicates).
    if (length(causal_ordering_vec_sort) != n_groups || any(sort(causal_ordering_vec_sort) != seq(n_groups))) {
      stop(paste0(
        "When the `causal_ordering` list contains integers, then it most contain all integers from 1 to ",
        n_groups, ", the number of groups, once."
      ))
    }
  } else {
    stop(paste0(
      "The `causal_ordering` list must contain either only integers representing the group",
      "indices or the group names as strings. See the documentation for more details."
    ))
  }
} else {
  # Feature-wise Shapley values
  n_features <- internal$parameters$n_features
  feature_names <- internal$parameters$feature_names

  if (is.character(causal_ordering_vec_sort)) {
    # Check that all feature names are included
    if (length(causal_ordering_vec_sort) != n_features || any(causal_ordering_vec_sort != sort(feature_names))) {
      stop(paste0(
        "When the `causal_ordering` list contains strings, then it most contain all feature names (`",
        paste0(feature_names, collapse = "`, `"), "`) once."
      ))
    }
  } else if (is.integer(causal_ordering_vec_sort)) {
    # Check that the we have m elements and that they are 1 through m (i.e., no duplicates).
    if (length(causal_ordering_vec_sort) != n_features || any(sort(causal_ordering_vec_sort) != seq(n_features))) {
      stop(paste0(
        "When the `causal_ordering` list contains integers, then it most contain all integers from 1 to ",
        n_features, ", the number of features, once."
      ))
    }
  } else {
    stop(paste0(
      "The `causal_ordering` list must contain either only integers representing the feature",
      "indices or the feature names as strings. See the documentation for more details."
    ))
  }
}



causal_ordering


#' Sample conditional Gaussian variables following a causal chain graph with do-calculus.
#'
#' @inheritParams sample_copula
#'
#' @param causal_ordering List of vectors specifying (partial) causal ordering. Each element in
#' the list is a component in the order, which can contain one or more variable indices in a vector.
#' For example, in list(1, c(2, 3)), 2 > 1 and 3 > 1, but 2 and 3 are not comparable.
#' @param confounding Logical vector specifying which variables are affected by confounding.
#' Confounding must be specified globally with a single TRUE / FALSE value for all components,
#' or separately for each causal component in the causal ordering.
#'
#' @return data.table
#'
#' @keywords internal
#'
#' @author Tom Heskes, Ioan Gabriel Bucur
#'
#' @examples
#' m <- 10
#' n_samples <- 50
#' mu <- rep(1, m)
#' cov_mat <- cov(matrix(rnorm(n_samples * m), n_samples, m))
#' x_test <- matrix(MASS::mvrnorm(1, mu, cov_mat), nrow = 1)
#' cnms <- paste0("x", seq(m))
#' colnames(x_test) <- cnms
#' index_given <- c(4, 7)
#' causal_ordering <- list(c(1:3), c(4:6), c(7:10))
#' confounding <- c(TRUE, FALSE, TRUE)
#' r <- shapr:::sample_causal(
#'   index_given, n_samples, mu, cov_mat, m, x_test,
#'   causal_ordering, confounding
#' )
sample_causal <- function(index_given, n_samples, mu, cov_mat, m, x_test, causal_ordering, confounding) {
  # Check input
  stopifnot(is.matrix(x_test))
  stopifnot(is.list(causal_ordering))
  stopifnot(is.logical(confounding))

  if (length(confounding) > 1 && length(confounding) != length(causal_ordering)) {
    stop("Confounding must be specified globally (one value for all components), or separately for each component in the causal ordering.")
  }

  # In case of global confounding value, replicate it across components.
  if (length(confounding) == 1) {
    confounding <- rep(confounding, length(causal_ordering))
  }

  if (!base::setequal(unlist(causal_ordering), seq(m))) {
    stop(paste("Incomplete or incorrect partial causal_ordering specified for", m, "variables"))
  }

  # Handles the unconditional and full conditional separately when predicting
  if (length(index_given) %in% c(0, m)) {
    return(data.table::as.data.table(x_test))
  }


  # index_given = c(1, 6, 9)
  # dependent_ind <- setdiff(1:ncol(S), index_given)
  dependent_ind <- setdiff(seq(m), index_given)
  xall <- data.table(matrix(ncol = m, nrow = n_samples))
  xall[, (index_given) := lapply(x_test[index_given], rep, n_samples)] # Add values from x_test to specified columns in xall

  what_kind <- rep(NA, m)
  what_kind[index_given] <- "given"

  for (i in seq(length(causal_ordering))) {
    print(what_kind)

    # check overlap between dependent_ind and component
    to_be_sampled <- intersect(causal_ordering[[i]], dependent_ind)

    if (length(to_be_sampled) > 0) {
      # condition upon all variables in ancestor components
      to_be_conditioned <- unlist(causal_ordering[0:(i - 1)])

      # back to conditioning if confounding is FALSE or no conditioning if confounding is TRUE
      if (!confounding[i]) {
        # add intervened variables in the same component
        to_be_conditioned <- union(intersect(causal_ordering[[i]], index_given), to_be_conditioned)
      }

      if (length(to_be_conditioned) == 0) {
        # draw new samples from marginal distribution
        newsamples <- as.data.table(mvnfast::rmvn(n_samples, mu = mu[to_be_sampled], sigma = as.matrix(cov_mat[to_be_sampled, to_be_sampled])))
        #newsamples <- create_marginal_data_training(n_explain = nrow(x_test), x_train = x_train, Sbar_features = to_be_sampled, n_samples = 50)


        what_kind[to_be_sampled] <- "marginal"
      } else {
        # compute conditional Gaussian
        C <- cov_mat[to_be_sampled, to_be_conditioned, drop = FALSE]
        D <- cov_mat[to_be_conditioned, to_be_conditioned]
        CDinv <- C %*% solve(D)
        cVar <- cov_mat[to_be_sampled, to_be_sampled] - CDinv %*% t(C)
        if (!isSymmetric(cVar)) {
          cVar <- Matrix::symmpart(cVar)
        }

        # draw new samples from conditional distribution
        mu_sample <- matrix(rep(mu[to_be_sampled], each = n_samples), nrow = n_samples)
        mu_cond <- matrix(rep(mu[to_be_conditioned], each = n_samples), nrow = n_samples)
        cMU <- mu_sample + t(CDinv %*% t(xall[, to_be_conditioned] - mu_cond))
        newsamples <- mvnfast::rmvn(n_samples, mu = matrix(0, 1, length(to_be_sampled)), sigma = as.matrix(cVar))
        newsamples <- newsamples + cMU

        what_kind[to_be_sampled] <- "conditional"
      }


      xall[, (to_be_sampled) := newsamples] # Data table to data table
      # xall[, (to_be_sampled) := split(newsamples, seq_len(ncol(newsamples)))] matrix to data table

      # xall[,to_be_sampled] <- newsamples Matrix version
      # print(c(to_be_sampled, c(0,0,0), to_be_conditioned))
      print(as.data.table(xall))
    }
  }
  what_kind

  # return(what_kind)

  colnames(xall) <- colnames(x_test)
  return(as.data.table(xall))
}

m <- 10
n_samples <- 50
mu <- rep(1, m)
cov_mat <- cov(matrix(rnorm(n_samples * m), n_samples, m))
x_test <- matrix(MASS::mvrnorm(1, mu, cov_mat), nrow = 1)
cnms <- paste0("x", seq(m))
colnames(x_test) <- cnms
index_given <- c(4, 7)
causal_ordering <- list(c(1:3), c(4:6), c(7:10))
confounding <- c(TRUE, FALSE, TRUE)
r <- sample_causal(
  index_given, n_samples, mu, cov_mat, m, x_test,
  causal_ordering, confounding
)

explanation$internal$objects$S
explanation$internal$objects$S









m <- 10
S <- feature_matrix_cpp(unlist(lapply(0:m, utils::combn, x = m, simplify = FALSE), recursive = FALSE), m = m)
causal_ordering <- list(c(1:3), c(4:6), c(7:10))
confounding <- c(FALSE, FALSE, FALSE)
confounding <- c(FALSE, FALSE, FALSE)
apply(what_type(S, causal_ordering, confounding), 2, unique)
View(what_type(S, causal_ordering, confounding))


m <- 5
S <- feature_matrix_cpp(unlist(lapply(0:m, utils::combn, x = m, simplify = FALSE), recursive = FALSE), m = m)
causal_ordering <- list(1:2, 3:5)
confounding <- c(FALSE, FALSE)
what_type(S, causal_ordering, confounding)


# Hvorfor er ikke andre component også marginal her da for S = 5?
m <- 5
causal_ordering <- list(1:2, 3:4, 5)
S <- feature_matrix_cpp(get_legit_causal_combinations(causal_ordering = causal_ordering), m = m)
confounding <- c(TRUE, TRUE, FALSE)
a1 <- what_type(S, causal_ordering, confounding)
a1
SS <- get_S_causal_steps(S, causal_ordering, confounding, as_string = TRUE)
S[3, ]
SS[[3]]

# Effekten av å sette de confounding er at vi får marginal for component 1 når enten feature 1 eller 2 er kjent.
causal_ordering <- list(1:2, 3:4, 5)
confounding <- c(TRUE, FALSE, FALSE)
a2 <- what_type(S, causal_ordering, confounding)
a2

a1

names(a1[a1 == ""])
names(a2[a2 == ""])



sbar <- c(1, 2, 3)

tmp <- sapply(names(a1), function(pattern) grepl(pattern, paste0(sbar, collapse = ","), fixed = TRUE))
plaussible <- rev(names(tmp)[tmp])



# Marginal features
lapply(strsplit(names(a2[a2 == ""]), ","), as.integer)

lapply(strsplit(names(a2[a2 != ""]), ","), as.integer)

a2[a2 == ""]
a2[a2 != ""]
get_legit_causal_combinations(causal_ordering = causal_ordering)






# Hvorfor skjer ikke dette for component 2?
causal_ordering <- list(1:2, 3:4, 5)
confounding <- c(TRUE, TRUE, FALSE)
what_type(S, causal_ordering, confounding)

causal_ordering <- list(1:2, 3:4, 5)
confounding <- c(FALSE, FALSE, TRUE)
what_type(S, causal_ordering, confounding)

m <- 3
S <- feature_matrix_cpp(unlist(lapply(0:m, utils::combn, x = m, simplify = FALSE), recursive = FALSE), m = m)
causal_ordering <- list(1, 2:3)
confounding <- c(TRUE, TRUE)
what_type(S, causal_ordering, confounding)


#
causal_ordering <- list(1, 2, 3)
confounding <- c(TRUE, TRUE, TRUE)
what_type(S, causal_ordering, confounding)

causal_ordering <- list(1, 2, 3)
confounding <- c(FALSE, FALSE, FALSE)
what_type(S, causal_ordering, confounding)

# Cofounder but no causal graph
causal_ordering <- list(1:3)
confounding <- c(TRUE)
what_type(S, causal_ordering, confounding)

# Regular. No Causal graph and no cofounder
causal_ordering <- list(1:3)
confounding <- c(FALSE)
what_type(S, causal_ordering, confounding)


m <- 7
S <- feature_matrix_cpp(unlist(lapply(0:m, utils::combn, x = m, simplify = FALSE), recursive = FALSE), m = m)
causal_ordering <- list(1:2, 3:5, 6:7)
confounding <- c(TRUE, FALSE, TRUE)
what_type(S, causal_ordering, confounding)







m <- 10
n_samples <- 50
mu <- rep(1, m)
cov_mat <- cov(matrix(rnorm(n_samples * m), n_samples, m))
x_test <- matrix(MASS::mvrnorm(1, mu, cov_mat), nrow = 1)
cnms <- paste0("x", seq(m))
colnames(x_test) <- cnms
causal_ordering <- list(c(1:3), c(4:6), c(7:10))
confounding <- c(FALSE, FALSE, TRUE)
index_given <- c(7)
sample_causal(index_given, n_samples, mu, cov_mat, m, x_test, causal_ordering, confounding)








m <- 5
causal_ordering <- list(1:2, 3:4, 5)
S <- feature_matrix_cpp(get_legit_causal_combinations(causal_ordering = causal_ordering), m = m)
S
confounding <- c(TRUE, FALSE, FALSE)
get_S_causal_steps(S, causal_ordering, confounding, as_string = TRUE)

sort(unique(unlist(get_S_causal_steps(S, causal_ordering, confounding, as_string = TRUE))))

SS1 <- get_S_causal_steps(S, causal_ordering, confounding = c(FALSE, FALSE, FALSE), as_string = TRUE)
SS2 <- get_S_causal_steps(S, causal_ordering, confounding = c(TRUE, FALSE, FALSE), as_string = TRUE)
SS3 <- get_S_causal_steps(S, causal_ordering, confounding = c(TRUE, TRUE, FALSE), as_string = TRUE)
SS4 <- get_S_causal_steps(S, causal_ordering, confounding = c(TRUE, TRUE, TRUE), as_string = TRUE)

all.equal(SS1, SS2)
SS1[[2]]
SS2[[2]]
SS1[[3]]
SS2[[3]]

all.equal(SS1, SS3)
SS1[[5]]
SS3[[5]]
SS1[[6]]
SS3[[6]]

all.equal(SS2, SS3)
SS2[[5]]
SS3[[5]]
SS2[[6]]
SS3[[6]]




m <- 11
causal_ordering <- list(3:1, c(8, 4), c(7, 5), 6, 9:10, 11) # All m features must be in this list
all.equal(
  get_legit_causal_combinations(causal_ordering, sort_features_in_coalitions = TRUE),
  dt[check_coalitions_respect_order(features, causal_ordering)]$features
)


causal_ordering <- list(c(1:11))
dt <- data.table::data.table(features = unlist(lapply(0:m, utils::combn, x = m, simplify = FALSE), recursive = FALSE))
dt <- dt[check_coalitions_respect_order(features, causal_ordering)]
dt[, id_combination := .I]
dt[, n_features := length(features[[1]]), id_combination]
dt[, N := .N, n_features]
dt[, shapley_weight := shapr:::shapley_weights(m = m, N = N, n_components = n_features, 10^6)]



m <- 4
causal_ordering <- list(1:2, 3, 4)
confounding <- c(TRUE, TRUE, TRUE)
causal_ordering <- list(1:4)
confounding <- FALSE
S <- feature_matrix_cpp(get_legit_causal_combinations(causal_ordering = causal_ordering), m = m)
S
get_S_causal_steps(S, causal_ordering, confounding, as_string = TRUE)


data("airquality")
data <- data.table::as.data.table(airquality)
data <- data[complete.cases(data), ]

x_var <- c("Solar.R", "Wind", "Temp", "Month")
y_var <- "Ozone"

ind_x_explain <- 1:6
x_train <- data[-ind_x_explain, ..x_var]
x_train
x_explain <- data[ind_x_explain, ..x_var]
x_explain
create_marginal_data(x_train = x_train, Sbar_features = c(1, 4), n_samples = 10)




id_comb_now <- 2

chain_components_all <- get_S_causal_steps(S, causal_ordering, confounding, as_string = TRUE)
chain_components <- get_S_causal_steps(S, causal_ordering, confounding, as_string = FALSE)
chain_components_batch <- chain_components[[id_comb_now]]

n_samples <- 5
n_explain <- nrow(x_explain)
n_features <- ncol(x_explain)

# Create the empty data table which we are to fill up with the Monte Carlo samples
# dt = x_explain[rep(seq(n_explain), each = n_samples)]
dt <- data.table(matrix(nrow = n_explain * n_samples, ncol = n_features))
colnames(dt) <- colnames(x_explain)

# Add the samples we condition on
S_names <- names(dt)[as.logical(S[id_comb_now, ])]
dt[, (S_names) := x_explain[rep(seq(n_explain), each = n_samples), .SD, .SDcols = S_names]]

# Create a duplicate internal list.
# This is needed as prepare_data extracts, e.g., x_explain from the internal list,
# but in this iterative causal chain component structure, we have to manipulate the
# internal list to compute what we are interested in.
# That is, we change x_explain to dt (and update it for each chain component), such that we can re-use
# the prepare_data function to generate MC samples from P(Sbar_i | S_i), where i represents that the S and
# Sbar changes for each chain component.
internal_copy <- copy(internal)
internal_copy$data$x_explain <- dt
internal_copy$parameters$n_explain <- nrow(dt)

# Generate one MC sample for each observation, as each observation is part of a chain of sampling steps to
# create the full MC sample. Otherwise, we get n_samples^(n_components) MC samples in the end.
internal_copy$parameters$n_samples <- 1

# Then sample the remaining features based on the chain of components
chain_components_batch_idx <- 1
for (chain_components_batch_idx in seq_along(chain_components_batch)) {
  # Get the S and Sbar in the current chain component
  S_now <- chain_components_batch[[chain_components_batch_idx]]$S # The features to condition on
  Sbar_now <- chain_components_batch[[chain_components_batch_idx]]$Sbar # The features to sample
  Sbar_now_names <- names(x_explain)[Sbar_now]

  # Check if we are to sample from the marginal or conditional distribution
  if (is.null(S_now)) {
    # Marginal distribution as there are NO variables to condition on
    # TODO: Add option for not use training data but rather some distribution
    dt[, (Sbar_now_names) := create_marginal_data(x_train, n_explain = n_explain, Sbar_features = Sbar_now, n_samples = n_samples)]
  } else {
    # Conditional distribution as there are variables to condition on

    # TODO: is this necessary? Is not S_row_now the same as the S that is given to us?
    # Find which row in S the current set of conditional features corresponds to. This will be the index_features.
    S_now_binary <- rep(0, n_features)
    S_now_binary[S_now] <- 1
    S_row_now <- which(apply(internal_copy$objects$S, 1, function(x) identical(x, S_now_binary)))

    # Generate the MC samples conditioning on S_now, but only keeping the features in Sbar_now.
    dt[, (Sbar_now_names) := prepare_data(internal_copy, index_features = S_row_now)[, .SD, .SDcols = Sbar_now_names]]
    internal_copy$data$x_explain <- dt
  }
}
dt




#' #' @keywords internal
#' batch_prepare_vS_MC_causal <- function(S, internal) {
#'   max_id_combination <- internal$parameters$n_combinations
#'   x_explain <- internal$data$x_explain
#'   n_explain <- internal$parameters$n_explain
#'
#'   if (max_id_combination %in% S) {
#'     dt <- if (length(S) == 1) NULL else prepare_data_causal(internal, index_features = S[S != max_id_combination])
#'     dt <- rbind(dt, data.table(id_combination = max_id_combination, x_explain, w = 1, id = seq_len(n_explain)))
#'     setkey(dt, id, id_combination)
#'   } else {
#'     dt <- prepare_data_causal(internal, index_features = S)
#'   }
#'   return(dt)
#' }


# prepare_data_causal <- function(internal, index_features = NULL, ...) {
#   message("In prepare_data_causal.")
#   print("In prepare_data_causal.")
#   # Recall that here, index_features is a vector of id_combinations, i.e., indicating which rows in S to use.
#   # Also note that we are guaranteed that index_features does not include the empty or grand coalition
#
#   approach <- internal$parameters$approach # Can only be single approach
#   n_samples <- internal$parameters$n_samples
#   n_explain <- internal$parameters$n_explain
#   n_features <- internal$parameters$n_features
#   feature_names <- internal$parameters$feature_names
#
#   # Extract the causal related variables
#   S <- internal$objects$S
#   S_causal <- internal$objects$S_causal
#   S_causal_batch <- S_causal[index_features]
#
#   # Create a list to store the populated data tables with the MC samples
#   dt_list <- list()
#
#   # Create a copy of the internal list. We will change its x_explain, n_explain, and n_samples such
#   # that we can the prepare_data() function which was not originally designed for the step-wise/iterative
#   # sampling process which is needed for Causal Shapley values where we sample from P(Sbar_i | S_i) and
#   # the S and Sbar changes in the iterative process. So those also the number of MC samples we need to generate.
#   internal_copy <- copy(internal)
#
#   # lapply over the coalitions in the batch
#   index_feature_idx <- 1
#   for (index_feature_idx in seq_along(index_features)) {
#     print(index_feature_idx)
#     # Reset the internal_copy list for each new combination
#     if (index_feature_idx > 1) {
#       internal_copy$data$x_explain <- internal$data$x_explain
#       internal_copy$parameters[c("n_explain", "n_samples")] <- internal$parameters[c("n_explain", "n_samples")]
#     }
#
#     # Extract the index of the current combination
#     index_feature <- index_features[index_feature_idx]
#
#     # Create the empty data table which we are to populate with the Monte Carlo samples for each combination
#     dt <- data.table(matrix(nrow = n_explain * n_samples, ncol = n_features))
#     colnames(dt) <- feature_names
#
#     # Populate the data table with the features we condition on
#     S_names <- feature_names[as.logical(S[index_feature, ])]
#     dt[, (S_names) := x_explain[rep(seq(n_explain), each = n_samples), .SD, .SDcols = S_names]]
#
#     # Get the iterative sampling process for the current combination
#     S_causal_now <- S_causal[[index_feature]]
#
#     # Loop over the steps in the iterative sampling process to generate MC samples for the unconditional features
#     sampling_step_idx <- 1
#     for (sampling_step_idx in seq_along(S_causal_now)) {
#       print(sampling_step_idx)
#       # Get the S (the conditional features) and Sbar (the unconditional features) in the current sampling step
#       S_now <- S_causal_now[[sampling_step_idx]]$S # The features to condition on in this sampling step
#       Sbar_now <- S_causal_now[[sampling_step_idx]]$Sbar # The features to sample in this sampling step
#       Sbar_now_names <- feature_names[Sbar_now]
#
#       # Check if we are to sample from the marginal or conditional distribution
#       if (is.null(S_now)) {
#         # Marginal distribution as there are no variables to condition on
#
#         # TODO: Add option for not use training data but rather some distribution
#         dt[, (Sbar_now_names) :=
#           create_marginal_data(x_train, n_explain = n_explain, Sbar_features = Sbar_now, n_samples = n_samples)]
#       } else {
#         # Conditional distribution as there are variables to condition on
#
#         # Find which row in S the current set of conditional features corresponds to
#         # This will be the value of index_features in the prepare_data function
#         S_now_binary <- rep(0, n_features)
#         S_now_binary[S_now] <- 1
#         S_row_now <- which(apply(S, 1, function(x) identical(x, S_now_binary)))
#
#         # Generate the MC samples conditioning on S_now
#         print("Generate samples")
#         print(system.time({
#         dt_new <- prepare_data(internal_copy, index_features = S_row_now, ...)
#         }))
#
#         if (approach %in% c("independence", "empirical", "ctree")) {
#           # These approaches produce weighted MC samples, i.e., the do not necessarily generate n_samples MC samples.
#           # We ensure n_samples by weighted sampling with replacements those ids with less than n_samples MC samples.
#           print("Ensure n_samples")
#           print(system.time({
#           dt_new <- dt_new[, .SD[if (.N >= n_samples) {
#             seq(.N)
#           } else {
#             sample(.N, internal_copy$parameters$n_samples, replace = TRUE, prob = w)
#           }], by = id]
#           }))
#
#           if (nrow(dt_new) != n_explain * n_samples) stop("SOMETHING IS WRONG")
#         }
#
#         # Insert/keep only the features in Sbar_now into dt
#         dt[, (Sbar_now_names) := dt_new[, .SD, .SDcols = Sbar_now_names]]
#       }
#
#       # Update the x_explain in internal_copy such that in the next sampling step use the values in dt
#       # as the conditional feature values. Furthermore, we set n_samples to 1 such that we in the next
#       # step generate one new value for the n_samples MC samples we have begun to generate.
#       internal_copy$data$x_explain <- dt
#       internal_copy$parameters$n_explain <- nrow(dt)
#       internal_copy$parameters$n_samples <- 1
#     }
#
#     # Save the now populated data table
#     dt_list[[index_feature_idx]] <- dt
#   }
#
#   # Combine the list of data tables and add the id columns
#   dt <- data.table::rbindlist(dt_list, fill = TRUE)
#   dt[, id_combination := rep(index_features, each = n_samples * n_explain)]
#   dt[, id := rep(seq(n_explain), each = n_samples, times = length(index_features))]
#   dt[, w := 1 / n_samples]
#   data.table::setcolorder(dt, c("id_combination", "id", feature_names))
#
#   # Aggregate the weights for the non-unique rows such that we only return a data table with unique rows
#   dt_final <- dt[, sum(w), by = c("id_combination", "id", feature_names)]
#   data.table::setnames(dt_final, "V1", "w")
#
#   return(dt_final)
# }
# environment(prepare_data_causal) <- asNamespace('shapr')
# assignInNamespace("prepare_data_causal", prepare_data_causal, ns = "shapr")



#' #' @keywords internal
#' #' @author Martin Jullum, Lars Henry Berge Olsen
#' batch_prepare_vS_MC <- function(S, internal, model, predict_model) {
#'   output_size <- internal$parameters$output_size
#'   feature_names <- internal$parameters$feature_names
#'   type <- internal$parameters$type
#'   horizon <- internal$parameters$horizon
#'   n_endo <- internal$data$n_endo
#'   explain_idx <- internal$parameters$explain_idx
#'   explain_lags <- internal$parameters$explain_lags
#'   y <- internal$data$y
#'   xreg <- internal$data$xreg
#'   keep_samp_for_vS <- internal$parameters$keep_samp_for_vS
#'   causal_Shapley <- internal$parameters$causal_Shapley
#'
#'   # Split in whether the MC samples are generated based on causal Shapley (i.e., more than one component)
#'   # TODO: discuss with Martin, as regular Shapley is a special case of causal Shapley with only one component.
#'   # Thus, we could have a single function where we just do one iteration of the sampling loop for regular Shapley.
#'   if (causal_Shapley) {
#'     dt <- batch_prepare_vS_MC_causal(S = S, internal = internal)
#'   } else {
#'     dt <- batch_prepare_vS_MC_auxiliary(S = S, internal = internal) # Make it optional to store and return the dt_list
#'   }
#'
#'   pred_cols <- paste0("p_hat", seq_len(output_size))
#'
#'   compute_preds(
#'     dt, # Updating dt by reference
#'     feature_names = feature_names,
#'     predict_model = predict_model,
#'     model = model,
#'     pred_cols = pred_cols,
#'     type = type,
#'     horizon = horizon,
#'     n_endo = n_endo,
#'     explain_idx = explain_idx,
#'     explain_lags = explain_lags,
#'     y = y,
#'     xreg = xreg
#'   )
#'   dt_vS <- compute_MCint(dt, pred_cols)
#'
#'   # Also return the dt object if keep_samp_for_vS is TRUE
#'   return(if (keep_samp_for_vS) list(dt_vS = dt_vS, dt_samp_for_vS = dt) else dt_vS)
#' }
