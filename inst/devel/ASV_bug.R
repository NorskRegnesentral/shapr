rm(list = ls())

# Libraries -------------------------------------------------------------------------------------------------------
library(shapr)
library(data.table)
library(gtools)
library(ggplot2)
library(GGally)


# Functions -------------------------------------------------------------------------------------------------------
#' Get the weight matrix for Shapley values based on valid permutations respecting a causal ordering
#'
#' @description Constructs a weight matrix for Shapley values based on valid permutations of features according
#'  to the specified `causal_ordering`. Each entry in the matrix represents the contribution of a coalition to the
#'  Shapley value of a feature, weighted by the number of valid permutations.
#'  The first row corresponds to the null coalition.
#'
#'  This function is similar to [shapr:::weight_matrix] but is based on permutations instead and it respects the
#'  causal ordering.
#'
#' @param coalitions_str Optional character vector. The coalitions to include in the weight matrix as strings.
#' @param m Optional integer. The number of features/Shapley values. If `NULL`, it is inferred as the maximum
#'  feature index in `causal_ordering`.
#' @param w_perm Optional numeric scalar. The weight assigned to each valid permutation. If `NULL`, it is set
#'  to `1/n_perms` where `n_perms` is the number of valid permutations.
#' @param causal_ordering A list of integer vectors defining the causal ordering of features.
#' @inheritParams explain
#'
#' @returns A numeric matrix of dimension `(m + 1) x n_coalitions` where `m` is the number of features
#' and `n_coalitions` is the number of valid coalitions. The first row corresponds to the null coalition.
#' Each entry `W_perms[i, j]` represents the weight contribution of coalition `j` to the `i - 1`th Shapley value.
#' @keywords internal
#' @author Lars Henry Berge Olsen
#' @export
#'
#' @examples
#' causal_ordering <- list(c(1, 2), c(3, 4))
#' weight_matrix_permutation(causal_ordering = causal_ordering)
weight_matrix_permutation <- function(causal_ordering, coalitions_str = NULL, m = NULL, w_perm = NULL) {
  # Get the valid coalitions as strings if not provided
  if (is.null(coalitions_str)) {
    coalitions_str <-
      sapply(shapr:::get_valid_causal_coalitions(causal_ordering = causal_ordering), paste, collapse = " ")
  }

  # Convert the coalitions to string for faster comparison. Assume then that it is a list of integer vectors if not str.
  if (class(coalitions_str) != "character") coalitions_str <- unname(sapply(coalitions_str, paste, collapse = " "))

  # Get the number of Shapley values (excluding phi0) if not provided
  if (is.null(m)) m <- max(unlist(causal_ordering))

  # Get the valid permutations that satisfy the causal ordering
  perms <- get_valid_permutations(causal_ordering = causal_ordering)

  # Get the number of permutations and the weight if not provided
  n_perms <- length(perms)
  if (is.null(w_perm)) w_perm <- 1 / n_perms

  # Initialize weight matrix: rows = null + features, columns = coalitions_str
  W_perms <- matrix(0, nrow = m + 1, ncol = length(coalitions_str))
  W_perms[1, 1] <- 1 # Null coalition gets weight 1

  # Loop over all valid permutations
  for (i in seq_len(n_perms)) {
    perm <- perms[[i]]

    # Loop over each feature/Shapley value
    for (j in seq_len(m)) {
      # Get features before j in the permutation
      S_without_j <- sort(perm[seq_len(which(perm == j) - 1)])
      S_with_j <- sort(unique(c(S_without_j, j)))

      # Convert to string for faster comparison
      S_without_j_str <- paste(S_without_j, collapse = " ")
      S_with_j_str <- paste(S_with_j, collapse = " ")

      # Find matching coalition indices
      id_S_without_j <- which(S_without_j_str == coalitions_str)
      id_S_with_j <- which(S_with_j_str == coalitions_str)

      # Update weight matrix
      W_perms[j + 1, id_S_with_j] <- W_perms[j + 1, id_S_with_j] + w_perm
      W_perms[j + 1, id_S_without_j] <- W_perms[j + 1, id_S_without_j] - w_perm
    }
  }

  return(W_perms)
}

#' Get the permutations that satisfy the (distal/root) causal ordering
#'
#' @description Generates a list of all valid permutations of the set {1, 2, ..., m} that adhere
#' to a specified causal ordering debined by `causal_ordering`. Each permutation is represented
#' as an integer vector. To convert to a matrix use `do.call(rbind, get_valid_permutations(causal_ordering))`.
#'
#' @inheritParams explain
#'
#' @importFrom gtools permutations
#'
#' @return A list where each element is a valid permutation of `1:m` that satisfies the causal ordering.
#' @keywords internal
#' @author Lars Henry Berge Olsen
#'
#' @examples
#' get_valid_permutations(list(c(1, 2, 3))) # Return all 3! = 6 possible permutations
#' get_valid_permutations(list(c(3), c(1, 4), c(2)))
get_valid_permutations <- function(causal_ordering) {
  # Load required package
  if (!requireNamespace("gtools", quietly = TRUE)) {
    stop("Package 'gtools' is required but not installed.")
  }

  # Generate permutations for each group in the causal ordering
  n_groups <- length(causal_ordering)
  perms_by_group <- lapply(causal_ordering, function(group) {
    asplit(gtools::permutations(n = length(group), r = length(group), v = group), 1)
  })

  # Start with the first group's permutations as a list of integer vectors
  valid_perms <- perms_by_group[[1]]

  # Iteratively expand with each subsequent group's permutations
  if (n_groups > 1) {
    for (i in 2:n_groups) {
      valid_perms <- as.list(unlist(
        lapply(valid_perms, function(prev) {
          lapply(perms_by_group[[i]], function(curr) c(prev, curr))
        }),
        recursive = FALSE
      ))
    }
  }

  return(valid_perms)
}

#' Get the permutations that satisfy the (distal/root) causal ordering
#'
#' @description Generates all valid permutations of the set {1, 2, ..., m} that adhere to a specified causal ordering.
#'
#' @inheritParams explain
#'
#' @importFrom gtools permutations
#'
#' @return A matrix where each row is a valid permutation of `1:m` that satisfies the causal ordering.
#' @keywords internal
#' @author Lars Henry Berge Olsen
#'
#' @examples
#' get_valid_permutations_slow(list(1, 2, 3)) # All 3! = 6 permutations are allowed as no ordering is specified
#' get_valid_permutations_slow(list(c(3), c(1, 4), c(2)))
get_valid_permutations_slow <- function(causal_ordering = list(seq_len(m))) {
  # Load required package
  if (!requireNamespace("gtools", quietly = TRUE)) {
    stop("Package 'gtools' is required but not installed.")
  }

  # Get the number of features
  m <- max(unlist(causal_ordering))

  # Generate all permutations of 1:m
  perms <- gtools::permutations(n = m, r = m, v = seq_len(m))

  # Check if a permutation satisfies the causal ordering
  satisfies_causal_ordering <- function(perm, causal_ordering) {
    # Flatten the causal ordering into a single vector and find the position of each element in the permutation
    positions <- match(unlist(causal_ordering), perm)

    # Get the number of elements in each group of the causal ordering
    group_lengths <- lengths(causal_ordering)

    # Split the positions into groups corresponding to the original causal ordering
    group_indices <- split(positions, rep(seq_along(causal_ordering), group_lengths))

    # Loop through each pair of consecutive groups
    for (i in seq_len(length(group_indices) - 1)) {
      # Check if any element in the current group appears after any element in the next group
      if (max(group_indices[[i]]) > min(group_indices[[i + 1]])) {
        # If so, the causal ordering is violated
        return(FALSE)
      }
    }

    # If all checks pass, the permutation satisfies the causal ordering
    return(TRUE)
  }

  # Filter permutations that satisfy the causal ordering
  valid_rows <- apply(perms, 1, satisfies_causal_ordering, causal_ordering = causal_ordering)
  valid_perms <- perms[valid_rows, , drop = FALSE]

  return(valid_perms)
}

## Check that they give the same, and yes they do.
# causal_ordering = list(c(4, 6), c(1, 3), c(2, 5, 7, 8, 9))
# system.time({val_fast = do.call(rbind, get_valid_permutations(causal_ordering = causal_ordering))})
# system.time({val_slow = get_valid_permutations_slow(causal_ordering = causal_ordering)})
# all.equal(val_fast, val_slow)

plot_beeswarms <- function(explanation_list, title = "", ...) {
  # Make the beeswarm plots
  grobs <- lapply(seq(length(explanation_list)), function(explanation_idx) {
    gg <- plot(explanation_list[[explanation_idx]], plot_type = "beeswarm", ...) +
      ggplot2::ggtitle(tools::toTitleCase(gsub("_", " ", names(explanation_list)[[explanation_idx]])))

    # Flip the order such that the features come in the right order
    gg <- gg +
      ggplot2::scale_x_discrete(limits = rev(levels(gg$data$variable)[levels(gg$data$variable) != "none"]))
  })

  # Get the limits
  ylim <- sapply(grobs, function(grob) ggplot2::ggplot_build(grob)$layout$panel_scales_y[[1]]$range$range)
  ylim <- c(min(ylim), max(ylim))

  # Update the limits
  grobs <- suppressMessages(lapply(grobs, function(grob) grob + ggplot2::coord_flip(ylim = ylim)))

  # Make the combined plot
  gridExtra::grid.arrange(
    grobs = grobs, ncol = 1,
    top = grid::textGrob(title, gp = grid::gpar(fontsize = 18, font = 8))
  )
}

# Bike ------------------------------------------------------------------------------------------------------------
## Setup -----------------------------------------------------------------------------------------------------------
# Set up the data
# Can also download the data set from the source https://archive.ics.uci.edu/dataset/275/bike+sharing+dataset
# temp <- tempfile()
# download.file("https://archive.ics.uci.edu/static/public/275/bike+sharing+dataset.zip", temp)
# bike <- read.csv(unz(temp, "day.csv"))
# unlink(temp)
bike <- read.csv("inst/extdata/day.csv") # Assuming the working directory is shapr

# Difference in days, which takes DST into account
bike$trend <- as.numeric(difftime(bike$dteday, bike$dteday[1], units = "days"))
bike$cosyear <- cospi(bike$trend / 365 * 2)
bike$sinyear <- sinpi(bike$trend / 365 * 2)
# Unnormalize variables (see data set information in link above)
bike$temp <- bike$temp * (39 - (-8)) + (-8)
bike$atemp <- bike$atemp * (50 - (-16)) + (-16)
bike$windspeed <- 67 * bike$windspeed
bike$hum <- 100 * bike$hum

# Plot the data
ggplot(bike, aes(x = trend, y = cnt, color = temp)) +
  geom_point(size = 0.75) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(colour = "temp") +
  xlab("Days since 1 January 2011") +
  ylab("Number of bikes rented") +
  theme_minimal() +
  theme(legend.position = "right", legend.title = element_text(size = 10))

# Define the features and the response variable
x_var <- c("trend", "cosyear", "sinyear", "temp", "atemp", "windspeed", "hum")
y_var <- "cnt"

# NOTE: To avoid RNG reproducibility issues across different systems, we
# load the training-test split from a file. 80% training and 20% test
train_index <- readRDS("inst/extdata/train_index.rds")

# Training data
x_train <- as.matrix(bike[train_index, x_var])
y_train_nc <- as.matrix(bike[train_index, y_var]) # not centered
y_train <- y_train_nc - mean(y_train_nc)

# Plot pairs plot
GGally::ggpairs(x_train)

# Test/explicand data
x_explain <- as.matrix(bike[-train_index, x_var])
y_explain_nc <- as.matrix(bike[-train_index, y_var]) # not centered
y_explain <- y_explain_nc - mean(y_train_nc)

# Get 6 explicands to plot the Shapley values for, with a wide spread in their predicted outcomes
n_index_x_explain <- 6
index_x_explain <- order(y_explain)[seq(1, length(y_explain), length.out = n_index_x_explain)]
y_explain[index_x_explain]
#> [1] -3900.03 -1872.03  -377.03   411.97  1690.97  3889.97

# Fit an XGBoost model to the training data
model <- xgboost::xgboost(
  data = x_train,
  label = y_train,
  nround = 100,
  verbose = FALSE,
)

# Save the phi0
phi0 <- mean(y_train)

# Look at the root mean squared error
sqrt(mean((predict(model, x_explain) - y_explain)^2))
#> [1] 798.71
ggplot(
  data.table("response" = y_explain[, 1], "predicted_response" = predict(model, x_explain)),
  aes(response, predicted_response)
) +
  geom_point()

causal_ordering <- list(1, c(2, 3), c(4:7))
causal_ordering <- list("trend", c("cosyear", "sinyear"), c("temp", "atemp", "windspeed", "hum"))
confounding <- c(FALSE, TRUE, FALSE)


## Explain ---------------------------------------------------------------------------------------------------------
explanation_list <- list()

# Asymmetric conditional Shapley values, non-iterative
explanation_list[["gaussian_non_iterative"]] <- explain(
  model = model,
  x_train = x_train,
  x_explain = x_explain,
  phi0 = phi0,
  seed = 1,
  n_MC_samples = 1000,
  approach = "gaussian",
  asymmetric = TRUE,
  causal_ordering = causal_ordering,
  confounding = NULL, # Default value
  iterative = FALSE
)



## Results ---------------------------------------------------------------------------------------------------------
### KernelSHAP -----------------------------------------------------------------------------------------------------
phi <- explanation_list$gaussian_non_iterative$shapley_values_est

### KernelSHAP manual ----------------------------------------------------------------------------------------------
dt_vS <- explanation_list$gaussian_non_iterative$internal$output$dt_vS
W <- explanation_list$gaussian_non_iterative$internal$objects$W
shap_names <- explanation_list$gaussian_non_iterative$internal$parameters$shap_names
kshap <- t(W %*% as.matrix(dt_vS[, -"id_coalition"]))
dt_kshap <- data.table::as.data.table(kshap)
colnames(dt_kshap) <- c("none", shap_names)
dt_kshap[, explain_id := .I]
setcolorder(dt_kshap, c("explain_id", "none", shap_names))
dt_kshap

all.equal(dt_kshap, phi)

### PermutationSHAP manual -----------------------------------------------------------------------------------------
causal_ordering <- explanation_list$gaussian_non_iterative$internal$parameters$causal_ordering
W_perm <- weight_matrix_permutation(causal_ordering = causal_ordering)

kshap_perm <- t(W_perm %*% as.matrix(dt_vS[, -"id_coalition"]))
dt_kshap_perm <- data.table::as.data.table(kshap_perm)
colnames(dt_kshap_perm) <- c("none", shap_names)
dt_kshap_perm
dt_kshap_perm[, explain_id := .I]
setcolorder(dt_kshap_perm, c("explain_id", "none", shap_names))
dt_kshap_perm


# Compare the weight matrices
# See that perm respects thae causal orderings, i.e., it contains zeros where it should
round(W, 4)
round(W_perm, 4)


## Plots -----------------------------------------------------------------------------------------------------------
# Add the perm based Shapley values to the explanation list by copying and modifying the previous result
explanation_list[["gaussian_non_iterative_perm"]] <- explanation_list[["gaussian_non_iterative"]]
explanation_list[["gaussian_non_iterative_perm"]]$shapley_values_est <- dt_kshap_perm

# See some very small differences, but difficult to see
plot_beeswarms(explanation_list, title = "Asymmetric conditional Shapley values")

# Plot them against each other, we see that their are some minor differences
# But they are not huge, and would most likely not change the conclusions motivated by the explanations.
dt_plot_against <- merge(
  data.table::melt(dt_kshap, id.vars = "explain_id", value.name = "asym"),
  data.table::melt(dt_kshap_perm, id.vars = "explain_id", value.name = "asym_perm"),
  by = c("explain_id", "variable")
)

ggplot(dt_plot_against, aes(x = asym, y = asym_perm)) +
  geom_point() +
  facet_wrap(~variable, scales = "free") +
  geom_abline(slope = 1, intercept = 0, col = "red", lwd = 0.75) +
  labs(title = "Comparing Asymmetric Conditional Shapley Values", x = "ACSV - Kernel", y = "ACSV - Permutation")


# Plot them with explain_id on the x-axis
dt_plot <- rbind(
  data.table::melt(dt_kshap, id.vars = "explain_id")[, method := "asym"],
  data.table::melt(dt_kshap_perm, id.vars = "explain_id")[, method := "asym_perm"]
)

ggplot(dt_plot, aes(x = explain_id, y = value, col = method)) +
  geom_point(alpha = 0.5) +
  facet_wrap(~variable, scales = "free")







# Symmetric ------------------------------------------------------------------------------------------------------
causal_ordering <- list(c(1, 2, 3))
m <- max(unlist(causal_ordering))

dt_valid_causal_coalitions <-
  data.table(coalitions = shapr:::get_valid_causal_coalitions(causal_ordering = causal_ordering))

X <- shapr:::create_coalition_table(dt_valid_causal_coalitions = dt_valid_causal_coalitions, m = m)
print(X)

W <- round(shapr:::weight_matrix(X = X, normalize_W_weights = TRUE), 5)
W_perm <- round(weight_matrix_permutation(causal_ordering = causal_ordering), 5)

W
W_perm

all.equal(W, W_perm)






# Asymmetric ------------------------------------------------------------------------------------------------------
## Case 1 ---------------------------------------------------------------------------------------------------------
causal_ordering <- list(c(1), c(2, 3))
m <- max(unlist(causal_ordering))

dt_valid_causal_coalitions <-
  data.table(coalitions = shapr:::get_valid_causal_coalitions(causal_ordering = causal_ordering))

X <- shapr:::create_coalition_table(dt_valid_causal_coalitions = dt_valid_causal_coalitions, m = m)
print(X)

W <- round(shapr:::weight_matrix(X = X, normalize_W_weights = TRUE), 5)
W_perm <- round(weight_matrix_permutation(causal_ordering = causal_ordering), 5)

W
W_perm


### Coincidence? ----------------------------------------------------------------------------------------------------
# Only use the first row. Yes, coincidence based on us setting 10^(-6). No longer true for other values.
X
X[id_coalition %in% c(3, 4), shapley_weight := 10^(-6)]
W_coindicence <- round(shapr:::weight_matrix(X = X, normalize_W_weights = TRUE), 8)

W_coindicence
W_perm

all.equal(W_coindicence, W_perm)



## Case 2 ---------------------------------------------------------------------------------------------------------
causal_ordering <- list(c(4, 5), c(2, 3), c(1, 6))
causal_ordering <- list(c(1, 2), c(3, 4), c(5, 6))
m <- max(unlist(causal_ordering))

dt_valid_causal_coalitions <-
  data.table(coalitions = shapr:::get_valid_causal_coalitions(causal_ordering = causal_ordering))

X <- shapr:::create_coalition_table(dt_valid_causal_coalitions = dt_valid_causal_coalitions, m = m)
print(X)

W <- round(shapr:::weight_matrix(X = X, normalize_W_weights = TRUE), 5)
W_perm <- round(weight_matrix_permutation(causal_ordering = causal_ordering), 5)

W
W_perm


## Case 3 ---------------------------------------------------------------------------------------------------------
causal_ordering <- list(c(1), c(2, 3, 4))
m <- max(unlist(causal_ordering))

dt_valid_causal_coalitions <-
  data.table(coalitions = shapr:::get_valid_causal_coalitions(causal_ordering = causal_ordering))

X <- shapr:::create_coalition_table(dt_valid_causal_coalitions = dt_valid_causal_coalitions, m = m)
X[c(1, .N), shapley_weight := 10^10]
print(X)


W <- round(shapr:::weight_matrix(X = X, normalize_W_weights = TRUE), 8)
W_perm <- round(weight_matrix_permutation(causal_ordering = causal_ordering), 8)

W
W_perm


# Seems to be a coincidence. changes based on the Shapley weight we set below.
X
X[id_coalition %in% seq(3, 8), shapley_weight := 10^(-10)]
W_coindicence <- round(shapr:::weight_matrix(X = X, normalize_W_weights = TRUE), 8)

W_perm
W_coindicence
W_perm - W_coindicence
all.equal(W_perm, W_coindicence)



# Martin's Code ---------------------------------------------------------------------------------------------------
m <- 4
weight_zero_m <- 10^6

causal_ordering <- list(c(1, 2), 3, 4)
causal_ordering <- list(c(1, 2, 3, 4))
causal_ordering <- list(c(1, 2), c(3, 4))
causal_ordering <- list(c(1), c(2, 3, 4))
causal_ordering <- list(c(2, 3, 4), c(1))



dt_valid_causal_coalitions <-
  data.table(coalitions = shapr:::get_valid_causal_coalitions(causal_ordering = causal_ordering))


aa <- shapr:::exact_coalition_table(dt_valid_causal_coalitions = dt_valid_causal_coalitions, m = m)[]


coalitions0 <- dt_valid_causal_coalitions[, coalitions]

dt <- data.table::data.table(id_coalition = seq_along(coalitions0))
dt[, coalitions := coalitions0]
dt[, coalitions_str := sapply(coalitions, paste, collapse = " ")]
dt[, coalition_size := lengths(coalitions)]
dt[, N := .N, coalition_size]
dt[, shapley_weight := shapr:::shapley_weights(m = m, N = N, n_components = coalition_size, weight_zero_m)]
# dt[, sample_freq := NA]

dt[-c(1, .N), shapley_weight0 := shapley_weight / sum(shapley_weight)]
dt[-c(1, .N)]



##
m <- 3
weight_zero_m <- 10^6
causal_ordering <- list(c(1, 2, 3))
causal_ordering <- list(c(1), c(2, 3))

dt_valid_causal_coalitions <-
  data.table(coalitions = shapr:::get_valid_causal_coalitions(causal_ordering = causal_ordering))
dt_valid_causal_coalitions

n_valid_causal_coalitions <- nrow(dt_valid_causal_coalitions)

aa <- shapr:::exact_coalition_table(dt_valid_causal_coalitions = dt_valid_causal_coalitions, m = m)[]
aa

coalitions0 <- dt_valid_causal_coalitions[, coalitions]
dt_valid_causal_coalitions


dt <- data.table::data.table(id_coalition = seq_along(coalitions0))
dt[, coalitions := coalitions0]
dt[, coalitions_str := sapply(coalitions, paste, collapse = " ")]
dt[, coalition_size := lengths(coalitions)]
dt[, N := .N, coalition_size]
dt[, shapley_weight := shapr:::shapley_weights(m = m, N = N, n_components = coalition_size, weight_zero_m)]
# dt[, sample_freq := NA]
dt[-c(1, .N), shapley_weight0 := shapley_weight / sum(shapley_weight)]
dt[-c(1, .N)]

X <- shapr:::create_coalition_table(dt_valid_causal_coalitions = dt_valid_causal_coalitions, m = m)
X[]

dt[, `:=`(coalitions_str, sapply(coalitions, paste, collapse = " "))]

X[id_coalition %in% c(3, 4, 7), shapley_weight := 10^(-6)]
X[-c(1, .N)]

# Get weighted matrix
W <- round(shapr:::weight_matrix(
  X = X,
  normalize_W_weights = TRUE
), 5)
W
t(W)

W_perm <- round(weight_matrix_permutation(
  causal_ordering = causal_ordering,
), 5)
W_perm

all.equal(W, W_perm)

## Get feature matrix
S <- coalition_matrix_cpp(
  coalitions = X[["features"]],
  m = m
)
S
