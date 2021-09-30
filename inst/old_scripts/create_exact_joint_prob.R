#' Calculat ethe true joint probability function under the assumption that the underlying features
#' are jointly normally distributed with some \code{mu} and some \code{Sigma}. Then the categorical
#' features are devised using a set cutoff vector.
#'
#' @param mu Vector. With length equal to the number of features (the dimension).
#'
#' @param Sigma Matrix. Of numerics. With nrow = ncol = the number of features.
#'
#' @param explainer An \code{explainer} object to use for explaining the observations.
#' See \code{\link{shapr}}.
#'
#' @param cutoff Vector. Of length equal to the number of categorical for each categorical variable.
#' Must be ordered (smallest to largest).
#'
#' @param algorithm One of three algorithm developed in the R library mvtnorm. Algorithms can be
#' 'GenzBretz()', 'Miwa' or 'TVPack.
#'
#' @param mc.cores Integer. Number of cores to use.
#'
#' @return Data.table. Includes a column for each feature as determined by \code{mu}. And a column
#' called 'joint_prob' with the joint probability of the combination of features.
#'
#' @author Martin Jullum
#'
#' @export
#'
#' @example
#' mu <- c(0, 0, 0); Sigma <- matrix(c(1, 0, 0, 0, 1, 0, 0, 0, 1), nrow = 3);
#' cutoff <- c(-200, 0, 1, 200)
#'
#' x <- mvrnorm(n =  10, mu = mu, Sigma = Sigma)
#'
#' dt <- NULL
#' for (i in 1:dim) {dt <- cbind(dt, cut(x[, i], cutoff, labels = 1:3))}
#'
#' x_test_list <- list()
#' for (i in 1:3) {x_test_list[[i]] <- 1:3}
#' x_test_dt <- do.call(CJ, x_test_list)
#'
#' dt <- data.table(rbind(dt, x_test_dt))
#' setnames(dt, names(dt), paste0("feat_", 1:dim,  "_"))
#' dt <- dt[, lapply(.SD, as.factor)]
#'
#' mod_matrix <- model.matrix(~  . - 1, data = dt[, 1:dim], contrasts.arg = lapply(dt[, 1:dim],
#' contrasts, contrasts = FALSE))
#'
#' beta <- c(-0.6, 0.2, -0.8, 1.6, 0.3, -0.8, 0.5, 0.7, 0.6, -0.3)
#' response_mod <- function(mod_matrix_full, beta) as.vector(mod_matrix_full %*% beta)
#'
#' dt[, response := response_mod(mod_matrix_full = cbind(1, mod_matrix), beta = beta)]
#'
#' model <- lm(response ~ feat_1_ + feat_2_ + feat_3_, data = dt[(1:No_train_obs)])
#'
#' p <- mean(dt$response[1:10])
#' x_train <- dt[(1:10), c("feat_1_", "feat_2_", "feat_3_")]
#'
#' explainer <- shapr(x_train, model)
#'
#' create_exact_joint_prob(mu, Sigma, explainer, cutoff)
#'
create_exact_joint_prob <- function(mu, Sigma, explainer, cutoff, algorithm = mvtnorm::GenzBretz(),
                                       mc.cores = 16) {

  feat_names <- colnames(explainer$x_train)
  dim <- length(feat_names)
  no_categories <- length(cutoff) - 1

  all_x_list <- list()
  for (i in 1:dim) {
    all_x_list[[i]] <- 1:no_categories
  }
  all_x_dt <- do.call(CJ, all_x_list)
  names(all_x_dt) <- feat_names

  all_x_dt[, (feat_names) := lapply(.SD, as.factor), .SDcols = feat_names]

  # Lists with vectors containing the lower and upper combinations
  upper_func <- function(x, cutoff) {
    cutoff[as.numeric(x) + 1]
  }

  lower_func <- function(x, cutoff) {
    cutoff[as.numeric(x)]
  }

  upper_dt <- all_x_dt[, lapply(.SD, upper_func, cutoff = cutoff), .SDcols = feat_names]
  lower_dt <- all_x_dt[, lapply(.SD, lower_func, cutoff = cutoff), .SDcols = feat_names]

  upper_dt_list <- as.list(as.data.table(t(upper_dt)))
  lower_dt_list <- as.list(as.data.table(t(lower_dt)))

  corr <- cov2cor(Sigma)

  all_probs <- parallel::mcmapply(FUN = mvtnorm::pmvnorm,
                                  lower = lower_dt_list,
                                  upper = upper_dt_list,
                                  MoreArgs = list(mean = mu,
                                                  corr = corr,
                                                  algorithm = algorithm),
                                  mc.cores = mc.cores)

  all_probs <- all_probs / sum(all_probs)

  all_x_dt[, joint_prob := all_probs]

  return(all_x_dt)
}

#' String extraction function
#'
#' Function extracting string between two specific characters, minor customization of this one
#' http://www.r-bloggers.com/how-to-extract-a-string-between-2-characters-in-r-and-sas/
#'
#' @param mystring Character vector to extract from.
#'
#' @param initial.character Character determining the starting point of extractions
#'
#' @param final.character Character determining the end point of extractions
#'
#' @return snippet
#'
#' @export
getstr = function(mystring, initial.character = "_", final.character = "_") {
  # check that all 3 inputs are character variables
  if (!is.character(mystring)) {
    stop('The parent string must be a character variable.')
  }

  if (!is.character(initial.character)) {
    stop('The initial character must be a character variable.')
  }


  if (!is.character(final.character)) {
    stop('The final character must be a character variable.')
  }

  add = 0
  if(initial.character == final.character){add=1}

  # pre-allocate a vector to store the extracted strings
  snippet = rep(0, length(mystring))

  for (i in 1:length(mystring)) {
    # extract the initial position
    initial.position <- gregexpr(initial.character, mystring[i])[[1]][1] + 1

    # extract the final position
    final.position <- gregexpr(final.character, mystring[i])[[1]][1+add] - 1

    # extract the substring between the initial and final positions, inclusively
    snippet[i] <- substr(mystring[i], initial.position, final.position)
  }
  return(snippet)
}


#' Function to calculate the mean average error (MAE) between the true Shapley values and the estimated Shapley values
#'
#' @param true_shapley vector of Numerics. The vector of true Shapley values.
#'
#' @param shapley_method vector of Numerics. The vector of estimated Shapley values
#'
#' @param weights vector of weights with length equal to  number of rows of true_shapley/shapley_method
#'
#' @return vector of Shapley values.
#'
#' @export
#'
#' @example
#' true_shapley <- data.frame(none = rep(0.035, 3), feat_1_ = c(0.09, 0.1, 0.02), feat_2_ = c(-0.3, 0.04, 0.04),
#' feat_3_ = c(0.15, 0.07, -0.8))
#'
#' shapley_method <- data.frame(none = rep(0.035, 3), feat_1_ = c(0.009, 0.11, 0.21), feat_2_ = c(1, 0.5, 0.45),
#' feat_3_ = c(0.15, 0.77, -0.44))
#'
#' weights <- c(0.3, 0.4, 0.3)
#'
#' MAE(true_shapley, shapley_method, weights)
#'
MAE <- function(true_shapley, shapley_method, weights){
  mean(colSums((abs(true_shapley - shapley_method))* weights)[-1])
}

#' Function to extract the list of variables to group (we group the one-hot encoded variables together).
#' The numeric variables get their own group.
#'
#' @param data Data.table. Includes only the features to be grouped. Features can be factors or numerics.
#'
#' @return List. List of features from \code{data} where each list entry corresponds to a feature and
#' either the name of the feature or a vector of the one-hot encoded names.
#'
#' @export
#'
#' @author Annabelle Redelmeier
#'
#' @example
#' data <- data.frame(feat_1_ = c(1, 2, 3, 3), feat_2_ = c(2, 2, 1, 1), feat_3_ = c(1, 4, 2, 3))
#' data <- as.data.table(data)
#' data2 <- data[, lapply(.SD, as.factor)]
#' data2 <- cbind(data2, feat_4_= c(2.24, 4.4421, 3.21, -0.051))
#'
#' group_factors(data2)
group_factors <- function(data) {
  group_names_list <- list()
  is_factor <- sapply(data, is.factor)
  factor_variables <- data[, is_factor, with = FALSE]
  dummylist <- make_dummies(data = data)
  k <- 1

  for(i in 1:ncol(data)) {
    if (is_factor[i] == FALSE) {
      group_names_list[[i]] <- colnames(data)[i]

    } else {
      lvs <- levels(factor_variables[[k]])

      group_names_list[[i]] <- paste0(colnames(factor_variables)[k], lvs[-1])
      k <- k + 1
    }
  }
  return(group_names_list)
}

