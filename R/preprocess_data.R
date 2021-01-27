#' Fetches feature information from a given data set
#'
#' @param x matrix, data.frame or data.table The data to extract feature information from.
#'
#' @details This function is used to extract the feature information to be checked against the corresponding
#' information extracted from the model and other data sets. The function is called from
#' \code{\link[shapr:preprocess_data]{preprocess_data}}
#' and \code{\link[shapr:make_dummies]{make_dummies}}
#'
#' @return A list with the following elements:
#' \describe{
#'   \item{labels}{character vector with the feature names to compute Shapley values for}
#'   \item{classes}{a named character vector with the labels as names and the class types as elements}
#'   \item{factor_levels}{a named list with the labels as names and character vectors with the factor levels as elements
#'   (NULL if the feature is not a factor)}
#' }
#' @author Martin Jullum
#'
#' @keywords internal
#' @export
#'
#' @examples
#' # Load example data
#' if (requireNamespace("MASS", quietly = TRUE)) {
#'   data("Boston", package = "MASS")
#'   # Split data into test- and training data
#'   x_train <- data.table::as.data.table(head(Boston))
#'   x_train[, rad := as.factor(rad)]
#'   get_data_specs(x_train)
#' }
get_data_specs <- function(x) {
  x <- data.table::as.data.table(x)

  feature_list <- list()
  feature_list$labels <- names(x)
  feature_list$classes <- unlist(lapply(x, class))
  feature_list$factor_levels <- lapply(x, levels)

  # Defining all integer values as numeric
  feature_list$classes[feature_list$classes == "integer"] <- "numeric"

  return(feature_list)
}

#' Process (check and update) data according to specified feature list
#'
#' @param x matrix, data.frame or data.table. The data to check input for and update
#' according to the specification in \code{feature_list}.
#' @param feature_list List. Output from running \code{\link[shapr:get_data_specs]{get_data_specs}} or
#' \code{\link[shapr:get_model_specs]{get_model_specs}}
#'
#' @details This function takes care of all preprocessing and checking of the provided data in \code{x} against
#' the feature_list which is typically the output from \code{\link[shapr:get_model_specs]{get_model_specs}}
#'
#' @return List with two named elements: \code{x_dt}: Checked and updated data \code{x} in data.table format, and
#' \code{update_feature_list} the output from \code{\link[shapr:check_features]{check_features}}
#'
#' @author Martin Jullum
#'
#' @keywords internal
#' @export
#'
#' @examples
#' # Load example data
#' if (requireNamespace("MASS", quietly = TRUE)) {
#'   data("Boston", package = "MASS")
#'   # Split data into test- and training data
#'   x_train <- data.table::as.data.table(head(Boston))
#'   x_train[, rad := as.factor(rad)]
#'   data_features <- get_data_specs(x_train)
#'   model <- lm(medv ~ lstat + rm + rad + indus, data = x_train)
#'
#'   model_features <- get_model_specs(model)
#'   preprocess_data(x_train, model_features)
#' }
preprocess_data <- function(x, feature_list) {
  if (all(is.null(colnames(x)))) {
    stop(paste0("The data is missing column names"))
  }

  x_dt <- data.table::as.data.table(x)

  feature_list_data <- get_data_specs(x_dt)
  feature_list_data$specs_type <- "data"

  updater <- check_features(feature_list, feature_list_data,
    use_1_as_truth = T
  )
  update_data(x_dt, updater) # Updates x_dt by reference

  ret <- list(
    x_dt = x_dt,
    updated_feature_list = updater
  )

  return(ret)
}


#' Checks that two extracted feature lists have exactly the same properties
#'
#' @param f_list_1,f_list_2 List. As extracted from either \code{get_data_specs} or \code{get_model_specs}.
#' @param use_1_as_truth Logical. If TRUE, \code{f_list_2} is compared to \code{f_list_1}, i.e. additional elements
#' is allowed in \code{f_list_2}, and if \code{f_list_1}'s feature classes contains NAs, feature class check is
#' ignored regardless of what is specified in \code{f_list_1}. If FALSE, \code{f_list_1} and \code{f_list_2} are
#' equated and they need to contain exactly the same elements. Set to TRUE when comparing a model and data, and FALSE
#' when comparing two data sets.
#'
#' @return List. The \code{f_list_1} is returned as inserted if there all check are carried out. If some info is
#' missing from \code{f_list_1}, the function continues consistency checking using \code{f_list_2} and returns that.
#'
#' @author Martin Jullum
#'
#' @keywords internal
#' @export
#'
#' @examples
#' # Load example data
#' if (requireNamespace("MASS", quietly = TRUE)) {
#'   data("Boston", package = "MASS")
#'   # Split data into test- and training data
#'   x_train <- data.table::as.data.table(head(Boston))
#'   x_train[, rad := as.factor(rad)]
#'   data_features <- get_data_specs(x_train)
#'   model <- lm(medv ~ lstat + rm + rad + indus, data = x_train)
#'
#'   model_features <- get_model_specs(model)
#'   check_features(model_features, data_features)
#' }
check_features <- function(f_list_1, f_list_2,
                           use_1_as_truth = T) {
  if (is.null(f_list_1$specs_type)) {
    f_list_1$specs_type <- "model"
  }

  if (is.null(f_list_2$specs_type)) {
    f_list_2$specs_type <- "model"
  }

  name_1 <- f_list_1$specs_type
  name_2 <- f_list_2$specs_type

  if (name_1 == name_2) { # If used in explain after a model has NA-info during check in shapr
    name_1 <- paste0(name_1, "_train")
    name_2 <- paste0(name_2, "_test")
  }

  #### Checking that labels exists if required, otherwise stop or switch ####
  NULL_1 <- is.null(f_list_1$labels)
  NULL_2 <- is.null(f_list_2$labels)

  if (NULL_2 | (NULL_1 & !use_1_as_truth)) {
    stop(paste0("The ", name_1, " or ", name_2, " have missing column names. Handle that to proceed."))
  }
  if (NULL_1 & use_1_as_truth) {
    message(paste0(
      "The specified ", name_1, " provides NULL feature labels. ",
      "The labels of ", name_2, " are taken as the truth."
    ))
    f_list_1 <- f_list_2
  }

  NA_1 <- any(is.na(f_list_1$labels))
  NA_2 <- any(is.na(f_list_2$labels))

  if ((NA_1 & NA_2) | ((NA_1 | NA_2) & !use_1_as_truth)) {
    stop(paste0("The ", name_1, " or ", name_2, " have column names that are NA. Handle that to proceed."))
  }
  if ((NA_1 & use_1_as_truth)) {
    message(paste0(
      "The specified ", name_1, " provides feature labels that are NA. ",
      "The labels of ", name_2, " are taken as the truth."
    ))
    f_list_1 <- f_list_2
  }

  # feature names must be unique
  if (any(duplicated(f_list_1$labels))) {
    stop(paste0(name_1, " must have unique column names."))
  }

  # feature names must be unique
  if (any(duplicated(f_list_2$labels))) {
    stop(paste0(name_2, " must have unique column names."))
  }


  feat_in_1_not_in_2 <- f_list_1$labels[!(f_list_1$labels %in% f_list_2$labels)]
  feat_in_2_not_in_1 <- f_list_2$labels[!(f_list_2$labels %in% f_list_1$labels)]

  # Check that the features in 1 are in 2
  if (length(feat_in_1_not_in_2) > 0) {
    stop(
      paste0(
        "Feature(s) ",
        paste0(feat_in_1_not_in_2, collapse = ", "),
        " in ", name_1, " is not in ", name_2, "."
      )
    )
  }

  # Also check that the features in 2 are in 1
  if (!use_1_as_truth) {
    if (length(feat_in_2_not_in_1) > 0) {
      stop(
        paste0(
          "Feature(s) ",
          paste0(feat_in_2_not_in_1, collapse = ", "),
          " in ", name_2, " is not in ", name_1, "."
        )
      )
    }
  }

  # Check if any features have empty names i.e ""
  if (any(f_list_1$labels == "")) {
    stop("One or more features is missing a name.")
  }

  # Order classes and factor levels in the same way as labels
  # for f_list_1
  order_1 <- match(f_list_1$labels, names(f_list_1$classes))
  f_list_1$classes <- f_list_1$classes[order_1]
  f_list_1$factor_levels <- f_list_1$factor_levels[order_1]

  # for f_list_2
  order_2 <- match(f_list_2$labels, names(f_list_2$classes))
  f_list_2$classes <- f_list_2$classes[order_2]
  f_list_2$factor_levels <- f_list_2$factor_levels[order_2]

  # Reorder f_List_2 to match f_list_1, also removing anything in the former which is not in the latter ####
  f_list_2_reordering <- match(f_list_1$labels, f_list_2$labels)

  f_list_2$labels <- f_list_2$labels[f_list_2_reordering]
  f_list_2$classes <- f_list_2$classes[f_list_2_reordering]
  f_list_2$factor_levels <- f_list_2$factor_levels[f_list_2_reordering]

  # Sorts the factor levels for easier comparison below
  f_list_1$sorted_factor_levels <- lapply(f_list_1$factor_levels, FUN = sort)
  f_list_2$sorted_factor_levels <- lapply(f_list_2$factor_levels, FUN = sort)


  #### Checking classes ####
  if (any(is.na(f_list_1$classes)) & use_1_as_truth) { # Only relevant when f_list_1 is a model
    message(paste0(
      "The specified ", name_1, " provides feature classes that are NA. ",
      "The classes of ", name_2, " are taken as the truth."
    ))
    f_list_1 <- f_list_2
  }
  # Check if f_list_1 and f_list_2 have features with the same class
  if (!identical(f_list_1$classes, f_list_2$classes)) {
    stop(paste0("The features in ", name_1, " and ", name_2, " must have the same classes."))
  }

  # Check if the features all have class "integer", "numeric" or "factor
  if (!all(f_list_1$classes %in% c("integer", "numeric", "factor"))) {
    invalid_class <- which(!(f_list_1$classes %in% c("integer", "numeric", "factor")))
    stop(paste0(
      "Feature(s) ", paste0(f_list_1$labels[invalid_class], collapse = ", "), " in ", name_1, " and ", name_2,
      " is not of class integer, numeric or factor."
    ))
  }

  #### Checking factor levels ####
  factor_classes <- which(f_list_1$classes == "factor")
  if (length(factor_classes) > 0) {
    relevant_factor_levels <- f_list_1$factor_levels[factor_classes]
    is_NA <- any(is.na(relevant_factor_levels))
    is_NULL <- any(is.null(relevant_factor_levels))
    if ((is_NA | is_NULL) & use_1_as_truth) {
      message(paste0(
        "The specified ", name_1, " provides factor feature levels that are NULL or NA. ",
        "The factor levels of ", name_2, " are taken as the truth."
      ))
      f_list_1 <- f_list_2 # Always safe to switch as f_list_2 is based on data, and extracts correctly
    }
  }

  # Checking factor levels #
  if (!identical(f_list_1$sorted_factor_levels, f_list_2$sorted_factor_levels)) {
    stop(paste0("Some levels for factor features are not present in both ", name_1, " and ", name_2, "."))
  }

  f_list_1$sorted_factor_levels <- NULL # Not needed

  return(f_list_1) #
}

#' Updates data by reference according to the updater argument.
#'
#' @description \code{data} is updated, i.e. unused columns and factor levels are removed as described in
#' \code{updater}. This is done by reference, i.e. updates the object being passed to data even if nothing is
#' returned by the function itself.
#'
#' @param data data.table. Data that ought to be updated.
#' @param updater List. The object should be the output from
#' \code{\link[shapr:check_features]{check_features}}.
#'
#'
#' @return NULL.
#'
#' @author Martin Jullum
#'
#' @keywords internal
#' @export
#'
#' @examples
#' # Load example data
#' if (requireNamespace("MASS", quietly = TRUE)) {
#'   data("Boston", package = "MASS")
#'   # Split data into test- and training data
#'   x_train <- data.table::as.data.table(head(Boston))
#'   x_train[, rad := as.factor(rad)]
#'   data_features <- get_data_specs(x_train)
#'   model <- lm(medv ~ lstat + rm + rad + indus, data = x_train)
#'
#'   model_features <- get_model_specs(model)
#'   updater <- check_features(model_features, data_features)
#'   update_data(x_train, updater)
#' }
update_data <- function(data, updater) {
  # Operates on data by reference, so no copying of data here

  new_labels <- updater$labels
  factor_levels <- updater$factor_levels

  # Reorder and delete unused columns
  cnms_remove <- setdiff(colnames(data), new_labels)
  if (length(cnms_remove) > 0) {
    message(
      paste0(
        "The columns(s) ",
        paste0(cnms_remove, collapse = ", "),
        " is not used by the model and thus removed from the data."
      )
    )
    data[, (cnms_remove) := NULL]
  }
  data.table::setcolorder(data, new_labels)

  # Reorderes the factor levels
  if (any(updater$classes == "factor")) {
    org_factor_levels <- lapply(data, levels)
    identical_levels <- mapply(FUN = "identical", org_factor_levels, factor_levels)
    if (any(!identical_levels)) {
      changed_levels <- which(!identical_levels)
      message(paste0(
        "Levels are reordered for the factor feature(s) ",
        paste0(new_labels[changed_levels], collapse = ", "), "."
      ))

      for (i in changed_levels) {
        data.table::set(data,
          j = i,
          value = factor(unlist(data[, new_labels[i], with = F], use.names = F), levels = factor_levels[[i]])
        )
      }
    }
  }

  return(NULL)
}
