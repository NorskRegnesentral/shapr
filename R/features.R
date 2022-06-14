

#' @keywords internal
group_fun <- function(x, group_num) {
  if (length(x) != 0) {
    unlist(group_num[x])
  } else {
    integer(0)
  }
}


#' Check that the group parameter has the right form and content
#'
#' @inheritParams shapr
#' @param feature_labels Vector of characters. Contains the feature labels used by the model
#'
#' @return Error or NULL
#'
#' @keywords internal
check_groups <- function(feature_labels, group) {
  if (!is.list(group)) {
    stop("group must be a list")
  }

  group_features <- unlist(group)

  # Checking that the group_features are characters
  if (!all(is.character(group_features))) {
    stop("All components of group should be a character.")
  }

  # Check that all features in group are in feature labels or used by model
  if (!all(group_features %in% feature_labels)) {
    missing_group_feature <- group_features[!(group_features %in% feature_labels)]
    stop(
      paste0(
        "The group feature(s) ", paste0(missing_group_feature, collapse = ", "), " are not\n",
        "among the features specified by the model/data. Delete from group."
      )
    )
  }

  # Check that all feature used by model are in group
  if (!all(feature_labels %in% group_features)) {
    missing_features <- feature_labels[!(feature_labels %in% group_features)]
    stop(
      paste0(
        "The model/data feature(s) ", paste0(missing_features, collapse = ", "), " do not\n",
        "belong to one of the groups. Add to a group."
      )
    )
  }

  # Check uniqueness of group_features
  if (length(group_features) != length(unique(group_features))) {
    dups <- group_features[duplicated(group_features)]
    stop(
      paste0(
        "Feature(s) ", paste0(dups, collapse = ", "), " are found in more than one group or ",
        "multiple times per group.\n",
        "Make sure each feature is only represented in one group, and only once."
      )
    )
  }
  return(NULL)
}







#' @keywords internal
helper_feature <- function(m, feature_sample) {
  sample_frequence <- is_duplicate <- NULL # due to NSE notes in R CMD check

  x <- feature_matrix_cpp(feature_sample, m)
  dt <- data.table::data.table(x)
  cnms <- paste0("V", seq(m))
  data.table::setnames(dt, cnms)
  dt[, sample_frequence := as.integer(.N), by = cnms]
  dt[, is_duplicate := duplicated(dt)]
  dt[, (cnms) := NULL]

  return(dt)
}

#' Initiate the making of dummy variables
#'
#' @param traindata data.table or data.frame.
#'
#' @param testdata data.table or data.frame. New data that has the same
#' feature names, types, and levels as \code{traindata}.
#'
#' @return A list that contains the following entries:
#' \describe{
#' \item{feature_list}{List. Output from \code{check_features}}
#' \item{train_dummies}{A data.frame containing all of the factors in \code{traindata} as
#' one-hot encoded variables.}
#' \item{test_dummies}{A data.frame containing all of the factors in \code{testdata} as
#' one-hot encoded variables.}
#' \item{traindata_new}{Original traindata with correct column ordering and factor levels. To be passed to
#' \code{\link[shapr:shapr]{shapr}.}}
#' \item{testdata_new}{Original testdata with correct column ordering and factor levels. To be passed to
#' \code{\link[shapr:explain]{explain}.}}
#' }
#'
#' @export
#'
#' @author Annabelle Redelmeier, Martin Jullum
#'
#' @examples
#' if (requireNamespace("MASS", quietly = TRUE)) {
#'   data("Boston", package = "MASS")
#'   x_var <- c("lstat", "rm", "dis", "indus")
#'   y_var <- "medv"
#'   x_train <- as.data.frame(Boston[401:411, x_var])
#'   y_train <- Boston[401:408, y_var]
#'   x_explain <- as.data.frame(Boston[1:4, x_var])
#'
#'   # convert to factors for illustational purpose
#'   x_train$rm <- factor(round(x_train$rm))
#'   x_explain$rm <- factor(round(x_explain$rm), levels = levels(x_train$rm))
#'
#'   dummylist <- make_dummies(traindata = x_train, testdata = x_explain)
#' }
make_dummies <- function(traindata, testdata) {
  if (all(is.null(colnames(traindata)))) {
    stop(paste0("The traindata is missing column names"))
  }

  if (all(is.null(colnames(testdata)))) {
    stop(paste0("The testdata is missing column names"))
  }

  train_dt <- data.table::as.data.table(traindata)
  test_dt <- data.table::as.data.table(testdata)

  feature_list_train <- get_data_specs(train_dt)
  feature_list_test <- get_data_specs(test_dt)

  feature_list_train$specs_type <- "traindata"
  feature_list_test$specs_type <- "testdata"

  updater <- check_features(feature_list_train, feature_list_test, F)

  # Reorderes factor levels so that they match each other
  update_data(train_dt, updater)
  update_data(test_dt, updater)

  feature_list <- updater

  # Extracts the components
  factor_features <- feature_list$labels[updater$classes == "factor"]

  if (length(factor_features) > 0) {
    factor_list <- feature_list$factor_levels[factor_features]
    feature_list$contrasts_list <- lapply(train_dt[, factor_features, with = FALSE], contrasts, contrasts = FALSE)

    # get train dummies
    m <- model.frame(
      data = train_dt,
      xlev = factor_list
    )
    train_dummies <- model.matrix(
      object = ~ . + 0,
      data = m,
      contrasts.arg = feature_list$contrasts_list
    )

    # get test dummies
    m <- model.frame(
      data = test_dt,
      xlev = factor_list
    )
    test_dummies <- model.matrix(
      object = ~ . + 0,
      data = m,
      contrasts.arg = feature_list$contrasts_list
    )
  } else {
    train_dummies <- train_dt
    test_dummies <- test_dt
  }

  return(list(
    feature_list = feature_list,
    train_dummies = train_dummies, test_dummies = test_dummies, traindata_new = train_dt,
    testdata_new = test_dt
  ))
}

#' Apply dummy variables - this is an internal function intended only to be used in
#' predict_model.xgb.Booster()
#'
#' @param feature_list List. The \code{feature_list} object in the output object after running
#' \code{\link[shapr:make_dummies]{make_dummies}}
#'
#' @param testdata data.table or data.frame. New data that has the same
#' feature names, types, and levels as \code{feature_list}.
#'
#' @return A data.table with all features but where the factors in \code{testdata} are
#' one-hot encoded variables as specified in feature_list
#'
#' @author Annabelle Redelmeier, Martin Jullum
#'
#' @keywords internal
#'
apply_dummies <- function(feature_list, testdata) {
  if (all(is.null(colnames(testdata)))) {
    stop(paste0("The testdata is missing column names"))
  }
  test_dt <- data.table::as.data.table(testdata)

  feature_list_test <- get_data_specs(test_dt)

  feature_list_test$specs_type <- "testdata"

  updater <- check_features(feature_list, feature_list_test, F)

  # Reorderes factor levels so that they match
  update_data(test_dt, updater)

  factor_features <- feature_list$labels[updater$classes == "factor"] # check which features are factors

  if (length(factor_features) > 0) {
    factor_list <- feature_list$factor_levels[factor_features]

    m <- model.frame(
      data = test_dt,
      xlev = factor_list
    )

    x <- model.matrix(
      object = ~ . + 0,
      data = m,
      contrasts.arg = feature_list$contrasts_list
    )
  } else {
    x <- test_dt
  }

  return(x)
}
