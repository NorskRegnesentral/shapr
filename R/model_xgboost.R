#' @rdname predict_model
#' @export
predict_model.xgb.Booster <- function(x, newdata) {
  if (!requireNamespace("stats", quietly = TRUE)) {
    stop("The xgboost package is required for predicting xgboost models")
  }

  if (is.null(x$feature_list)) {
    predict(x, as.matrix(newdata))
  } else {
    newdata_dummy <- apply_dummies(feature_list = x$feature_list, testdata = newdata)
    predict(x, as.matrix(newdata_dummy))
  }
}

#' @rdname get_model_specs
#' @export
get_model_specs.xgb.Booster <- function(x) {
  model_checker(x) # Checking if the model is supported

  feature_list <- list()
  if (is.null(x[["feature_list"]])) {
    feature_list$labels <- x$feature_names
    m <- length(feature_list$labels)

    feature_list$classes <- setNames(rep(NA, m), feature_list$labels) # Not supported
    feature_list$factor_levels <- setNames(vector("list", m), feature_list$labels)
  } else {
    feature_list <- x$feature_list
  }

  return(feature_list)
}

#' @rdname model_checker
#' @export
model_checker.xgb.Booster <- function(x) {
  if (!is.null(x$params$objective) &&
      (x$params$objective == "multi:softmax" | x$params$objective == "multi:softprob")
  ) {
    stop(
      paste0(
        "\n",
        "We currently don't support multi-classification using xgboost, i.e.\n",
        "where num_class is greater than 2."
      )
    )
  }

  if (!is.null(x$params$objective) && x$params$objective == "reg:logistic") {
    stop(
      paste0(
        "\n",
        "We currently don't support standard classification, which predicts the class directly.\n",
        "To train an xgboost model predicting the class probabilities, you'll need to change \n",
        "the objective to 'binary:logistic'"
      )
    )
  }
  return(NULL)
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
#' shapr}
#' \item{testdata_new}{Original testdata with correct column ordering and factor levels. To be passed to
#' \code{\link[shapr:explain]{explain}.}}
#' }
#'
#' @export
#'
#' @author Annabelle Redelmeier, Martin Jullum
#'
#' @examples
#' # Load example data
#' data("airquality")
#' airquality <- airquality[complete.cases(airquality), ]
#' x_var <- c("Solar.R", "Wind", "Temp", "Month")
#' y_var <- "Ozone"
#' x_train <- as.data.frame(airquality[100:110, x_var])
#' y_train <- airquality[100:110, y_var]
#' x_explain <- as.data.frame(airquality[1:4, x_var])
#'
#' # convert to factors for illustational purpose
#' x_train$Temp <- factor(round(x_train$Temp, -1))
#' x_explain$Temp <- factor(round(x_explain$Temp, -1), levels = levels(x_train$Temp))
#'
#' dummylist <- make_dummies(traindata = x_train, testdata = x_explain)
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
