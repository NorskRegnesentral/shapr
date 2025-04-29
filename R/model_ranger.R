#' @rdname predict_model
#' @export
predict_model.ranger <- function(x, newdata, ...) {
  if (!requireNamespace("ranger", quietly = TRUE)) {
    cli::cli_abort("The ranger package is required for predicting ranger models")
  }

  if (x$treetype == "Probability estimation") {
    predict(x, newdata)$predictions[, 2]
  } else {
    predict(x, newdata)$predictions
  }
}

#' @rdname get_model_specs
#' @export
get_model_specs.ranger <- function(x) {
  model_checker(x) # Checking if the model is supported

  feature_specs <- list()
  feature_specs$labels <- unique_features(x$forest$independent.variable.names)
  m <- length(feature_specs$labels)

  feature_specs$classes <- setNames(rep(NA, m), feature_specs$labels) # Not supported
  feature_specs$factor_levels <- setNames(vector("list", m), feature_specs$labels)

  # Only provided when respect.unordered.factors == T
  feature_specs$factor_levels[names(x$forest$covariate.levels)] <- x$forest$covariate.levels

  return(feature_specs)
}


#' @rdname model_checker
#' @name model_checker
#' @export
model_checker.ranger <- function(x) {
  if (x$treetype == "Classification") {
    cli::cli_abort(
      paste0(
        "We currently don't support standard classification, which predicts the class directly. ",
        "To train a ranger model predicting the class probabilities, you'll need to grow a ",
        "probability forest by setting probability = TRUE in {.fn ranger::ranger}."
      )
    )
  }

  if (x$treetype == "survival") {
    cli::cli_abort(
      paste0(
        "We currently don't support explanation of survival type of ranger models."
      )
    )
  }

  if (x$treetype == "Probability estimation" && length(x$forest$levels) > 2) {
    cli::cli_abort(
      paste0(
        "We currently don't support multi-classification using ranger, i.e. ",
        "where `length(model$forest$levels)` is greater than 2."
      )
    )
  }

  # Additional check
  if (is.null(x$forest)) {
    cli::cli_abort(
      paste0(
        "It looks like the model was fitted without saving the forest. Please set ",
        "write.forest = TRUE when fitting a model using {.fn ranger::ranger}."
      )
    )
  }


  return(NULL)
}

#' @keywords internal
unique_features <- function(x) {
  unique(
    unlist(
      strsplit(x, split = ":", fixed = TRUE)
    )
  )
}
