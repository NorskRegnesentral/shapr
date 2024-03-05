# THIS IS NOT DONE!

#' @rdname predict_model
#' @export
predict_model.workflow <- function(x, newdata, ...) {
  if (!requireNamespace("stats", quietly = TRUE)) {
    stop("The xgboost package is required for predicting xgboost models")
  }

  predict(x, as.matrix(newdata))
}


#' @rdname get_model_specs
#' @export
get_model_specs.workflow <- function(x) {
  model_checker(x) # Checking if the model is supported

  feature_specs <- list()
  feature_specs$labels <- x$feature_names
  m <- length(feature_specs$labels)

  feature_specs$classes <- setNames(rep(NA, m), feature_specs$labels) # Not supported
  feature_specs$factor_levels <- setNames(vector("list", m), feature_specs$labels)

  return(feature_specs)


  model_checker(x) # Checking if the model is supported

  feature_specs <- list()
  feature_specs$labels <- shapr:::unique_features(x$forest$independent.variable.names)
  m <- length(feature_specs$labels)

  feature_specs$classes <- setNames(rep(NA, m), feature_specs$labels) # Not supported
  feature_specs$factor_levels <- setNames(vector("list", m), feature_specs$labels)

  # Only provided when respect.unordered.factors == T
  feature_specs$factor_levels[names(x$forest$covariate.levels)] <- x$forest$covariate.levels

  return(feature_specs)


  lm_fit <-
    linear_reg() %>%
    fit(width ~ initial_volume * food_regime, data = urchins)
  lm_fit

  lm_fit_w = workflow() %>% add_model(linear_reg()) %>% add_recipe(recipes::recipe(y ~ ., data = x_train)) %>% fit(data = x_train)
  lm_fit_w$trained
  class(lm_fit_w)
  tmp = lm_fit_w$pre$actions$recipe$recipe$var_info
  tmp

  rf_fit <-
    rand_forest("regression") %>%
    fit(width ~ initial_volume * food_regime, data = urchins)

  rf_fit

  rf_fit_w <- workflow() %>%
    add_model(rand_forest("regression")) %>%
    add_recipe(recipes::recipe(width ~ initial_volume + food_regime, data = urchins)) %>%
    fit(data = urchins)

  rf_fit_w$fit
  tmp = rf_fit_w$pre$actions$recipe$recipe$var_info
  feature_names = tmp$variable[tmp$role == "predictor"]

  rf_fit_w$pre$actions$recipe$recip$template
  lapply(rf_fit_w$pre$actions$recipe$recip$template, levels)

  x = rf_fit_w$fit$fit$fit



  rf_fit$spec$mode
  class(rf_fit)

  rf_fit$fit$forest$independent.variable.names
  rf_fit$preproc$xlevels
  rf_fit$lvl
  extract_fit_parsnip(rf_fit)
  summary(rf_fit)

}



#' @rdname model_checker
#' @export
model_checker.workflow <- function(x) {


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
