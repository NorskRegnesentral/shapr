
model_setup_R <- function(predict_model, get_model_specs, model){

  model_class <- NULL # due to NSE

  model_class0 <- class(model)

  # predict_model
  if(!(is.function(predict_model)) &&
     !(is.null(predict_model))){
    stop("`predict_model` must be NULL or a function.")
  }
  # get_model_specs
  if(!is.function(get_model_specs) &&
     !is.null(get_model_specs) &&
     !is.na(get_model_specs)){
    stop("`get_model_specs` must be NULL, NA or a function.") # NA is used to avoid using internally defined get_model_specs where this is defined and not valid for the specified model
  }

  model_objects <- list(
    predict_model = predict_model,
    get_model_specs = get_model_specs
  )

  supported_models <- get_supported_models()

  # Get native predict_model if not passed and exists
  if (is.null(funcs$predict_model)) {
    native_func_available <- supported_models[predict_model == TRUE, model_class0 %in% model_class]
    if (native_func_available) {
      model_objects$predict_model <- get(paste0("predict_model.", model_class0))
    } else {
      stop(
        "You passed a model to explain() which is not natively supported, and did not supply the 'predict_model' ",
        "function to explain().\n",
        "See ?shapr::explain or the vignette for more information on how to run shapr with custom models."
      )
    }
  }

  # Get native get_model_specs if not passed and exists
  if (is.null(model_objects$get_model_specs)) {
    native_func_available <- supported_models[get_model_specs == TRUE, model_class0 %in% model_class]
    if (native_func_available) {
      model_objects$get_model_specs <- get(paste0("get_model_specs.", model_class0))
    }
  }


  # Get feature_specs
  if (is.function(model_objects$get_model_specs)) {
    model_objects$feature_specs <- model_objects$get_model_specs(model)
  } else {
    model_objects$feature_specs <- NULL
  }


  return(model_objects)
}


