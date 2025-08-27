#' Get feature specifications from the model
#'
#' @inheritParams explain
#' @keywords internal
get_feature_specs <- function(get_model_specs, model) {
  # Check that get_model_specs is a proper function (R + Python)
  # Extract natively supported functions for get_model_specs if they exist and are not passed (R only)
  # Apply get_model_specs to model and check that it provides the right output format (R and Python)
  # Return feature_specs (R and py)

  model_class <- NULL # due to NSE

  model_class0 <- class(model)[1]

  # get_model_specs
  if (!is.function(get_model_specs) &&
    !is.null(get_model_specs) &&
    !is.na(get_model_specs)) {
    cli::cli_abort("`get_model_specs` must be NULL, NA, or a function.")
    # NA is used to avoid using internally defined get_model_specs where this is
    # defined and not valid for the specified model
  }

  supported_models <- get_supported_models()


  # Get native get_model_specs if not passed and available
  if (is.null(get_model_specs)) {
    native_func_available <- supported_models[get_model_specs == TRUE, model_class0 %in% model_class]
    if (native_func_available) {
      get_model_specs <- get(paste0("get_model_specs.", model_class0))
    } else {
      # The checks are disabled in the check_data function
    }
  }

  # Get feature_specs from the model object by get_model_specs(model)
  if (is.function(get_model_specs)) {
    # Tests the get_model_specs function
    feature_specs <- tryCatch(get_model_specs(model), error = errorfun)
    if (class(feature_specs)[1] == "error") {
      cli::cli_abort(paste0(
        "The `get_model_specs` function for class `", model_class0, "` is invalid. ",
        "See the 'Advanced usage' section of ",
        "{.vignette shapr::general_usage} vignette ",
        "for more information on running shapr with custom models. ",
        "Note that `get_model_specs` is not required (can be set to NULL) ",
        "unless you require consistency checks between model and data. ",
        "A basic function test threw the following error: ", as.character(feature_specs[[1]])
      ))
    }

    if (!(is.list(feature_specs) &&
      length(feature_specs) == 3 &&
      all(names(feature_specs) == c("labels", "classes", "factor_levels")))) {
      cli::cli_abort(
        paste0(
          "The `get_model_specs` function for class `", model_class0,
          "` does not return a list of length 3 with elements \"labels\", \"classes\", and \"factor_levels\". ",
          "See the 'Advanced usage' section of ",
          "{.vignette shapr::general_usage} vignette ",
          "for more information on running shapr with custom models and the required output format of get_model_specs."
        )
      )
    }
  } else {
    feature_specs <- NULL
  }


  return(feature_specs)
}
