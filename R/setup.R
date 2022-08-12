

#' check_setup
#' @inheritParams explain
#' @param feature_specs List. The output from [get_model_specs()] or [get_data_specs()].
#' Contains the 3 elements:
#' \describe{
#'   \item{labels}{Character vector with the names of each feature.}
#'   \item{classes}{Character vector with the classes of each features.}
#'   \item{factor_levels}{Character vector with the levels for any categorical features.}
#'   }
#' @param is_python Logical. Indicates whether the function is called from the Python wrapper. Default is FALSE which is
#' never changed when calling the function via \code{explain()} in R. The parameter is later used to disallow
#' running the AICc-versions of the empirical as that requires data based optimization.
#' @export
setup <- function(x_train,
                  x_explain,
                  approach,
                  prediction_zero,
                  n_combinations,
                  group,
                  n_samples,
                  n_batches,
                  seed,
                  keep_samp_for_vS,
                  feature_specs,
                  is_python = FALSE, ...) {
  internal <- list()

  internal$parameters <- get_parameters(
    approach = approach,
    prediction_zero = prediction_zero,
    n_combinations = n_combinations,
    group = group,
    n_samples = n_samples,
    n_batches = n_batches,
    seed = seed,
    keep_samp_for_vS = keep_samp_for_vS,
    is_python = is_python, ...
  )

  internal$data <- get_data(
    x_train,
    x_explain
    )

  internal$objects <- list(feature_specs=feature_specs)

  check_data(internal)

  internal <- get_extra_parameters(internal) # This includes both extra parameters and other objects

  check_parameters(internal)

  return(internal)
}

#' @keywords internal
check_parameters <- function(internal){

  # Check groups
  feature_names <- internal$parameters$feature_names
  group <- internal$parameters$group
  if(!is.null(group)){
    check_groups(feature_names,group)
  }

  # Checking n_batches vs n_combinations etc
  check_n_batches(internal)

  # Check approach
  check_approach(internal)

}

#' @keywords internal
check_n_batches <- function(internal){
  n_batches <- internal$parameters$n_batches
  n_features <- internal$parameters$n_features
  n_combinations <- internal$parameters$n_combinations
  is_groupwise <- internal$parameters$is_groupwise
  n_groups <- internal$parameters$n_groups

  if(!is_groupwise){
    actual_n_combinations <- ifelse(is.null(n_combinations),2^n_features,n_combinations)
  } else {
    actual_n_combinations <- ifelse(is.null(n_combinations),2^n_groups,n_combinations)
  }

  if (n_batches > actual_n_combinations) {
    stop(paste0("`n_batches` (",n_batches,") is greater than the number feature combinations/`n_combinations` (",
                actual_n_combinations,")"))
  }
}





#' @keywords internal
check_data <- function(internal){

  # Check model and data compatability
  x_train <- internal$data$x_train
  x_explain <- internal$data$x_explain

  model_feature_specs <- internal$objects$feature_specs

  x_train_feature_specs <- get_data_specs(x_train)
  x_explain_feature_specs <- get_data_specs(x_explain)

  factors_exists <- any(model_feature_specs$classes=="factor")

  NA_labels <- any(is.na(model_feature_specs$labels))
  NA_classes <- any(is.na(model_feature_specs$classes))
  NA_factor_levels <- any(is.na(model_feature_specs$factor_levels))


  if(is.null(model_feature_specs)){
    message("Note: You passed a model to explain() which is not natively supported, and did not supply a ",
            "'get_model_specs' function to explain().\n",
            "Consistency checks between model and data is therefore disabled.\n")

    model_feature_specs <- x_train_feature_specs

  } else if(NA_labels){

    message("Note: Feature names extracted from the model contains NA.\n",
            "Consistency checks between model and data is therefore disabled.\n")

    model_feature_specs <- x_train_feature_specs

  } else if(NA_classes){
    message("Note: Feature classes extracted from the model contains NA.\n",
            "Assuming feature classes from the data are correct.\n")

    model_feature_specs$classes <- x_train_feature_specs$classes
    model_feature_specs$factor_levels <- x_train_feature_specs$factor_levels

  } else if (factors_exists & NA_factor_levels){

    message("Note: Feature factor levels extracted from the model contains NA.\n",
            "Assuming feature factor levels from the data are correct.\n")

    model_feature_specs$factor_levels <- x_train_feature_specs$factor_levels

  }


  # First check model vs x_train (possibly modified)
  # Then x_train vs x_explain
  compare_feature_specs(model_feature_specs,x_train_feature_specs,"model","x_train")
  compare_feature_specs(x_train_feature_specs,x_explain_feature_specs,"x_train","x_explain")


}

compare_vecs <- function(vec1,vec2,vec_type,name1,name2){
  if(!identical(vec1,vec2)){
    if(is.null(names(vec1))){
      text_vec1 <- paste(vec1, collapse = ", ")
    } else {
      text_vec1 <- paste(names(vec1), vec1, sep = ": ", collapse = ", ")
    }
    if(is.null(names(vec2))){
      text_vec2 <- paste(vec2, collapse = ", ")
    } else {
      text_vec2 <- paste(names(vec2), vec1, sep = ": ", collapse = ", ")
    }

    stop(paste0("Feature ",vec_type," are not identical for ",name1," and ",name2,".\n",
                name1," provided: ",text_vec1,",\n",
                name2," provided: ",text_vec2,".\n")
         )
  }
}

compare_feature_specs <- function(spec1,spec2,name1="model",name2="x_train"){
  compare_vecs(spec1$labels,spec2$labels,"names",name1,name2)
  compare_vecs(spec1$classes,spec2$classes,"classes",name1,name2)

  factor_classes <- which(spec1$classes == "factor")
  if (length(factor_classes) > 0) {
    for(fact in names(factor_classes)){
      vec_type = paste0("factor levels for feature '",fact,"'")
      compare_vecs(spec1$factor_levels[[fact]],spec2$factor_levels[[fact]],vec_type,name1,name2)
    }
  }

}


#' This includes both extra parameters and other objects
#' @keywords internal
get_extra_parameters <- function(internal){

  # get number of features and observations to explain
  internal$parameters$n_features <- ncol(internal$data$x_explain)
  internal$parameters$n_explain <- nrow(internal$data$x_explain)
  internal$parameters$n_train <- nrow(internal$data$x_train)

  # Names of features (already checked to be OK)
  internal$parameters$feature_names = names(internal$data$x_explain)

  # Update feature_specss (in case model based spec included NAs)
  internal$objects$feature_specs = get_data_specs(internal$data$x_explain)

  internal$parameters$is_groupwise <- !is.null(internal$parameters$group)

  # Processes groups if specified. Otherwise do nothing
  if(internal$parameters$is_groupwise){
    group <- internal$parameters$group

    # Make group names if not existing
    if (is.null(names(group))) {
      message(
        "\nSuccess with message:\n
      Group names not provided. Assigning them the default names 'group1', 'group2', 'group3' etc."
      )
      names(internal$parameters$group) <- paste0("group", seq_along(group))
    }

    # Make group list with numeric feature indicators
    internal$objects$group_num <- lapply(group, FUN = function(x) {
      match(x, internal$parameters$feature_names)
    })

    internal$parameters$n_groups <- length(group)

  } else {
    internal$objects$group_num <- NULL
    internal$parameters$n_groups <- NULL
  }

  return(internal)
}

#' @keywords internal
get_parameters <- function(approach, prediction_zero, n_combinations, group, n_samples,
                           n_batches, seed, keep_samp_for_vS, is_python, ...) {

  # Check input type for approach

  # approach is checked later

  # prediction_zero
  if(!(is.numeric(prediction_zero) &&
       length(prediction_zero)==1 &&
       !is.na(prediction_zero))){
    stop("`prediction_zero` must be a single numeric.")
  }
  # n_combinations
  if(!is.null(n_combinations) &&
     !(is.wholenumber(n_combinations) &&
       length(n_combinations)==1 &&
       !is.na(n_combinations) &&
       n_combinations > 0)){
    stop("`n_combinations` must be NULL or a single positive integer.")
  }

  # group (checked more thoroughly later)
  if (!is.null(group) &&
      !is.list(group)){
    stop("`group` must be NULL or a list")
  }

  # n_samples
  if(!(is.wholenumber(n_samples) &&
       length(n_samples)==1 &&
       !is.na(n_samples) &&
       n_samples > 0)){
    stop("`n_samples` must be a single positive integer.")
  }
  # n_batches
  if(!(is.wholenumber(n_batches) &&
       length(n_batches)==1 &&
       !is.na(n_batches) &&
       n_batches > 0)){
    stop("`n_batches` must be a single positive integer.")
  }
  # seed is already set, so we know it works
  # keep_samp_for_vS
  if(!(is.logical(keep_samp_for_vS) &&
       length(keep_samp_for_vS)==1)){
    stop("`keep_samp_for_vS` must be single logical.")
  }


  # Getting basic input parameters
  parameters <- list(
    approach = approach,
    prediction_zero = prediction_zero,
    n_combinations = n_combinations,
    group = group,
    n_samples = n_samples,
    n_batches = n_batches,
    seed = seed,
    keep_samp_for_vS = keep_samp_for_vS,
    is_python = is_python
  )

  # Getting additional parameters from ...
  parameters <- append(parameters, list(...))


  # Setting exact based on n_combinations (TRUE if NULL)
  parameters$exact <- ifelse(is.null(parameters$n_combinations), TRUE, FALSE)

  return(parameters)
}

#' @keywords internal
get_data <- function(x_train, x_explain) {

  # Check data object type
  stop_message <- ""
  if (!is.matrix(x_train) && !is.data.frame(x_train)) {
    stop_message <- paste0(stop_message,"x_train should be a matrix or a data.frame/data.table.\n")
  }
  if (!is.matrix(x_explain) && !is.data.frame(x_explain)) {
    stop_message <- paste0(stop_message,"x_explain should be a matrix or a data.frame/data.table.\n")
  }
  if(stop_message!=""){
    stop(stop_message)
  }

  # Check column names
  if (all(is.null(colnames(x_train)))) {
    stop_message <- paste0(stop_message,"x_train misses column names.\n")
  }
  if (all(is.null(colnames(x_explain)))) {
    stop_message <- paste0(stop_message,"x_explain misses column names.\n")
  }
  if(stop_message!=""){
    stop(stop_message)
  }


  data <- list(
    x_train = data.table::as.data.table(x_train),
    x_explain = data.table::as.data.table(x_explain)
  )
}




#' Fetches feature information from a given data set
#'
#' @param x matrix, data.frame or data.table The data to extract feature information from.
#'
#' @details This function is used to extract the feature information to be checked against the corresponding
#' information extracted from the model and other data sets. The function is called from internally
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
#' data("airquality")
#' airquality <- airquality[complete.cases(airquality), ]
#' # Split data into test- and training data
#' x_train <- head(airquality, -3)
#' x_explain <- tail(airquality, 3)
#' # Split data into test- and training data
#' x_train <- data.table::as.data.table(head(airquality))
#' x_train[, Temp := as.factor(Temp)]
#' get_data_specs(x_train)
get_data_specs <- function(x) {

  feature_specs <- list()
  feature_specs$labels <- names(x)
  feature_specs$classes <- unlist(lapply(x, class))
  feature_specs$factor_levels <- lapply(x, levels)

  # Defining all integer values as numeric
  feature_specs$classes[feature_specs$classes == "integer"] <- "numeric"

  return(feature_specs)
}



#' Check that the group parameter has the right form and content
#'
#'
#' @param feature_names Vector of characters. Contains the feature labels used by the model
#'
#' @return Error or NULL
#'
#' @keywords internal
check_groups <- function(feature_names, group) {
  if (!is.list(group)) {
    stop("group must be a list")
  }

  group_features <- unlist(group)

  # Checking that the group_features are characters
  if (!all(is.character(group_features))) {
    stop("All components of group should be a character.")
  }

  # Check that all features in group are in feature labels or used by model
  if (!all(group_features %in% feature_names)) {
    missing_group_feature <- group_features[!(group_features %in% feature_names)]
    stop(
      paste0(
        "The group feature(s) ", paste0(missing_group_feature, collapse = ", "), " are not\n",
        "among the features in the data: ",paste0(feature_names,collapse = ", "),". Delete from group."
      )
    )
  }

  # Check that all feature used by model are in group
  if (!all(feature_names %in% group_features)) {
    missing_features <- feature_names[!(feature_names %in% group_features)]
    stop(
      paste0(
        "The data feature(s) ", paste0(missing_features, collapse = ", "), " do not\n",
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
}

#' @keywords internal
check_approach <- function(internal) {
  # Check length of approach

  approach <- internal$parameters$approach
  n_features <- internal$parameters$n_features
  supported_approaches <- get_supported_approaches()

  if (!(is.character(approach)&&
        (length(approach) == 1 | length(approach) == n_features) &&
        all(is.element(approach, supported_approaches)))
  ) {
    stop(
      paste(
        "`approach` must be one of the following: \n", paste0(supported_approaches, collapse = ", "), "\n",
        "or a vector of length equal to the number of features (",n_features,") with only the above strings."
      )
    )
  }
}

#' Gets the implemented approaches
#'
#' @return Character vector.
#' The names of the implemented approaches that can be passed to argument \code{approach} in [explain()].
#'
#' @export
get_supported_approaches <- function() {
  substring(rownames(attr(methods(prepare_data), "info")), first = 14)
}
