#' Fetches feature labels from a given model object
#'
#' @param x matrix, data.frame or data.table The data to extract feature information from.
#'
#' @keywords internal
#'
#' @export
#'
#' @examples
#'# Load example data
#' data("Boston", package = "MASS")
#' # Split data into test- and training data
#' x_train <- data.table::as.data.table(head(Boston))
#' x_train[,rad:=as.factor(rad)]
#' get_data_specs(x_train)
get_data_specs <- function(x){

  x <- data.table::as.data.table(x)

  feature_list = list()
  feature_list$labels <- names(x)
  feature_list$classes <- unlist(lapply(x,class))
  feature_list$factor_levels = lapply(x,levels)
  feature_list$specs_type <- "data"

  # Defining all integer values as numeric
  feature_list$classes[feature_list$classes=="integer"] <- "numeric"

  return(feature_list)
}

#' Process (check and update) data according to specified feature list
#'
#' @param x matrix, data.frame or data.table. The data to check input for and update
#' according to the specification in \code{feature_list}.
#' @param feature_list List. Output from running \code{get_data_specs} or \code{get_model_specs}
#'
#' @return Checked and updated data \code{x} in data.table format.
#'
#' @keywords internal
#'
#' @export
#'
preprocess_data = function(x,feature_list){
  if(all(is.null(colnames(x)))){
    stop(paste0("The data is missing column names"))
  }

  x_dt <- data.table::as.data.table(x)

  feature_list_data <- get_data_specs(x_dt)

  updater <- check_features(feature_list,feature_list_data,
                            use_1_as_truth = T)
  update_data(x_dt,updater) # Updates x_dt by reference

  ret <- list(x_dt = x_dt,
              updated_feature_list = updater)

  return(ret)
}


#' Checks that two extracted feature lists have exactly the same properites
#'
#' @param f_list_1,f_list_2 List. As extracted from either \code{get_data_specs} or \code{get_model_specs}.
#' @param use_1_as_truth Logical. If TRUE, \code{f_list_2} is compared to \code{f_list_1}, i.e. additional elements
#' is allowed in \code{f_list_2}, and if \code{f_list_1}'s feature classes contains NA's, feature class check is
#' ignored regardless of what is specified in \code{f_list_1}. If FALSE, \code{f_list_1} and \code{f_list_2} are
#' equated and they need to contain exactly the same elements. Set to TRUE when comparing a model and data, and FALSE
#' when comparing two data sets.
#'
#' @return List. The \code{f_list_1} is returned as inserted if there all check are carried out, otherwise
#' \code{f_list_2} is used.
#'
#' @keywords internal
#'
#' @export
#'
#' @examples
#'
#'
#' # Load example data
#' data("Boston", package = "MASS")
#' # Split data into test- and training data
#' x_train <- data.table::as.data.table(head(Boston))
#' x_train[,rad:=as.factor(rad)]
#' data_features <- get_data_specs(x_train)
#' model <- lm(medv ~ lstat + rm + rad + indus, data = x_train)
#'
#' model_features <- get_model_specs(model)
#' check_features(model_features,data_features)
check_features <- function(f_list_1,f_list_2,
                           use_1_as_truth=T){

  name_1 <- f_list_1$specs_type
  name_2 <- f_list_2$specs_type

  if(name_1 == name_2){
    name_1 <- paste0("shapr-",name_1)
    name_2 <- paste0("test-",name_2)
  }

  #### Checking labels ####
  if (is.null(f_list_1$labels)) {
    stop(paste0(name_1," must have column names."))
  }
  if (is.null(f_list_2$labels)) {
    stop(paste0(name_2," must have column names."))
  }

  # feature names must be unique
  if (any(duplicated(f_list_1$labels))) {
    stop(paste0(name_1," must have unique column names."))
  }

  # feature names must be unique
  if (any(duplicated(f_list_2$labels))) {
    stop(paste0(name_2," must have unique column names."))
  }


  missing_1_in_2 <- f_list_1$labels[!(f_list_1$labels %in% f_list_2$labels)]
  missing_2_in_1 <- f_list_2$labels[!(f_list_2$labels %in% f_list_1$labels)]

  if (length(missing_1_in_2)>0) {
    stop(paste0("Feature(s) ",paste0(missing_1_in_2,collapse=", ")," in ",name_1," is not in ",name_2,"."))
  }

  # Also check also that the features in 2 are in 1
  if(!use_1_as_truth){
    if (length(missing_2_in_1)>0) {
      stop(paste0("Feature(s) ",paste0(missing_2_in_1,collapse=", ")," in ",name_2," is not in ",name_1,"."))
    }
  }


  #### Reorder f_List_2 to match f_list_1, also removing anything in the former which is not in the latter ####
  f_list_2_reordering = match(f_list_1$labels,f_list_2$labels)

  f_list_2$labels <- f_list_2$labels[f_list_2_reordering]
  f_list_2$classes <- f_list_2$classes[f_list_2_reordering]
  f_list_2$factor_levels <- f_list_2$factor_levels[f_list_2_reordering]

  # Sorts the factor levels for easier comparison below
  f_list_1$sorted_factor_levels <- lapply(f_list_1$factor_levels,FUN=sort)
  f_list_2$sorted_factor_levels <- lapply(f_list_2$factor_levels,FUN=sort)

  # Check if any features have empty names i.e ""
  if (any(f_list_1$labels == "")) {
    stop("One or more features is missing a name.")
  }


  #### Checking classes ####
  if(any(is.na(f_list_1$classes))){
    message(paste0("The specified ",name_1," does not provide (all) feature classes. ",
                   "Feature class and any factor level checking is disabled, and those specifed in the ",
                   name_2," are passed on from here."))
    ret <- ref <- f_list_2

  } else {
    ret <- f_list_1
    ref <- f_list_2

  }

  # Check if f_list_1 and f_list_2 have features with the same class
  if (!identical(ret$classes,  ref$classes)) {
    stop(paste0("The features in ",name_1," and ",name_2," must have the same classes."))
  }

  # Check if the features all have class "integer", "numeric" or "factor
  if (!all(ret$classes %in% c("integer", "numeric", "factor"))) {
    invalid_class <- which(!(ret$classes %in% c("integer", "numeric", "factor")))
    stop(paste0("Feature(s) ",paste0(ret$labels[invalid_class],collapse=", ")," in ",name_1," and ",name_2,
                " is not of class integer, numeric or factor."))
  }

  # Checking factor levels #
  if (!identical(ret$sorted_factor_levels, ref$sorted_factor_levels)) {
    stop(paste0("Some levels for factor features are not present in both ",name_1," and ",name_2,"."))
  }


  ret$sorted_factor_levels <- NULL # Not needed

  return(ret) #

}

#' Updates data by reference according to the updater argument.
#'
#' @description \code{data} is updated, i.e. unused columns and factor levels are removed as described in
#' \code{updater}. This is done by reference, i.e. updates the object being passed to data even if nothing is
#' returned by the function itself.
#'
#' @param data data.table. Data that ought to be updated.
#' @param updater List. The object should be the output from
#' \code{\link[shapr:check_features]{check_features()}}.
#'
#'
#' @return NULL.
#' @keywords internal
update_data = function(data,updater){
  # Operates on data by reference, so no copying of data here

  new_labels <- updater$labels
  factor_features <- which(updater$classes=="factor")
  factor_levels <- updater$factor_levels

  # Reorder and delete unused columns
  cnms_remove <- setdiff(colnames(data), new_labels)
  if (length(cnms_remove) > 0) {
    message(paste0("The columns(s) ",paste0(cnms_remove,collapse=", ")," is not used by the model and thus removed ",
                   "from the data."))

    data[, (cnms_remove) := NULL]
  }
  data.table::setcolorder(data, new_labels)

  # Reorderes the factor levels
  org_factor_levels <- lapply(data,levels)
  identical_levels <- mapply(FUN = "identical",org_factor_levels,factor_levels)
  if(any(!identical_levels)){
    changed_levels <- which(!identical_levels)
    message(paste0("Levels are reordered for the factor feature(s) ",
                   paste0(new_labels[changed_levels],collapse=", "),"."))

    for (i in changed_levels) {
      data.table::set(data,
                      j=i,
                      value = factor(unlist(data[,new_labels[i],with=F],use.names = F), levels = factor_levels[[i]]))
    }
  }

  return(NULL)
}
