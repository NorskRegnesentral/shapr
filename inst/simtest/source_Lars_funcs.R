


#' Generate All Formulas
#'
#' @description An auxiliary function that creates the formulas for all simulation studies
#'
#' @details In this function we specify all the different scenarios/set ups we want to consider.
#' That is, we start with linear model, then we add some interaction, then we progress to gam, then
#' gam with some interactions, and then many interactions.
#'
#' @return A list of lists. Each element in the list is the returned object from the function
#' "additive_formula_generator" for the different setups/formulas.
#' @export
#'
#' @examples
generate_all_formulas = function(feature_names,
                                 response_name,
                                 non_linear_function,
                                 predictive_function_type_default = "lm",
                                 predictive_function_type_non_linear_or_interactions = "gam") {
  # List to return all the formulas
  return_list = list()

  # get number of features
  n_features = length(feature_names)

  ### Linear models
  # No interactions
  formula_lm_no_interactions =
    additive_formula_generator(feature_names = feature_names,
                               response_name = response_name,
                               name = "lm_no_interactions",
                               predictive_function_type_default = predictive_function_type_default,
                               predictive_function_type_non_linear_or_interactions = predictive_function_type_non_linear_or_interactions)
  return_list[[formula_lm_no_interactions$name]] = formula_lm_no_interactions

  # Some interaction
  if (n_features >= 2) {
    formula_lm_some_interactions = additive_formula_generator(
      feature_names = feature_names,
      response_name = response_name,
      linear_interaction_features = list(c(1,2)),
      name = "lm_some_interactions",
      predictive_function_type_default = predictive_function_type_default,
      predictive_function_type_non_linear_or_interactions = predictive_function_type_non_linear_or_interactions)
    return_list[[formula_lm_some_interactions$name]] = formula_lm_some_interactions
  }

  # More interactions
  if (n_features >= 4) {
    formula_lm_more_interactions = additive_formula_generator(
      feature_names = feature_names,
      response_name = response_name,
      linear_interaction_features = list(c(1,2), c(3, 4)),
      name = "lm_more_interactions",
      predictive_function_type_default = predictive_function_type_default,
      predictive_function_type_non_linear_or_interactions = predictive_function_type_non_linear_or_interactions)
    return_list[[formula_lm_more_interactions$name]] = formula_lm_more_interactions
  }


  # many interactions
  if (n_features >= 6) {
    formula_lm_many_interactions = additive_formula_generator(
      feature_names = feature_names,
      response_name = response_name,
      linear_interaction_features = list(c(1,2), c(3, 4), c(5, 6)),
      name = "lm_many_interactions",
      predictive_function_type_default = predictive_function_type_default,
      predictive_function_type_non_linear_or_interactions = predictive_function_type_non_linear_or_interactions)
    return_list[[formula_lm_many_interactions$name]] = formula_lm_many_interactions
  }

  # numerous interactions
  if (n_features >= 8) {
    formula_lm_numerous_interactions = additive_formula_generator(
      feature_names = feature_names,
      response_name = response_name,
      linear_interaction_features = list(c(1,2), c(3, 4), c(5, 6), c(7, 8)),
      name = "lm_numerous_interactions",
      predictive_function_type_default = predictive_function_type_default,
      predictive_function_type_non_linear_or_interactions = predictive_function_type_non_linear_or_interactions)
    return_list[[formula_lm_numerous_interactions$name]] = formula_lm_numerous_interactions
  }

  ### Move from lm to gam without interactions
  formula_lm_to_gam_one = additive_formula_generator(
    feature_names = feature_names,
    response_name = response_name,
    non_linear_function = non_linear_function,
    non_linear_feature_indices = c(1),
    name = "lm_to_gam_one",
    predictive_function_type_default = predictive_function_type_default,
    predictive_function_type_non_linear_or_interactions = predictive_function_type_non_linear_or_interactions
  )
  return_list[[formula_lm_to_gam_one$name]] = formula_lm_to_gam_one

  if (n_features >= 2) {
    formula_lm_to_gam_two = additive_formula_generator(
      feature_names = feature_names,
      response_name = response_name,
      non_linear_function = non_linear_function,
      non_linear_feature_indices = c(1, 2),
      name = "lm_to_gam_two",
      predictive_function_type_default = predictive_function_type_default,
      predictive_function_type_non_linear_or_interactions = predictive_function_type_non_linear_or_interactions)
    return_list[[formula_lm_to_gam_two$name]] = formula_lm_to_gam_two
  }

  if (n_features >= 3) {
    formula_lm_to_gam_three = additive_formula_generator(
      feature_names = feature_names,
      response_name = response_name,
      non_linear_function = non_linear_function,
      non_linear_feature_indices = c(1, 2, 3),
      name = "lm_to_gam_three",
      predictive_function_type_default = predictive_function_type_default,
      predictive_function_type_non_linear_or_interactions = predictive_function_type_non_linear_or_interactions)
    return_list[[formula_lm_to_gam_three$name]] = formula_lm_to_gam_three
  }

  if (n_features >= 5) {
    formula_lm_to_gam_five = additive_formula_generator(
      feature_names = feature_names,
      response_name = response_name,
      non_linear_function = non_linear_function,
      non_linear_feature_indices = c(1, 2, 3, 4, 5),
      name = "lm_to_gam_five",
      predictive_function_type_default = predictive_function_type_default,
      predictive_function_type_non_linear_or_interactions = predictive_function_type_non_linear_or_interactions)
    return_list[[formula_lm_to_gam_five$name]] = formula_lm_to_gam_five
  }

  ### Generalized additive model
  # No interactions
  formula_gam_no_interactions = additive_formula_generator(
    feature_names = feature_names,
    response_name = response_name,
    non_linear_function = non_linear_function,
    non_linear_feature_indices = seq(length(feature_names)),
    name = "gam_no_interactions",
    predictive_function_type_default = predictive_function_type_default,
    predictive_function_type_non_linear_or_interactions = predictive_function_type_non_linear_or_interactions)
  return_list[[formula_gam_no_interactions$name]] = formula_gam_no_interactions

  # Some interaction
  if (n_features >= 2) {
    formula_gam_some_interactions = additive_formula_generator(
      feature_names = feature_names,
      response_name = response_name,
      non_linear_interaction_features = list(c(1,2)),
      non_linear_function = non_linear_function,
      non_linear_feature_indices = seq(length(feature_names)),
      name = "gam_some_interactions",
      predictive_function_type_default = predictive_function_type_default,
      predictive_function_type_non_linear_or_interactions = predictive_function_type_non_linear_or_interactions)
    return_list[[formula_gam_some_interactions$name]] = formula_gam_some_interactions
  }

  # More interactions
  if (n_features >= 4) {
    formula_gam_more_interactions = additive_formula_generator(
      feature_names = feature_names,
      response_name = response_name,
      non_linear_interaction_features = list(c(1,2), c(3, 4)),
      non_linear_function = non_linear_function,
      non_linear_feature_indices = seq(length(feature_names)),
      name = "gam_more_interactions",
      predictive_function_type_default = predictive_function_type_default,
      predictive_function_type_non_linear_or_interactions = predictive_function_type_non_linear_or_interactions)
    return_list[[formula_gam_more_interactions$name]] = formula_gam_more_interactions
  }

  # many interactions
  if (n_features >= 6) {
    formula_gam_many_interactions = additive_formula_generator(
      feature_names = feature_names,
      response_name = response_name,
      non_linear_interaction_features = list(c(1,2), c(3, 4), c(5, 6)),
      non_linear_function = non_linear_function,
      non_linear_feature_indices = seq(length(feature_names)),
      name = "gam_many_interactions",
      predictive_function_type_default = predictive_function_type_default,
      predictive_function_type_non_linear_or_interactions = predictive_function_type_non_linear_or_interactions)
    return_list[[formula_gam_many_interactions$name]] = formula_gam_many_interactions
  }

  # nomerous interactions
  if (n_features >= 8) {
    formula_gam_numerous_interactions = additive_formula_generator(
      feature_names = feature_names,
      response_name = response_name,
      non_linear_interaction_features = list(c(1,2), c(3, 4), c(5, 6), c(7, 8)),
      non_linear_function = non_linear_function,
      non_linear_feature_indices = seq(length(feature_names)),
      name = "gam_numerous_interactions",
      predictive_function_type_default = predictive_function_type_default,
      predictive_function_type_non_linear_or_interactions = predictive_function_type_non_linear_or_interactions)
    return_list[[formula_gam_numerous_interactions$name]] = formula_gam_numerous_interactions
  }

  ### Return the formula objects
  return(return_list)
}



#' Additive formula generator
#'
#' @description Function to create formula used to generate response.
#'
#' @details This is a function which lets the user specify the which features are linear, which are sent through a non-linear function,
#' and which features to have interactions between them. Returns the formula as different string objects and formula object.
#'
#' @param feature_names Vector containing the names of the features to be included in the lm formula.
#' @param linear_interaction_features Specifying the index of the features we also want to include in the linear interactions terms.
#' Can be a vector, then we take all pairwise linear interactions, or it can be a list of vectors of specified indices of features
#' we are to take the interaction between.
#' @param non_linear_feature_indices The index of the features we are going to apply the 'non_linear_function' function on.
#' @param non_linear_function A function which we are applying on the 'non_linear_feature_indices'.
#' @param non_linear_interaction_features Specifying the index of the features we also want to include in the non-linear interactions terms.
#' Can be a vector, then we take all pairwise non-linear interactions, or it can be a list of vectors of specified indices of features
#' we are to take the interaction between.
#' @param non_linear_interaction_function A function returning a string specifying the non-linear interaction between pairs of
#' features. The default returns "a\*b + a\*b^2 + b\*a^2", where 'a' and 'b' are feature names.
#' @param response_name String containing the name of the response value such that we can add "response_name ~" to the return string.
#' @param remove_duplicates With the default versions of linear and non_linear interaction terms, there is an overlap as a:b and a*b
#' are identical and present in both of them. We therefore remove a:b from the returned string if a*b is present.
#' @param name String. If we want to include a name entry in the returned list.
#'
#' @return Return a list of strings/formulas which are to be used as the formula in the lm function call or to generate the design matrix
#' through the "model.matrix(formula, data = as.data.frame(data))" function.
#'
additive_formula_generator = function(feature_names,
                                      linear_interaction_features = NULL,
                                      non_linear_feature_indices = NULL,
                                      non_linear_function = "cos",
                                      non_linear_interaction_features = NULL,
                                      non_linear_interaction_function = internal_non_linear_interaction_function,
                                      response_name = NULL,
                                      remove_interaction_duplicates = TRUE,
                                      name = NULL,
                                      predictive_function_type_default = "lm",
                                      predictive_function_type_non_linear_or_interactions = "gam"){

  ### Some checks ###
  # Get the number of features
  p = length(feature_names)

  # Variable to store the number of linear and non-linear interactions terms. Will be updated later.
  n_linear_interactions = 0
  n_non_linear_interactions = 0

  #
  if (!is.null(non_linear_feature_indices) || !is.null(non_linear_interaction_features)) {
    predictive_function_type = predictive_function_type_non_linear_or_interactions
  } else {
    predictive_function_type = predictive_function_type_default
  }

  if (any(unlist(linear_interaction_features) > p)) {
    stop(sprintf("The argument 'linear_interaction_features' references feature index %d, but 'feature_names' contains only %d features.",
                 max(unlist(linear_interaction_features)), p))
  }
  if (any(unlist(non_linear_feature_indices) > p)) {
    stop(sprintf("The argument 'non_linear_feature_indices' references feature index %d, but 'feature_names' contains only %d features.",
                 max(unlist(non_linear_feature_indices)), p))
  }
  if (!is.null(non_linear_function) && !exists(non_linear_function)) {
    stop(sprintf("The function '%s' does not exists in the environment. Provide a well defined function for the 'non_linear_function' argument.",
                 non_linear_function))
  }
  if (any(unlist(non_linear_interaction_features) > p)) {
    stop(sprintf("The argument 'non_linear_interaction_features' references feature index %d, but 'feature_names' contains only %d features.",
                 max(unlist(non_linear_interaction_features)), p))
  }
  if (!is.null(response_name) && response_name == "") {
    stop(sprintf("The string 'response_name' is an empty string."))
  }
  if (!is.null(non_linear_interaction_function) && !is.function(non_linear_interaction_function)) {
    stop(sprintf("The argument 'non_linear_interaction_function' is not a function, but of class: %s.", class(non_linear_interaction_function)))
  }

  # List to store formulas, strings and variables to return.
  return_list = list()

  ### First order terms ###
  # Check if any of the terms are to be non-linear
  if (!is.null(non_linear_feature_indices)) {
    # Sort the order and remove duplicated features
    non_linear_feature_indices = sort(unique(non_linear_feature_indices))

    # Get the features which are to be linear and those who are to be non-linear.
    feature_names_linear    = feature_names[-non_linear_feature_indices]
    feature_names_nonlinear = feature_names[non_linear_feature_indices]

    # If all the features are sent through the non-linear, we need to create the string a little bit different.
    if (length(feature_names_linear) == 0) {
      # Create the formula with only the non-linear terms
      formula_str = paste(non_linear_function, "(", feature_names_nonlinear, ")", sep = "", collapse = " + ")
    } else {
      # Create the formula with the non-linear terms and linear terms.
      formula_str = paste(paste(non_linear_function, "(", feature_names_nonlinear, ")", sep = "", collapse = " + "), paste(feature_names_linear, sep = "", collapse = " + "), sep = " + ")
    }
  } else {
    # Create the first order linear additive terms.
    formula_str = paste(feature_names, sep = "", collapse = " + ")
  }

  # Get the string version for the gam model
  formula_as_string_gam = stringr::str_replace_all(formula_str, non_linear_function, "s")
  formula_as_string_gam2 = formula_as_string_gam

  ### Second order terms ###
  # Non-linear interactions #
  # Check if we are to add interactions or if we are done making the formula
  if (!is.null(non_linear_interaction_features)) {
    # Add interaction terms to the formula

    # Need to change the smooths in the gam formula to the ti type
    formula_as_string_gam = stringr::str_replace_all(formula_as_string_gam, "s", "ti")

    # Remove any duplicated elements
    non_linear_interaction_features = unique(non_linear_interaction_features)

    # If non_linear_interaction_features is not a list, we assume it is array and that
    # we are to take the pairwise non-linear interactions between all the specified features
    if (!is.list(non_linear_interaction_features)) {
      # Sort them to get the interaction terms in the same order as the first order features.
      non_linear_interaction_features = sort(non_linear_interaction_features)

      # Check that we are given at least two features to make non_linear interaction terms
      if (length(non_linear_interaction_features) < 2) {
        stop(sprintf("Need at least two features to create interactions term. 'non_linear_interaction_features' contains %d features.",
                     length(non_linear_interaction_features)))
      }

      # Make a list to store the non_linear interaction indices
      non_linear_interaction_feature_list = list()

      # Iterate over the unique combinations of features and add them to the list
      for (i in seq(length(non_linear_interaction_features)-1)){
        for (j in seq(i, length(non_linear_interaction_features)-1)) {
          non_linear_interaction_feature_list = c(non_linear_interaction_feature_list, list(non_linear_interaction_features[c(i,j+1)]))
        }
      }
    } else {
      # non_linear_interaction_features is already a list and we can directly use it.
      non_linear_interaction_feature_list = non_linear_interaction_features
    }

    # Update the number of non-linear interaction terms
    n_non_linear_interactions = length(non_linear_interaction_feature_list)

    # Add the non_linear interaction terms to the formula string
    for (non_linear_interactions in non_linear_interaction_feature_list) {
      formula_str = paste(formula_str, internal_non_linear_interaction_function(feature_names[non_linear_interactions]), sep = " + ")
      formula_as_string_gam = paste(formula_as_string_gam, paste("ti(", paste(feature_names[non_linear_interactions], collapse = ", "), ")", sep = ""), sep = " + ")
      formula_as_string_gam2 = paste(formula_as_string_gam2, paste("ti(", paste(feature_names[non_linear_interactions], collapse = ", "), ")", sep = ""), sep = " + ")
    }
  }

  # Add the number of non-linear interaction terms to the return list
  return_list = c(return_list, list(n_non_linear_interactions = n_non_linear_interactions))

  # Linear interaction #
  # Check if we are to add linear interactions or if we are done making the formula
  if (!is.null(linear_interaction_features)) {
    # Add interaction terms to the formula

    # Remove any duplicated elements
    linear_interaction_features = unique(linear_interaction_features)

    # If linear_interaction_features is not a list, we assume it is array and that
    # we are to take the pairwise interactions between all the specified features
    if (!is.list(linear_interaction_features)) {
      # Sort them to get the interaction terms in the same order as the first order features.
      linear_interaction_features = sort(linear_interaction_features)

      # Check that we are given at least two features to make interaction terms
      if (length(linear_interaction_features) < 2) {
        stop(sprintf("Need at least two features to create interactions term. 'linear_interaction_features' contains %d features.",
                     length(linear_interaction_features)))
      }

      # Make a list to store the interaction indices
      linear_interaction_feature_list = list()

      # Iterate over the unique combinations of features and add them to the list
      for (i in seq(length(linear_interaction_features)-1)){
        for (j in seq(i, length(linear_interaction_features)-1)) {
          linear_interaction_feature_list = c(linear_interaction_feature_list, list(linear_interaction_features[c(i,j+1)]))
        }
      }
    } else {
      # linear_interaction_features is already a list and we can directly use it.
      linear_interaction_feature_list = linear_interaction_features
    }

    # If we are to remove terms that are identical.
    # NOTE that this only works for the default versions of this function.
    # I.e., the linear interactions a:b is included in the non-linear
    # interaction a*b + a*b^2 + b*a^2, where a:b == a*b is the same.
    # So we end up with two columns with co-linearity.
    if (remove_interaction_duplicates && !is.null(non_linear_interaction_features)) {
      linear_interaction_feature_list = setdiff(linear_interaction_feature_list, non_linear_interaction_feature_list)
    }

    # Update the number of linear interaction terms
    n_linear_interactions = length(linear_interaction_feature_list)

    # Check that linear_interaction_feature_list is not an empty list, which occurs if
    # linear_interaction_feature_list is a subset of non_linear_interaction_feature_list.
    # In that case, all the linear interaction terms are already included.
    if (n_linear_interactions  >= 1) {
      # Add the linear interaction terms to the formula str
      for (linear_interactions in linear_interaction_feature_list) {
        formula_str = paste(formula_str, paste(feature_names[linear_interactions], collapse = ":"), sep = " + ")
      }
    }
  }

  # Add the number of linear interactions to the return list.
  return_list = c(return_list, list(n_linear_interactions = n_linear_interactions))

  ### Response feature ###
  # Remove leading and tailing white spaces
  formula_as_string = trimws(formula_str)
  formula_as_string_gam = trimws(formula_as_string_gam)
  formula_as_string_gam2 = trimws(formula_as_string_gam2)

  # Create return list
  return_list = c(return_list,
                  list(formula_features_as_string = formula_as_string,
                       formula_features_as_formula = formula(paste("~", formula_as_string)),
                       formula_features_as_string_gam = formula_as_string_gam,
                       formula_features_as_formula_gam = formula(paste("~", formula_as_string_gam)),
                       formula_features_as_string_gam2 = formula_as_string_gam2,
                       formula_features_as_formula_gam2 = formula(paste("~", formula_as_string_gam2))))

  # If the response feature is provided, then we add that to the formula too
  if (!is.null(response_name)) {
    formula_as_string = paste(response_name, "~", formula_as_string)
    formula_as_string_gam = paste(response_name, "~", formula_as_string_gam)
    formula_as_string_gam2 = paste(response_name, "~", formula_as_string_gam2)
  } else {
    # Add tilde to make a formula
    formula_as_string = paste("~", formula_as_string)
    formula_as_string_gam = paste("~", formula_as_string_gam)
    formula_as_string_gam2 = paste("~", formula_as_string_gam2)
  }

  # Remove leading and tailing white spaces (needed if user input response_name = "").
  # Maybe better to give a warning/error in the start...
  formula_as_string = trimws(formula_as_string)
  formula_as_string_gam = trimws(formula_as_string_gam)
  formula_as_string_gam2 = trimws(formula_as_string_gam2)

  # Add to return list
  return_list = c(return_list, list(formula_as_string = formula_as_string,
                                    formula_as_string_gam = formula_as_string_gam,
                                    formula_as_string_gam2 = formula_as_string_gam2))

  # Convert to formula
  formula_as_formula = formula(formula_as_string)
  formula_as_formula_gam = formula(formula_as_string_gam)
  formula_as_formula_gam2 = formula(formula_as_string_gam2)

  # Add to return list, and the suitable predictive function type
  return_list = c(return_list, list(formula_as_formula = formula_as_formula,
                                    formula_as_formula_gam = formula_as_formula_gam,
                                    formula_as_formula_gam2 = formula_as_formula_gam2,
                                    predictive_function_type = predictive_function_type))

  # Add name to return list if it is provided
  if (!is.null(name)) return_list = c(list(name = name), return_list)

  # Return the list of the formula and string
  return(return_list)
}
# Examples
# additive_formula_generator(feature_names = c("a", "b", "c", "d"),
#                            linear_interaction_features = c(1, 3, 6)) # Error on purpose
# additive_formula_generator(feature_names = c("a", "b", "c", "d"),
#                            linear_interaction_features = c(2, 4, 1))
# additive_formula_generator(feature_names = c("a", "b", "c", "d"),
#                            linear_interaction_features = c(1, 2, 4))
# additive_formula_generator(feature_names = c("a", "b", "c", "d"),
#                            linear_interaction_features = c(1, 2, 4, 4))
# additive_formula_generator(feature_names = c("a", "b", "c", "d"),
#                            linear_interaction_features = list(c(1,2), c(3,4)))
# additive_formula_generator(feature_names = c("a", "b", "c", "d"),
#                            linear_interaction_features = list(c(1,2), c(3,4), c(3,4)))
# additive_formula_generator(feature_names = c("a", "b", "c", "d"),
#                            linear_interaction_features = list(c(1,2), c(3,4)),
#                            non_linear_feature_indices = c(2,4),
#                            non_linear_function = "sin",
#                            response_name = "e")
# additive_formula_generator(feature_names = c("a", "b", "c", "d"),
#                            linear_interaction_features = list(c(1,2), c(3,4)),
#                            non_linear_feature_indices = c(2,4),
#                            non_linear_function = "sin",
#                            response_name = "e",
#                            non_linear_interaction_features = c(1,2),
#                            non_linear_interaction_function = internal_non_linear_interaction_function,
#                            remove_interaction_duplicates = TRUE)
# additive_formula_generator(feature_names = c("a", "b", "c", "d"),
#                            linear_interaction_features = list(c(1,2), c(3,4)),
#                            non_linear_feature_indices = c(2,4),
#                            non_linear_function = "sin",
#                            response_name = "e",
#                            non_linear_interaction_features = c(1,3),
#                            non_linear_interaction_function = internal_non_linear_interaction_function,
#                            remove_interaction_duplicates = TRUE)
# additive_formula_generator(feature_names = c("a", "b", "c", "d"),
#                            linear_interaction_features = list(c(1,2), c(3,4)),
#                            non_linear_feature_indices = c(2,4),
#                            non_linear_function = "sin",
#                            response_name = "e",
#                            non_linear_interaction_features = list(c(1,2), c(2,4)),
#                            non_linear_interaction_function = internal_non_linear_interaction_function,
#                            remove_interaction_duplicates = TRUE)
# additive_formula_generator(feature_names = c("a", "b", "c", "d"),
#                            linear_interaction_features = list(c(1,2), c(3,4)),
#                            non_linear_feature_indices = c(2,4),
#                            non_linear_function = "sin",
#                            response_name = "e",
#                            non_linear_interaction_features = seq(length(feature_names)),
#                            non_linear_interaction_function = internal_non_linear_interaction_function,
#                            remove_interaction_duplicates = TRUE)
# additive_formula_generator(feature_names = c("a", "b", "c", "d"),
#                            linear_interaction_features = NULL,
#                            non_linear_feature_indices = seq(length(feature_names)),
#                            non_linear_function = "sin",
#                            response_name = NULL,
#                            non_linear_interaction_features = lapply(seq(length(feature_names)/2), function(j) c(2*j-1, 2*j)),
#                            non_linear_interaction_function = internal_non_linear_interaction_function,
#                            remove_interaction_duplicates = TRUE)

# Generate Formulas ---------------------------------------------------------------
#' Non-linear interactions
#'
#' @description Creates non-linear interaction terms
#'
#' @details This function specify how a pair of features should interact with each other by defining the corresponding
#' non-linear interactions. In this function we specify the interactions to be a*b + a\*b^2 + b\*a^2.
#'
#' @param two_feature_names Vector of two strings of the feature names active in the non-linear interaction function.
#'
#' @return String with the formula for the non-linear interaction function
#' @export
#'
#' @examples Function call non_linear_interaction_function(c(a, b)) returns: I(a*b) + I(a\*b^2) + I(b\*a^2).
#'
internal_non_linear_interaction_function = function(two_feature_names) {
  a = two_feature_names[1]
  b = two_feature_names[2]
  paste("I(", a, "*", b, ") + I(", a, "*", b, "^2", ") + I(", b, "*", a, "^2)", sep = "")
}

#' Paper 2: Simulated Training and Test Data
#'
#' @description Function that calls "create_data()" twice, once for the training data and once for the test data.
#'  The generate simulated data used in the numerical simulation studies in paper 2.
#'
#' @details The function allows the user to specify how the data is to be simulated.
#'
#' @param n_train Integer specifying the number of training observations to generate.
#' @param n_test Integer specifying the number of test observations to generate.
#' @param p Integer specifying the number of features.
#' @param formula Formula specifying how to compute the response. Should be on the same form as the outputted by the 'additive_formula_generator' function.
#' More specifically, the '$formula_features_as_formula' of the returned object.
#' @param beta_vec Vector of numerics specifying the coefficients of the first order terms in the formula.
#' @param gamma_vec Vector of numerics specifying the coefficients of the first order terms in the formula.
#' @param distribution Which of the three distributions we are simulating the features from: multivariate Gaussian/normal (mvn),
#' generalized hyperbolic (gh), or Burr (burr). The latter is skewed, and heavy-tailed.
#' @param noise_sigma The standard deviation of the random, zero mean, normal noise added to the simulated response.
#' @param mean_vec Needs to be specified if distribution is "mvn". A vector containing the mean of the mvn data.
#' @param rho Needs to be specified if distribution is "mvn". The covariance used in the covariance matrix in the mvn.
#' @param rho_equicorrelation Needs to be specified if distribution is "mvn". Related to the entries of the covariance matrix in the mvn.
#' TRUE: all off-diagonal entries are 'rho', while the on diagonal entries are 1.
#' TRUE: the i,jth off-diagonal entry is rho^|i-j|, while the on diagonal entries are 1.
#' @param burr_p Needs to be specified if distribution is "burr".
#' @param burr_r_vec Needs to be specified if distribution is "burr".
#' @param burr_b_vec Needs to be specified if distribution is "burr".
#' @param seed Integer seed to be able to reproduce the same simulated training data several times.
#' @param seed Integer seed to be able to reproduce the same simulated test data several times.
#'
#' @return Returns the simulated data, both features, designnmatrix, response and additional parameters. For both training and test data.
#' @export
#'
#' @examples
create_train_and_test_data = function(n_train,
                                      n_test,
                                      p,
                                      formula = formula(paste("~", paste("Var", seq(p), sep = "", collapse = " + "))),
                                      beta_vec = rep(1, p),
                                      gamma_vec = rep(1, length(strsplit(toString(formula), "\\+")[[1]])-p),
                                      distribution = c("mvn", "gh", "burr"),
                                      noise_sigma = 1,
                                      mean_vec = rep(0, p),
                                      rho = 0.0,
                                      rho_equicorrelation = FALSE,
                                      burr_p = 1.5,
                                      burr_r_vec = rep(c(1,3,5), ceiling(p/3))[1:p],
                                      burr_b_vec = rep(c(2,4,6), ceiling(p/3))[1:p],
                                      seed_train = sample(10^5, 1),
                                      seed_test = sample(10^5, 1)) {

  # Check for valid distribution
  distribution = match.arg(distribution, distribution, several.ok = FALSE)

  # Generate the training data
  training_data = create_data(n = n_train,
                              p = p,
                              formula = formula,
                              beta_vec = beta_vec,
                              gamma_vec = gamma_vec,
                              distribution = distribution,
                              noise_sigma = noise_sigma,
                              mean_vec = mean_vec,
                              rho = rho,
                              rho_equicorrelation = rho_equicorrelation,
                              burr_p = burr_p,
                              burr_r_vec = burr_r_vec,
                              burr_b_vec = burr_b_vec,
                              seed = seed_train)

  # Generate the test data
  test_data = create_data(n = n_test,
                          p = p,
                          formula = formula,
                          beta_vec = beta_vec,
                          gamma_vec = gamma_vec,
                          distribution = distribution,
                          noise_sigma = noise_sigma,
                          mean_vec = mean_vec,
                          rho = rho,
                          rho_equicorrelation = rho_equicorrelation,
                          burr_p = burr_p,
                          burr_r_vec = burr_r_vec,
                          burr_b_vec = burr_b_vec,
                          seed = seed_test)

  if (distribution == "mvn") {
    common_indices = 1:8
    return_list_ordering = c(1, 19, 2, 20, 3, 21, 4, 22, 5, 23, 6, 24, 7, 25, 8, 26, 9:18)
  } else if (distribution == "burr") {
    common_indices = c(1:8, 18)
    return_list_ordering = c(1, 19, 2, 20, 18, 27, 3, 21, 4, 22, 5, 23, 6, 24, 7, 25, 8, 26, 9:17)
  }

  # Rename the entries which are different for the training and test data
  names(training_data)[common_indices] = paste("training_", names(training_data)[common_indices], sep = "")
  names(test_data)[common_indices] = paste("test_", names(test_data)[common_indices], sep = "")

  # Combine training and test data lists into one common list
  return_list = c(training_data, test_data)

  # Remove duplicated entries
  return_list = return_list[unique(names(return_list))]
  names(return_list)

  # Reorder the entries
  return_list = return_list[return_list_ordering]
  names(return_list)

  # Return the data
  return(return_list)
}

#' Paper 2: Simulated Data
#'
#' @description Function used to generate simulated data used in the numerical simulation studies
#' in paper 2.
#'
#' @details The function allows the user to specify how the data is to be simulated.
#'
#'
#'
#' @param n Integer specifying the number of observations to generate.
#' @param p Integer specifying the number of features.
#' @param formula Formula specifying how to compute the response. Should be on the same form as the outputted by the 'additive_formula_generator' function.
#' More specifically, the '$formula_features_as_formula' of the returned object.
#' @param beta_vec Vector of numerics specifying the coefficients of the first order terms in the formula.
#' @param gamma_vec Vector of numerics specifying the coefficients of the first order terms in the formula.
#' @param distribution Which of the three distributions we are simulating the features from: multivariate Gaussian/normal (mvn),
#' generalized hyperbolic (gh), or Burr (burr). The latter is skewed, and heavy-tailed.
#' @param noise_sigma The standard deviation of the random, zero mean, normal noise added to the simulated response.
#' @param mean_vec Needs to be specified if distribution is "mvn". A vector containing the mean of the mvn data.
#' @param rho Needs to be specified if distribution is "mvn". The covariance used in the covariance matrix in the mvn.
#' @param rho_equicorrelation Needs to be specified if distribution is "mvn". Related to the entries of the covariance matrix in the mvn.
#' TRUE: all off-diagonal entries are 'rho', while the on diagonal entries are 1.
#' TRUE: the i,jth off-diagonal entry is rho^|i-j|, while the on diagonal entries are 1.
#' @param burr_p Needs to be specified if distribution is "burr".
#' @param burr_r_vec Needs to be specified if distribution is "burr".
#' @param burr_b_vec Needs to be specified if distribution is "burr".
#' @param seed Integer seed to be able to reproduce the same simulated data several times.
#'
#' @return Returns the simulated data, both features, design matrix, response and additional parameters.
#' @export
#'
#' @examples
create_data = function(n,
                       p,
                       formula = formula(paste("~", paste("Var", seq(p), sep = "", collapse = " + "))),
                       beta_vec = rep(1, p+1),
                       gamma_vec = rep(1, length(strsplit(toString(formula), "\\+")[[1]])-p),
                       distribution = c("mvn", "gh", "burr"),
                       noise_sigma = 1,
                       mean_vec = rep(0, p),
                       rho = 0.0,
                       rho_equicorrelation = FALSE,
                       burr_p = 1.5,
                       burr_r_vec = rep(c(1,3,5), ceiling(p/3))[1:p],
                       burr_b_vec = rep(c(2,4,6), ceiling(p/3))[1:p],
                       seed = sample(10^5, 1)) {

  ### Some checks ###
  # Check that the provided distribution is valid.
  distribution = match.arg(distribution)

  # We subtract one due to the intercept
  print(formula)
  print(toString(formula))
  if (length(strsplit(toString(formula), "\\+")[[1]]) != (length(beta_vec) + length(gamma_vec) - 1)) {
    stop(sprintf("Mismatched number of terms (%d) and coefficients (%d).",
                 length(strsplit(toString(formula), "\\+")[[1]]),
                 length(beta_vec) + length(gamma_vec)
    )
    )
  }

  if (noise_sigma < 0) {
    stop(sprintf("The 'noise_sigma' (%g) is the standard deviation of a normal distribution and has to be positive.",
                 noise_sigma))
  }

  ### Simulate the data based on the given distribution ###
  if (tolower(distribution) == "mvn") {
    ### Working with the multivariate normal distribution

    # Check that mean_vec is of the right dimension
    if (length(mean_vec) != p) {
      if (length(mean_vec) == 1) mean_vec = rep(mean_vec, p)
      else stop(sprintf("Argument 'mean_vec' (%d) is not of length 1 and do not match the number of features 'p' (%d).", length(mean_vec), p))
    }

    # Check for valid correlation
    rho = rho[1] # Ensure that rho is a single value
    if (rho < -1 || 1 < rho) stop(sprintf("The correlation 'rho' (%g) must at least be between -1 and 1.", rho))

    # Covariance matrix with fixed or decreasing correlation
    sigma_mat = diag(p)
    for (i in seq(nrow(sigma_mat))) {
      for (j in seq(ncol(sigma_mat))) {
        if (i != j) {
          sigma_mat[i,j] = ifelse(rho_equicorrelation, rho, rho^abs(i-j))
        }
      }
    }

    # Set the seed if it is given to us
    if (!is.null(seed)) set.seed(seed)

    # Create the multivariate normal distributed feature values
    x = mvtnorm::rmvnorm(n, mean = mean_vec, sigma = sigma_mat)
    colnames(x) = paste("Var", seq(ncol(x)), sep = "")

    # List of parameters to return to the user
    return_list = list(mean_vec = mean_vec,
                       sigma_mat = sigma_mat,
                       rho = rho,
                       rho_equicorrelation = rho_equicorrelation)

  } else if (tolower(distribution) == "gh") {
    stop(sprintf("TODO: add support for data according to the Generalized Hyperbolic distribution."))

  } else if (tolower(distribution) == "burr") {
    # Check the parameters of the burr distribution
    if (burr_p <= 0) stop(sprintf("The 'burr_p' argument must be positive for the corresponding burr distribution to exists."))
    if (any(burr_r_vec <= 0)) stop(sprintf("The entries in 'burr_r_vec' argument must be positive for the corresponding burr distribution to exists."))
    if (any(burr_b_vec <= 0)) stop(sprintf("The entries in 'burr_b_vec' argument must be positive for the corresponding burr distribution to exists."))

    # Set the seed if it is given to us
    if (!is.null(seed)) set.seed(seed)

    # Create the random multivariate Burr data
    x = rmvburr(n = n, parm1 = burr_p, parm2 = burr_r_vec, parm3 = burr_b_vec)
    colnames(x) = paste("Var", seq(ncol(x)), sep = "")

    ### Create the response Y according to a similar function as (11) in Aas et al (2021).
    # Get the u_m's for the training at test set from the burr_pit function.
    x_post_processing = burr_pit(x, a = burr_p, d = burr_r_vec, c = burr_b_vec)
    colnames(x_post_processing) = paste("Var", seq(ncol(x_post_processing)), sep = "")

    # List of parameters to return to the user
    return_list = list(burr_p = burr_p,
                       burr_r_vec = burr_r_vec,
                       burr_b_vec = burr_b_vec,
                       x_post_processing = x_post_processing)

  } else {
    stop("We do not support the given distribution.")
  }

  # Create the normal distributed noise
  noise = rnorm(n, mean = 0, sd = noise_sigma)

  # Compute the model design matrix
  # x = as.data.frame(x)
  # model_design_matrix = model.matrix.default(formula, data = x)
  model_design_matrix = model.matrix(formula, data = as.data.frame(x))

  # Create the response without noise
  # y_without_noise = cbind(rep(1, n), as.matrix(x)) %*% beta_vec
  y_without_noise = model_design_matrix %*% c(beta_vec, gamma_vec)
  colnames(y_without_noise) = "y"

  # Add the noise to the response
  y = y_without_noise + noise

  # Create a data.frame version
  df = as.data.frame(cbind(y, x))

  # Return the data to the user
  return(c(list(x = x,
                model_design_matrix = model_design_matrix,
                y = y,
                df = df,
                y_without_noise = y_without_noise,
                noise = noise,
                seed = seed,
                n = n,
                p = p,
                distribution = distribution,
                noise_sigma = noise_sigma,
                beta_vec = beta_vec,
                gamma_vec = gamma_vec,
                formula = formula),
           return_list))
}

