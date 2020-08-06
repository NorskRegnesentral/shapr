
## THIS IS BASED ENTIRELY ON THE CARET PACKAGE - JUST SOME NAME CHANGES AND REMOVAL OF SOME FUNCTIONALITY
## SEE HERE: https://github.com/topepo/caret/blob/master/pkg/caret/R/dummyVar.R

make_dummies <-
  function(formula, ...){
    UseMethod("make_dummies")
  }

make_dummies.default <- function (data, ...) { # formula, #  fullRank = FALSE
  # formula <- as.formula(formula)
  if(!is.data.frame(data)) {
    data <- as.data.frame(data, stringsAsFactors = FALSE)
  }


  # features <- all.vars(formula)
  features <- colnames(data)
  if(length(unique(features)) < length(features)){
    stop("Features must have unique names.")
  }

  if(any(features == ".")) {
    # features <- features[features != "."]
    # features <- unique(c(features, colnames(data)))
  }
  p <- sapply(data[, features, drop = FALSE], is.factor)
  p_sum <- sum(p)

  if(p_sum > 0) {
    charac_variables <- features[p]

    charac_list <- list()
    for(i in charac_variables){
      charac_list[[i]] <- levels(data[, i])
    }

  } else {
    charac_variables <- NULL
    charac_list <- NULL
  }
  contrasts_list <- list()
  for(i in charac_variables){
    contrasts_list[[i]] <- contrasts(data[,i], contrasts = FALSE)
  }

  # trms <- attr(model.frame(formula, data), "terms")
  r <- list(#call = match.call(),
            # form = formula,
            features = features,
            charac_variables = charac_variables,
            charac_list = charac_list,
            contrasts_list = contrasts_list)
            # terms = trms)
  class(r) <- "make_dummies"
  return(r)

}

predict.make_dummies <- function(object, newdata, na.action = na.pass, ...) {

  if(is.null(newdata)) {
    stop("newdata must be supplied")
  }
  # if(!is.data.frame(newdata)) {
  #   newdata <- as.data.frame(newdata, stringsAsFactors = FALSE)
  # }
  newdata <- data.table::as.data.table(as.data.frame(newdata, stringsAsFactors = FALSE))

  if(!all(object$charac_variables %in% names(newdata))) {
    stop(paste("Variable(s)", paste0("'", object$charac_variables[!object$charac_variables %in% names(newdata)], "'", collapse = ", "), "are not in newdata"))
  }
  vars <- object$features
  newdata0 <- newdata[, ..vars]

  m <- model.frame(data = newdata0, # #formula = Terms,
                   na.action = na.action,
                   xlev = object$charac_list)

  x <- model.matrix(object = ~. + 0, data = m, contrasts.arg = object$contrasts_list) # Terms, m

  all_column_names <- NULL
  for(i in object$features){
    if (is.factor(newdata0[[i]])) {
      all_column_names <- c(all_column_names, paste(colnames(newdata0[, ..i]), levels(newdata0[[i]]), sep = "."))
    } else{
      all_column_names <- c(all_column_names, colnames(newdata0[, ..i]))
    }
  }
  colnames(x) <- all_column_names
  #x[, colnames(x) != "(Intercept)", drop = FALSE]
  return(x)
}


## Not used anymore:
make_dummy_var <- function(formula, data) {
  p <- sapply(data, is.factor)
  p_sum <- sum(p)
  print(paste0("The number of factor variables: ", p, " out of ", ncol(data), "." ))

  col_names <- NULL
  data0 <- NULL
  for(i in 1:ncol(data)) {
    if(is.factor(data[,i])) {
      levels <- levels(data[,i])
      n_levels <- length(levels)

      skap <- matrix(NA, nrow = nrow(data), ncol = n_levels - 1)

      for(j in 1:(n_levels - 1)) {
        skap[, j] = ifelse(data[,i] == levels[j + 1], 1, 0)
      }
      data0 <- cbind(data0, skap)
      col_names <- c(col_names, paste0(names(data)[i], levels[-1]))

    } else {
      data0 <- cbind(data0, data[,i])
      col_names <- c(col_names, names(data)[i])
    }

  }
  colnames(data0) <- col_names

  return(data0)
}

