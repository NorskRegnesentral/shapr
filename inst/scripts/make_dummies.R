
make_dummies <-
  function(formula, ...){
    UseMethod("make_dummies")
  }

make_dummies.default <- function (data, ...) {
  if(!is.data.frame(data)) {
    data <- as.data.frame(data, stringsAsFactors = FALSE)
  }

  features <- colnames(data)
  if(length(unique(features)) < length(features)){
    stop("Features must have unique names.")
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

  r <- list(features = features,
            charac_variables = charac_variables,
            charac_list = charac_list,
            contrasts_list = contrasts_list)
  class(r) <- "make_dummies"
  return(r)

}

predict.make_dummies <- function(olddata, newdata, na.action = na.pass, ...) {

  if(is.null(newdata)) {
    stop("newdata needs to be included.")
  }
  newdata <- data.table::as.data.table(as.data.frame(newdata, stringsAsFactors = FALSE))

  if(!all(olddata$charac_variables %in% names(newdata))) {
    stop("Some features missing from newdata.")
  }
  vars <- olddata$features
  newdata0 <- newdata[, ..vars]

  m <- model.frame(data = newdata0,
                   na.action = na.action,
                   xlev = olddata$charac_list)

  x <- model.matrix(object = ~. + 0,
                    data = m,
                    contrasts.arg = olddata$contrasts_list)

  all_column_names <- NULL
  for(i in olddata$features){
    if (is.factor(newdata0[[i]])) {
      all_column_names <- c(all_column_names, paste(colnames(newdata0[, ..i]), levels(newdata0[[i]]), sep = "."))
    } else{
      all_column_names <- c(all_column_names, colnames(newdata0[, ..i]))
    }
  }
  colnames(x) <- all_column_names
  return(x)
}
