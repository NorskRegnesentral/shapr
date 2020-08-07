make_dummies <- function (data, ...) {

  data <- data.table::as.data.table(as.data.frame(data, stringsAsFactors = FALSE))

  features <- colnames(data)
  if(length(unique(features)) < length(features)){
    stop("Features must have unique names.")
  }
  p <- sapply(data[, ..features], is.factor)
  p_sum <- sum(p)

  if(p_sum > 0) {
    factor_features <- features[p]
    factor_list <- lapply(data[, ..factor_features], levels)

  } else {
    factor_features <- NULL
    factor_list <- NULL
  }
  contrasts_list <- lapply(data[, ..factor_features], contrasts, contrasts = FALSE)


  r <- list(features = features,
            factor_features = factor_features,
            factor_list = factor_list,
            contrasts_list = contrasts_list)
  return(r)

}

apply_dummies <- function(obj, newdata, ...) {

  if(is.null(newdata)) {
    stop("newdata needs to be included.")
  }
  newdata <- data.table::as.data.table(as.data.frame(newdata, stringsAsFactors = FALSE))

  if(!all(obj$charac_variables %in% names(newdata))) {
    stop("Some features missing from newdata.")
  }
  features <- obj$features
  newdata_sub <- newdata[, ..features]

  m <- model.frame(data = newdata_sub,
                   na.action = na.pass,
                   xlev = obj$charac_list)

  x <- model.matrix(object = ~. + 0,
                    data = m,
                    contrasts.arg = obj$contrasts_list)
  return(x)
}
