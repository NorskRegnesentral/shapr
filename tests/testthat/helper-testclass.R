# Create custom function of model_type for caret
model_type.testclass <- function(x) {
  "regression"
}

# Create custom function of predict_model for caret
predict_model.testclass <- function(x, newdata) {
  if (!any(colnames(newdata)=="lstat")){
    stop("lstat not in newdata")
  }
  as.double(newdata[,which(colnames(newdata)=="lstat")])
}
