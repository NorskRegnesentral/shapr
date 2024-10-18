#' @export
print.shapr <- function(x, digits = 4, ...) {
  shap <- copy(x$shapley_values)
  shap_names <- x$internal$parameters$shap_names
  cols <- c("none",shap_names)
  shap[,(cols):=lapply(.SD,round,digits=digits+2),.SDcols=cols]
  print(shap, digits = digits)
}
