#' @keywords internal
unique_features <- function(x) {
  unique(
    unlist(
      strsplit(x, split = ":", fixed = TRUE)
    )
  )
}

#' @keywords internal
get_native_methods <- function(){
  tmp <- ls(getNamespace("shapr"),all.names=TRUE)
  tmp <- tmp[grep("features\\.",tmp)]
  tmp <- substring(tmp,nchar("features.")+1)
  native_methods <- tmp[tmp != "default"]
}
