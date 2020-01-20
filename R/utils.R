#' @keywords internal
unique_features <- function(x) {
  unique(
    unlist(
      strsplit(x, split = ":", fixed = TRUE)
    )
  )
}
