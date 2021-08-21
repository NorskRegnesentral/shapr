#' @keywords internal
unique_features <- function(x) {
  unique(
    unlist(
      strsplit(x, split = ":", fixed = TRUE)
    )
  )
}

#' Helper function that checks a conditioning index against a particular causal ordering.
#'
#' @param index Integer conditioning index to check against the causal ordering.
#' @param causal_ordering List of vectors specifying (partial) causal ordering. Each element in
#' the list is a component in the order, which can contain one or more variable indices in a vector.
#' For example, in list(1, c(2, 3)), 2 > 1 and 3 > 1, but 2 and 3 are not comparable.
#'
#' @keywords internal
#'
#' @author Tom Heskes, Ioan Gabriel Bucur
respects_order <- function(index, causal_ordering) {
  
  for (i in index) {
    
    idx_position <- Position(function(x) i %in% x, causal_ordering, nomatch = 0)
    
    stopifnot(idx_position > 0) # It should always be in the causal_ordering
    
    # check for precedents (only relevant if not root set)
    if (idx_position > 1) {
      
      # get precedents
      precedents <- unlist(causal_ordering[1:(idx_position-1)])
	  
      # all precedents must be in index
      if (!setequal(precedents, intersect(precedents, index))) {
        return(FALSE)
      }
    }
  }
  
  return(TRUE)
}
