
## THIS IS BASED ENTIRELY ON THE CARET PACKAGE - JUST SOME NAME CHANGES AND REMOVAL OF SOME FUNCTIONALITY
## SEE HERE: https://github.com/topepo/caret/blob/master/pkg/caret/R/dummyVar.R

make_dummies <-
  function(formula, ...){
    UseMethod("make_dummies")
  }

make_dummies.default <- function (formula, data, fullRank = FALSE, sep = ".", ...) {
  formula <- as.formula(formula)
  if(!is.data.frame(data)) data <- as.data.frame(data, stringsAsFactors = FALSE)

  vars <- all.vars(formula)
  if(any(vars == ".")) {
    vars <- vars[vars != "."]
    vars <- unique(c(vars, colnames(data)))
  }
  p <- sapply(data[, vars, drop = FALSE], is.factor)
  p_sum <- sum(p)

  if(p_sum > 0) {
    facVars <- vars[p]
    lvls <- lapply(data[, facVars, drop = FALSE], levels)
  } else {
    facVars <- NULL
    lvls <- NULL
  }
  trms <- attr(model.frame(formula, data), "terms")
  out <- list(call = match.call(),
              form = formula,
              vars = vars,
              facVars = facVars,
              lvls = lvls,
              sep = sep,
              terms = trms,
              fullRank = fullRank) # this is important for Shapley!
  class(out) <- "make_dummies"
  out

}

predict.make_dummies <- function(object, newdata, na.action = na.pass, ...) {

  if(is.null(newdata)) stop("newdata must be supplied")
  if(!is.data.frame(newdata)) newdata <- as.data.frame(newdata, stringsAsFactors = FALSE)
  if(!all(object$vars %in% names(newdata))) stop(
    paste("Variable(s)",
          paste("'", object$vars[!object$vars %in% names(newdata)],
                "'", sep = "",
                collapse = ", "),
          "are not in newdata"))
  Terms <- object$terms
  Terms <- delete.response(Terms)
  if(!object$fullRank) { # this is important for Shapley!
    oldContr <- options("contrasts")$contrasts
    newContr <- oldContr
    newContr["unordered"] <- "contr.ltfr"
    options(contrasts = newContr)
    on.exit(options(contrasts = oldContr))
  }
  m <- model.frame(Terms, newdata, na.action = na.action, xlev = object$lvls)

  x <- model.matrix(Terms, m)

  cnames <- colnames(x)
  if(!is.null(object$sep)) {
    for(i in object$facVars[order(-nchar(object$facVars))]) {
      ## the default output form model.matrix is NAMElevel with no separator.
      for(j in object$lvls[[i]]) {
        from_text <- paste0(i, j)
        to_text <- paste(i, j, sep = object$sep)
        pos = which(cnames == from_text)
        # If there are several identical NAMElevel matching (example: "X1" with level "11" and "X11" with level "1")
        if (length(pos) > 1) {
          # If the level j is not the first level of the feature i
          if (which(object$lvls[[i]] == j) > 1) {
            # Then we just have to test for the preceding NAMElevel being NAME(level-1)
            cnames[pos][cnames[pos-1] == paste(i, object$lvls[[i]][which(object$lvls[[i]] == j)-1], sep = object$sep)] <- to_text
          } else {
            # Otherwise, we have to test for the preceding NAMElevel being (NAME-1)(last_level)
            cnames[pos][cnames[pos-1] == paste(object$facVars[order(-nchar(object$facVars))][which(object$facVars[order(-nchar(object$facVars))] == i) - 1],
                                               utils::tail(object$lvls[[object$facVars[order(-nchar(object$facVars))][which(object$facVars[order(-nchar(object$facVars))] == i) - 1]]], n=1), sep = object$sep)] <- to_text
          }
        } else {
          # Otherwise simply replace the last occurence of the pattern
          cnames[pos] <- to_text
        }
      }
    }
  }
  colnames(x) <- cnames
  x[, colnames(x) != "(Intercept)", drop = FALSE]
}

print.make_dummies <- function(x, ...) {
  cat("Dummy Variable Object\n\n")
  cat("Formula: ")
  print(x$form)
  cat(length(x$vars),  " variables, ", length(x$facVars), " factors\n", sep = "")
  if(!is.null(x$sep)) cat("Variables and levels will be separated by '",
                                          x$sep, "'\n", sep = "")
  if(x$fullRank) cat("A full rank encoding is used") else cat("A less than full rank encoding is used")
  cat("\n")
  invisible(x)
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

