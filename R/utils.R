
is.wholenumber <-function(x, tol = .Machine$double.eps^0.5){
  is.numeric(x) && (abs(x - round(x)) < tol)
}

