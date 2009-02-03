goinit <- function(X, garchf = ~ garch(1, 1), scale = FALSE){
  dname <- deparse(substitute(X))
  X <- as.matrix(X)
  if(ncol(X) > nrow(X)){
    stop("\nMatrix has more columns than rows.\n")
  }
  garchf <- as.formula(garchf)
  if(scale){
    X <- scale(X)
  }
  V <- t(X) %*% X / nrow(X)
  svd <- svd(V)
  P <- svd$u
  Dsqr <- diag(sqrt(svd$d))
  result <- new("Goinit", X = X, V = V, P = P, Dsqr = Dsqr, garchf = garchf, name = dname)
  return(result)
}
