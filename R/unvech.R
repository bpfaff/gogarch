unvech <-
function(v){
  v <- as.vector(v)
  l <- length(v)
  n <- -(1 - sqrt(1 + 8 * l)) / 2
  if(n %% 1 != 0.0){
    stop("\nCannot produce symmetric matrix, check length of v.\n")
  }
  X <- matrix(NA, ncol = n, nrow = n)
  X[lower.tri(X, diag = TRUE)] <- v
  X[upper.tri(X)] <- X[lower.tri(X)]
  return(X)
}

