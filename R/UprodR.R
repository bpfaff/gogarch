UprodR <-
function(theta){
  theta <- as.vector(theta)
  l <- length(theta)
  d <- as.integer(0.5 + sqrt(0.5^2 + 2*l))  
  if(l != d * (d - 1) / 2){
    stop("\nLength of theta does not match implied dimension of U.\n")
  }
  Id <- diag(d)
  U <- Id
  rc <- combn(x = d, m = 2)
  idx <- seq(along.with = theta)
  Rs <- lapply(idx, function(x){
    tmp <- Id
    tmp[rc[, x], rc[, x]] <- Rd2(theta = theta[x])
    return(tmp)
  })
  for(i in 1:l) U <- U %*% Rs[[i]]
  result <- new("Orthom", M = U)
  return(result)
}

