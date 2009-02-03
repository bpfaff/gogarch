gonls <-
function(params, SSI){
  B <- unvech(params)
  n <- length(SSI[[1]]) - 1
  fl <- list()
  length(fl) <- n
  for(i in 1:n){
    fl[[i]] <- (SSI[[1]][[i]] - B %*% SSI[[2]][[i]] %*% B)^2
  }
  f <- sum(unlist(lapply(fl, function(x) sum(diag(x))))) / n
  return(f)   
}
