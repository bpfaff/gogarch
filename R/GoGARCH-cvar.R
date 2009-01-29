setMethod(f = "cvar", signature(object = "GoGARCH"), definition = function(object){
  m <- ncol(object@X)
  n <- nrow(object@X)
  cvar <- matrix(c(unlist(lapply(object@H, function(x) diag(x)))), ncol = m, nrow = n, byrow = TRUE)
  colnames(cvar) <- paste("V.", colnames(object@X), sep = "")
  rownames(cvar) <- rownames(object@X)
  return(cvar)
})
