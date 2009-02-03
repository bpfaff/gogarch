setMethod(f = "cvar", signature(object = "Gopredict"), definition = function(object){
  m <- ncol(object@Xf)
  n <- nrow(object@Xf)
  cvar <- matrix(c(unlist(lapply(object@Hf, function(x) diag(x)))), ncol = m, nrow = n, byrow = TRUE)
  colnames(cvar) <- paste("V.", colnames(object@Xf), sep = "")
  rownames(cvar) <- rownames(object@Xf)
  return(cvar)
})
