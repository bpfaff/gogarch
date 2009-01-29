setMethod(f = "residuals", signature(object = "GoGARCH"), definition = function(object, ...){
  m <- ncol(object@X)
  n <- nrow(object@X)
  resl <- lapply(object@models, residuals, ...)
  resm <- matrix(c(unlist(resl)), ncol = m, nrow = n)
  ynames <- paste("y", 1:2, sep = "")
  colnames(resm) <- ynames
  rownames(resm) <- rownames(object@X)
  return(resm)
})

setMethod(f = "resid", signature(object = "GoGARCH"), definition = function(object, ...){
  m <- ncol(object@X)
  n <- nrow(object@X)
  resl <- lapply(object@models, residuals, ...)
  resm <- matrix(c(unlist(resl)), ncol = m, nrow = n)
  ynames <- paste("y", 1:2, sep = "")
  colnames(resm) <- ynames
  rownames(resm) <- rownames(object@X)
  return(resm)
})

