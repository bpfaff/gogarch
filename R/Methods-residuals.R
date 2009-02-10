setMethod(f = "residuals", signature(object = "GoGARCH"), definition = function(object, standardize = FALSE){
  m <- ncol(object@X)
  n <- nrow(object@X)
  resl <- lapply(object@models, residuals, standardize = standardize)
  resm <- matrix(c(unlist(resl)), ncol = m, nrow = n)
  ynames <- paste("y", 1:2, sep = "")
  colnames(resm) <- ynames
  rownames(resm) <- rownames(object@X)
  return(resm)
})

setMethod(f = "residuals", signature(object = "Goestica"), definition = function(object, standardize = FALSE){
  callNextMethod(object = object, standardize = standardize)
})

setMethod(f = "residuals", signature(object = "Goestmm"), definition = function(object, standardize = FALSE){
  callNextMethod(object = object, standardize = standardize)
})

setMethod(f = "residuals", signature(object = "Goestnls"), definition = function(object, standardize = FALSE){
  callNextMethod(object = object, standardize = standardize)
})

setMethod(f = "residuals", signature(object = "Goestml"), definition = function(object, standardize = FALSE){
  callNextMethod(object = object, standardize = standardize)
})
