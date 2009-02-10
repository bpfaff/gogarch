setMethod(f = "coef", signature(object = "GoGARCH"), definition = function(object){
  garchc <- matrix(unlist(lapply(object@models, coef)), nrow = ncol(object@X), byrow = TRUE)
  colnames(garchc) <- names(object@models[[1]]@fit$par)
  rownames(garchc) <- paste("y", 1:nrow(garchc), sep = "")
  return(garchc)
})

setMethod(f = "coef", signature(object = "Goestica"), definition = function(object){
  callNextMethod()
})

setMethod(f = "coef", signature(object = "Goestmm"), definition = function(object){
  callNextMethod()
})

setMethod(f = "coef", signature(object = "Goestnls"), definition = function(object){
  callNextMethod()
})

setMethod(f = "coef", signature(object = "Goestml"), definition = function(object){
  callNextMethod()
})

