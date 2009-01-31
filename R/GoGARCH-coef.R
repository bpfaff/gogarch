setMethod(f = "coef", signature(object = "GoGARCH"), definition = function(object){
  garchc <- matrix(unlist(lapply(object@models, coef)), nrow = ncol(object@X), byrow = TRUE)
  colnames(garchc) <- names(object@models[[1]]@fit$par)
  rownames(garchc) <- paste("y", 1:nrow(garchc), sep = "")
  return(garchc)
})
    
