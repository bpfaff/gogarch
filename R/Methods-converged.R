setMethod(f = "converged", signature(object = "GoGARCH"), definition = function(object, ...){
  conv <- c(unlist(lapply(object@models, function(x) x@fit$convergence)))
  cnames <- paste("y", seq(along.with = conv), sep = "")
  names(conv) <- cnames
  return(conv)
})

setMethod(f = "converged", signature(object = "Goestica"), definition = function(object){
  converged(as(object, "GoGARCH"))
})

setMethod(f = "converged", signature(object = "Goestmm"), definition = function(object){
  converged(as(object, "GoGARCH"))
})

setMethod(f = "converged", signature(object = "Goestnls"), definition = function(object){
  converged(as(object, "GoGARCH"))
})

setMethod(f = "converged", signature(object = "Goestml"), definition = function(object){
  converged(as(object, "GoGARCH"))
})

