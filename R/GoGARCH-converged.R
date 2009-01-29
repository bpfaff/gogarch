setMethod(f = "converged", signature(object = "GoGARCH"), definition = function(object, ...){
  conv <- c(unlist(lapply(object@models, function(x) x@fit$convergence)))
  cnames <- paste("y", seq(along.with = conv), sep = "")
  names(conv) <- cnames
  return(conv)
})
