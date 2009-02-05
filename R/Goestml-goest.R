setMethod(f = "goest", signature(object = "Goestml"), definition = function(object, initial, garchlist, ...){
  d <- ncol(object@X)
  if(is.null(initial)){
    l <- d * (d - 1)/2
    initial <- seq(3.0, 0.1, length.out = l)
  } else {
    l <- length(initial)
    if (l != d * (d - 1)/2) {
      stop(paste("\nLength of initial vector does not match implied dimension of orthogonal matrix.\n", "It should have length: ", d * (d - 1)/2, sep = ""))
    }
  }
  llobj <- nlminb(start = initial, objective = gollh, object = object, garchlist = garchlist, lower = 1.5e-8, upper = pi/2, ...)
  gotheta <- gotheta(llobj$par, object, garchlist)
  result <- new("Goestml", opt = llobj, estby = "maximum likelihood", gotheta)
  return(result)  
})
