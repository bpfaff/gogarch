setMethod(f = "goest", signature = c(object = "Goestml", initial = "numeric", garchlist = "list"), definition = function(object, initial, garchlist, ...){
  llobj <- nlminb(start = initial, objective = gollh, object = object, garchlist = garchlist, lower = 1.5e-8, upper = pi/2, ...)
  gotheta <- gotheta(llobj$par, object)
  result <- new("Goestml", opt = llobj, estby = "maximum likelihood", gotheta)
  return(result)  
})
