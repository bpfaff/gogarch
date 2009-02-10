setMethod(f = "converged", signature(object = "Goestica"), definition = function(object){
  converged(as(object, "GoGARCH"))
})
