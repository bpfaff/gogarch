setMethod(f = "converged", signature(object = "Goestnls"), definition = function(object){
  converged(as(object, "GoGARCH"))
})
