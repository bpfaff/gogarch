setMethod(f = "converged", signature(object = "Goestml"), definition = function(object){
  converged(as(object, "GoGARCH"))
})
