setMethod(f = "converged", signature(object = "Goestmm"), definition = function(object){
  converged(as(object, "GoGARCH"))
})
