setMethod(f = "ccor", signature(object = "Goestmm"), definition = function(object){
  ccor(as(object, "GoGARCH"))
})
