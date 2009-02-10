setMethod(f = "ccor", signature(object = "Goestica"), definition = function(object){
  ccor(as(object, "GoGARCH"))
})
