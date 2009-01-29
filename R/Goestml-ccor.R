setMethod(f = "ccor", signature(object = "Goestml"), definition = function(object){
  ccor(as(object, "GoGARCH"))
})
