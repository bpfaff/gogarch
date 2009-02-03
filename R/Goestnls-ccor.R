setMethod(f = "ccor", signature(object = "Goestnls"), definition = function(object){
  ccor(as(object, "GoGARCH"))
})
