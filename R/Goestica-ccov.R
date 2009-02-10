setMethod(f = "ccov", signature(object = "Goestica"), definition = function(object){
  ccov(as(object, "GoGARCH"))
})
