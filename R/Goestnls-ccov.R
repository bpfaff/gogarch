setMethod(f = "ccov", signature(object = "Goestnls"), definition = function(object){
  ccov(as(object, "GoGARCH"))
})
