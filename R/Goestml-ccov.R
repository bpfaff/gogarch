setMethod(f = "ccov", signature(object = "Goestml"), definition = function(object){
  ccov(as(object, "GoGARCH"))
})
