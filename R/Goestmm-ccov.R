setMethod(f = "ccov", signature(object = "Goestmm"), definition = function(object){
  ccov(as(object, "GoGARCH"))
})
