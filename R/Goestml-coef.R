setMethod(f = "coef", signature(object = "Goestml"), definition = function(object){
  coef(as(object, "GoGARCH"))
})
    
