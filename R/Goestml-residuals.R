setMethod(f = "residuals", signature(object = "Goestml"), definition = function(object,  ...){
  residuals(as(object, "GoGARCH"), ...) 
})


setMethod(f = "resid", signature(object = "Goestml"), definition = function(object, ...){
  resid(as(object, "GoGARCH"), ...) 
})

