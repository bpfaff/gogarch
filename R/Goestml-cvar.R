setMethod(f = "cvar", signature(object = "Goestml"), definition = function(object){
  cvar(as(object, "GoGARCH"))
})
