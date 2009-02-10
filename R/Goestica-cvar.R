setMethod(f = "cvar", signature(object = "Goestica"), definition = function(object){
  cvar(as(object, "GoGARCH"))
})
