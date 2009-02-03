setMethod(f = "cvar", signature(object = "Goestnls"), definition = function(object){
  cvar(as(object, "GoGARCH"))
})
