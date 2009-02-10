setMethod(f = "cvar", signature(object = "Goestmm"), definition = function(object){
  cvar(as(object, "GoGARCH"))
})
