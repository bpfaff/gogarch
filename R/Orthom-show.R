##
## show-method for objects of class Orthom
##
setMethod(f = "show", signature(object = "Orthom"), function(object) print(object@M))
