##
## Transpose method for objects of class Orthom
##
setMethod("t", signature(x = "Orthom"), function(x) t(x@M))
