##
## Transpose method for objects of class Orthom
##
setMethod("t", "Orthom", function(x) t(x@M))
