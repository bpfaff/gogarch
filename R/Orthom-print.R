##
## print-method for objects of class Orthom
##
setMethod(f = "print", signature(x = "Orthom"), function(x, ...) print(x@M, ...))
