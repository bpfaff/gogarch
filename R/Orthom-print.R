##
## print-method for objects of class Orthom
##
setMethod(f = "print", signature = "Orthom", function(x, ...) print(x@M, ...))
