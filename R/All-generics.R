##
## Generic definition for estimating GO-GARCH models
##
setGeneric("goest", function(object, initial, garchlist, ...) standardGeneric("goest"))
##
## Generic definition for extracting object@M for objects of class Orthom
##
setGeneric("M", function(object, ...) standardGeneric("M"))
