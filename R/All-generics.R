##
## Generic definition for estimating GO-GARCH models
##
setGeneric("goest", function(object, initial, garchlist, ...) standardGeneric("goest"))
##
## Generic definition for extracting object@M for objects of class Orthom
##
setGeneric("M", function(object, ...) standardGeneric("M"))
##
## Generic definition for extracting Euler angles
##
setGeneric("angles", function(object, ...) standardGeneric("angles"))
##
## Generic definition for extracting the conditional variances
##
setGeneric("cvar", function(object, ...) standardGeneric("cvar"))
##
## Generic definition for extracting the conditional covariances
##
setGeneric("ccov", function(object, ...) standardGeneric("ccov"))
##
## Generic definition for extracting the conditional correlations
##
setGeneric("ccor", function(object, ...) standardGeneric("ccor"))
##
## Generic definition for extracting convergence codes
##
setGeneric("converged", function(object, ...) standardGeneric("converged"))
