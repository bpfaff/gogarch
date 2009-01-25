##
## Validity function for objects of class Goinit
##
validGoinitObject <- function(object){
  m <- nrow(object@V)
  if(all.equal(diff(dim(object@V)), 0, check.attributes = FALSE)){
    TRUE
  } else {
    print("\nObject 'V' is not a square matrix.\n")
  }
  if(all.equal(diff(dim(object@P)), 0, check.attributes = FALSE)){
    TRUE
  } else {
    print("\nObject 'P' is not a square matrix.\n")
  }
  if(all.equal(diff(dim(object@Dsqr)), 0, check.attributes = FALSE)){
    TRUE
  } else {
    print("\nObject 'Dsqr' is not a square matrix.\n")
  }
  if(all.equal(det(object@Dsqr), prod(diag(object@Dsqr)), check.attributes = FALSE)){
    TRUE
  } else {
    print("\nObject 'Dsqr' is not a diagonal matrix.\n")
  }  
  if(all.equal(object@V, object@P %*% object@Dsqr^2 %*% t(object@P), check.attributes = FALSE)){ 
    TRUE
  } else {
    print("\nCovariance matrix cannot be replicated from singular values.\n")
  }
}
##
## Class definition of initial GO-GARCH objects
##
setClass(Class = "Goinit", representation(X = "matrix", V = "matrix", P = "matrix", Dsqr = "matrix", garchf = "formula"), validity = validGoinitObject)

