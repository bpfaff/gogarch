##
## Validity function for objects of class Orthom
##
validOrthomObject <- function(object){
  m <- nrow(object@M)
  if(all.equal(diff(dim(object@M)), 0, check.attributes = FALSE)){
    TRUE
  } else {
    stop("\nObject is not a square matrix.\n")
  }
  if(all.equal(det(object@M), 1, check.attributes = FALSE)){
    TRUE
  } else {
    stop("\nDeterminant of object is not equal to 1.\n")
  }
  if(all.equal(crossprod(object@M), diag(m), check.attributes = FALSE)){
    TRUE
  } else {
    stop("\nThe cross product of the object is not the Identity matrix.\n")
  }
}
##
## Class definition of orthogonal matrices
##
setClass(Class = "Orthom", representation(M = "matrix"), validity = validOrthomObject)

