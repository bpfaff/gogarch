##
## Validity function for objects of class Orthom
##
validOrthomObject <- function(object){
  m <- nrow(object@M)
  Id <- diag(m)
  if(diff(dim(object@M)) == 0){
    TRUE
  } else {
    print("\nObject is not a square matrix.\n")
  }
  if(isTRUE(all.equal(1, abs(det(object@M))))){
    TRUE
  } else {
    print("\nAbsolute value of Determinant of object is not equal to 1.\n")
  }
  if(isTRUE(all.equal(Id, crossprod(object@M), check.attributes = FALSE))){
    TRUE
  } else {
    print("\nThe cross product of the object is not the Identity matrix.\n")
  }
}
##
## Setting as validity function
##
setValidity("Orthom", validOrthomObject)
