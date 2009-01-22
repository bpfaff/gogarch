Rd2 <-
function(theta){
  theta <- as.vector(theta)
  if(length(theta) > 1){
    stop("\nLength of argument 'theta' should be one.\n")
    }
  if((theta <= 0) | (theta > pi/2)){
    stop("\nTheta should be in the interval [0, pi/2).\n")
  }
  R <- matrix(c(cos(theta), sin(theta), -sin(theta), cos(theta)), ncol = 2, nrow = 2)
  return(R)              
}

