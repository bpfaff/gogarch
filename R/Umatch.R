Umatch <- function(from, to){
  cols <- ncol(from)
  mat <- matrix(0, nrow = cols, ncol = cols) 
  for(i in 1:cols){
    inner <- colSums(to * as.vector(from[, i]))
    maxcol <- which.max(inner)
    mat[, i] <- to[, maxcol]
    to <- as.matrix(to[, -maxcol])
  }
  signs <- matrix(sign(diag(mat)), nrow = cols, ncol = cols, byrow = TRUE)
  mat <- mat * signs
  if(det(mat) < 1.0){
    colminus <- which.min(abs(colSums(mat * from)))
    mat[, colminus] <- -1.0 * mat[, colminus]
  }
  return(mat)
}
