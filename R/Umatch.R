Umatch <- function(from, to){
  cols <- ncol(from)
  mat <- matrix(0, nrow = cols, ncol = cols)
  inner <- abs(colSums(from[, 1] * to))
  maxcol <- which.max(inner)
  mat[, 1] <- to[, maxcol]
  to <- as.matrix(to[, -maxcol])
  for(i in 2:cols){
    inner <- abs(colSums(from[, i] * to))
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
