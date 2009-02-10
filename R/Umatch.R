Umatch <- function(from, to){
  cols <- ncol(from)
  mat <- matrix(0, nrow = cols, ncol = cols)
  for(i in 1:cols){
    inner <- abs(colSums(from[, i] * to))
    maxcol <- which.max(inner)
    mat[, i] <- to[, maxcol]
    to <- as.matrix(to[, -c(maxcol)])
  }
  signs <- matrix(sign(diag(mat)), nrow = cols, ncol = cols, byrow = TRUE)
  mat <-  signs * mat
  if(det(mat) < 0.0){
    colminus <- which.min(abs(colSums(from * mat)))
    mat[, colminus] <- -1.0 * mat[, colminus]
  }
  return(mat)
}
