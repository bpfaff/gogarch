setMethod(f = "ccov", signature(object = "GoGARCH"), definition = function(object){
  m <- ncol(object@X)
  d <- m * (m - 1) / 2
  n <- nrow(object@X)
  cnames <- colnames(object@X)
  ccov <- matrix(c(unlist(lapply(object@H, function(x) x[lower.tri(x)]))), ncol = d, nrow = n, byrow = TRUE)
  ngrid <- data.frame(expand.grid(cnames, cnames), stringsAsFactors = FALSE)
  mgrid <- paste(ngrid[, 1], ngrid[, 2], sep = " & ")
  mgrid <- matrix(mgrid, nrow = m, ncol = m)
  names <- mgrid[lower.tri(mgrid)]
  colnames(ccov) <- names
  rownames(ccov) <- rownames(object@X)
  return(ccov)
})
