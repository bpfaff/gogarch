setMethod(f = "ccov", signature(object = "Gopredict"), definition = function(object){
  m <- ncol(object@Xf)
  d <- m * (m - 1) / 2
  n <- nrow(object@Xf)
  cnames <- colnames(object@Xf)
  ccov <- matrix(c(unlist(lapply(object@Hf, function(x) x[lower.tri(x)]))), ncol = d, nrow = n, byrow = TRUE)
  ngrid <- data.frame(expand.grid(cnames, cnames), stringsAsFactors = FALSE)
  mgrid <- paste(ngrid[, 1], ngrid[, 2], sep = " & ")
  mgrid <- matrix(mgrid, nrow = m, ncol = m)
  names <- mgrid[lower.tri(mgrid)]
  colnames(ccov) <- names
  rownames(ccov) <- rownames(object@Xf)
  return(ccov)
})
