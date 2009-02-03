setMethod(f = "ccor", signature(object = "Gopredict"), definition = function(object){
  m <- ncol(object@Xf)
  d <- m * (m - 1) / 2
  n <- nrow(object@Xf)
  cnames <- colnames(object@Xf)
  ccor <- matrix(c(unlist(lapply(object@Hf, function(x) cov2cor(x)[lower.tri(x)]))), ncol = d, nrow = n, byrow = TRUE)
  ngrid <- data.frame(expand.grid(cnames, cnames), stringsAsFactors = FALSE)
  mgrid <- paste(ngrid[, 1], ngrid[, 2], sep = " & ")
  mgrid <- matrix(mgrid, nrow = m, ncol = m)
  names <- mgrid[lower.tri(mgrid)]
  colnames(ccor) <- names
  rownames(ccor) <- rownames(object@Xf)
  return(ccor)
})
