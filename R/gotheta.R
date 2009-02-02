gotheta <-
function(theta, object, garchlist = list(init.rec = "mci", delta = 2, skew = 1, shape = 4, cond.dist = "norm", include.mean = FALSE, include.delta = NULL, include.skew = NULL, include.shape = NULL, leverage = NULL, trace = FALSE, algorithm = "nlminb", hessian = "ropt", control = list(), title = NULL, description = NULL)){
  if(!any(inherits(object, what = c("Goinit", "GoGARCH", "Goestml")))) {
    stop("\nObject is neither of class 'Goinit', 'GoGARCH' or 'Goestml'.\n")
  }
  l <- length(theta)
  d <- as.integer(0.5 + sqrt(0.5^2 + 2 * l))
  if (l != d * (d - 1)/2) {
    stop(paste("\nLength of theta does not match implied dimension of orthogonal matrix.\n", "It should have length: ", d, sep = ""))
  }
  m <- ncol(object@X)
  n <- nrow(object@X)
  U <- UprodR(theta)@M
  Z <- object@P %*% object@Dsqr %*% t(U)
  Zinv <- solve(Z)
  Y <- object@X %*% Zinv
  fitted <- apply(Y, 2, function(x) do.call("garchFit", c(list(formula = object@garchf, data = quote(x)), garchlist)))
  H <- matrix(unlist(lapply(fitted, function(x) x@h.t)), ncol = m, nrow = n)
  Hdf <- data.frame(t(H))
  Ht <- lapply(Hdf, function(x) Z %*% diag(x) %*% t(Z))
  names(Ht) <- rownames(object@X)
  result <- new("GoGARCH", Z = Z, Y = Y, H = Ht, models = fitted, X = object@X, P = object@P, Dsqr = object@Dsqr, V = object@V, garchf = object@garchf, CALL = match.call())
  return(result)
}
