setMethod(f = "goest", signature(object = "Goestica"), definition = function(object, initial, garchlist, ...){
  X <- object@X
  m <- ncol(X)
  n <- nrow(X)
  P <- object@P
  Id <- diag(m)
  Dsqr <- object@Dsqr
  ica <- fastICA(X, n.comp = m, ...)
  W <- ica$W
  Z <- P %*% Dsqr %*% t(P) %*% W
  Zinv <- solve(Z)
  Y <- X %*% Zinv
  fitted <- apply(Y, 2, function(x) do.call("garchFit", c(list(formula = object@garchf, data = quote(x)), garchlist)))
  H <- matrix(unlist(lapply(fitted, function(x) x@h.t)), ncol = m, nrow = n)
  Hdf <- data.frame(t(H))
  Ht <- lapply(Hdf, function(x) Z %*% diag(x) %*% t(Z))
  names(Ht) <- rownames(object@X)            
  result <- new("Goestica", ica = ica, estby = "fast ICA", U = W, Z = Z, Y = Y, H = Ht, models = fitted, X = object@X, P = object@P, Dsqr = object@Dsqr, V = object@V, garchf = object@garchf, name = object@name)
  return(result)  
})
