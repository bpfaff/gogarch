setMethod(f = "goest", signature(object = "Goestnls"), definition = function(object, initial, garchlist, ...){
  X <- object@X
  m <- ncol(X)
  n <- nrow(X)
  Dsqr <- object@Dsqr
  Dsqri <- diag(1 / diag(Dsqr))
  P <- object@P
  S <- data.frame(t(X %*% Dsqri %*% P))
  SSI <- lapply(S, function(x) x %*% t(x) - diag(m))
  SSI0 <- SSI[-1]
  SSI1 <- SSI[-n]
  SSI <- list(SSI0 = SSI0, SSI1 = SSI1)  
  nlsobj <- optim(par = initial, fn = gonls, SSI = SSI, ...)
  B <- unvech(nlsobj$par)
  U <- svd(B)$u
  U <- eigen(B)$vectors
  Z <- P %*% Dsqr %*% t(U)
  Zinv <- solve(Z)
  Y <- X %*% Zinv
  fitted <- apply(Y, 2, function(x) do.call("garchFit", c(list(formula = object@garchf, data = quote(x)), garchlist)))
  H <- matrix(unlist(lapply(fitted, function(x) x@h.t)), ncol = m, nrow = n)
  Hdf <- data.frame(t(H))
  Ht <- lapply(Hdf, function(x) Z %*% diag(x) %*% t(Z))
  names(Ht) <- rownames(object@X)          
  result <- new("Goestnls", nls = nlsobj, estby = "non-linear Least-Squares", Z = Z, Y = Y, H = Ht, models = fitted, X = object@X, P = object@P, Dsqr = object@Dsqr, V = object@V, garchf = object@garchf, name = object@name) 
  return(result)  
})
