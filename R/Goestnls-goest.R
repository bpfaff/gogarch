setMethod(f = "goest", signature(object = "Goestnls"), definition = function(object, initial, garchlist, ...){
  d <- ncol(object@X)
  if(is.null(initial)){
    l <- d * (d + 1)/2
    initial <- rep(0.1, l)
  } else {
    l <- length(initial)
    if (l != d * (d + 1)/2) {
      stop(paste("\nLength of initial vector does not match length of vech(B).\n", "It should have length: ", d * (d + 1)/2, sep = ""))
    }
  }
  X <- object@X
  m <- ncol(X)
  n <- nrow(X)
  Dsqr <- object@Dsqr
  Dsqri <- diag(1 / diag(Dsqr))
  P <- object@P
  S <- X %*% P %*% Dsqri 
  SSI <- list()
  length(SSI) <- n
  for(i in 1:n){
    SSI[[i]] <- S[i, ] %*% t(S[i, ]) - diag(m)
  }
  SSI0 <- SSI[-1]
  SSI1 <- SSI[-n]
  SSI <- list(SSI0 = SSI0, SSI1 = SSI1)  
  nlsobj <- nlminb(start = initial, objective = gonls, SSI = SSI, ...)
  B <- unvech(nlsobj$par)
  U <- svd(B)$u
  Z <- P %*% Dsqr %*% t(U)
  Y <- S %*% U
  fitted <- apply(Y, 2, function(x) do.call("garchFit", c(list(formula = object@garchf, data = quote(x)), garchlist)))
  H <- matrix(unlist(lapply(fitted, function(x) x@h.t)), ncol = m, nrow = n)
  Hdf <- data.frame(t(H))
  Ht <- lapply(Hdf, function(x) Z %*% diag(x) %*% t(Z))
  names(Ht) <- rownames(object@X)          
  result <- new("Goestnls", nls = nlsobj, estby = "non-linear Least-Squares", U = U, Z = Z, Y = Y, H = Ht, models = fitted, X = object@X, P = object@P, Dsqr = object@Dsqr, V = object@V, garchf = object@garchf, name = object@name) 
  return(result)  
})
