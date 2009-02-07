setMethod(f = "goest", signature(object = "Goestmm"), definition = function(object, lag.max, garchlist, ...){
  lag.max <- abs(as.integer(lag.max))
  X <- object@X
  m <- ncol(X)
  n <- nrow(X)
  P <- object@P
  Id <- diag(m)
  Dsqr <- object@Dsqr
  Dsqri <- diag(1 / diag(Dsqr))
  Sinv <- P %*% Dsqri %*% t(P)
  S <- X %*% Sinv
  if(lag.max < 1){
    U <- Id
    Umatched <- list(U)
    weights <- 1
  } else {
    SSI <- array(dim = c(m, m, n))
    for(i in 1:n){
      SSI[, , i] <- S[i, ] %*% t(S[i, ]) - diag(m)
    }
    Phil <- lapply(1:lag.max, function(x) cora(SSI, lag = x))
    svd <- lapply(Phil, function(x) svd(x))
    evmin <- unlist(lapply(svd, function(x){
      sel <- combn(1:m, 2)
      diffs2 <- (x$d[sel[1, ]] - x$d[sel[2, ]])^2
      min(diffs2)
    }))
    denom <- sum(evmin)
    weights <- evmin / denom
    Ul <- lapply(svd, function(x) x$u)
    Ul[[1]] <- Umatch(Id, Ul[[1]])
    Sm <- matrix(0, nrow = m, ncol = m)
    for(i in 1:lag.max){
      Ul[[i]] <- Umatch(Ul[[1]], Ul[[i]])
      mp <- Id + Ul[[i]]
      mpsvd <- svd(mp)
      mpinv <- mpsvd$u %*% diag(1/mpsvd$d) %*% t(mpsvd$u)
      mm <- Id - Ul[[i]]
      mmprod <- weights[i] * mm %*% mpinv
      Sm <- Sm + mmprod
    }
    Umatched <- Ul
    mp <- Id + Sm
    mpsvd <- svd(mp)
    mpinv <- mpsvd$u %*% diag(1/mpsvd$d) %*% t(mpsvd$u)
    mm <- Id - Sm
    U <- mm %*% mpinv
  }
  Y <- S %*% U
  Z <- P %*% Dsqr %*% t(P) %*% t(U)
  fitted <- apply(Y, 2, function(x) do.call("garchFit", c(list(formula = object@garchf, data = quote(x)), garchlist)))
  H <- matrix(unlist(lapply(fitted, function(x) x@h.t)), ncol = m, nrow = n)
  Hdf <- data.frame(t(H))
  Ht <- lapply(Hdf, function(x) Z %*% diag(x) %*% t(Z))
  names(Ht) <- rownames(object@X)          
  result <- new("Goestmm", weights = weights, Umatched = Umatched, estby = "Methods of Moments", U = U, Z = Z, Y = Y, H = Ht, models = fitted, X = object@X, P = object@P, Dsqr = object@Dsqr, V = object@V, garchf = object@garchf, name = object@name) 
  return(result)  
})
