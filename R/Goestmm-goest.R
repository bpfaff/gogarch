setMethod(f = "goest", signature(object = "Goestmm"), definition = function(object, lag.max, garchlist, ...){
  lag.max <- abs(as.integer(lag.max))
  X <- object@X
  m <- ncol(X)
  n <- nrow(X)
  P <- object@P
  Id <- diag(m)
  Dsqr <- object@Dsqr
  S <- P %*% Dsqr %*% t(P)
  Sinv <- solve(S)
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
    svd <- lapply(Phil, function(x) eigen(x, symmetric = TRUE))
    evmin <- unlist(lapply(svd, function(x){
      sel <- combn(1:m, 2)
      diffs2 <- (x$values[sel[1, ]] - x$values[sel[2, ]])^2
      min(diffs2)
    }))
    denom <- sum(evmin)
    weights <- evmin / denom
    Ul <- lapply(svd, function(x) x$vectors)
    Ul[[1]] <- Umatch(Id, Ul[[1]])
    Sm <- matrix(0, nrow = m, ncol = m)
    for(i in 1:lag.max){
      Ul[[i]] <- Umatch(Ul[[1]], Ul[[i]])
      mmprod <- weights[i] * (Id - Ul[[i]]) %*% solve(Id + Ul[[i]])
      Sm <- Sm + mmprod
    }
    Umatched <- Ul
    U <- (Id - Sm) %*% solve(Id + Sm)
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
