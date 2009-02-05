cora <- function(SSI, lag = 1){
  lags <- abs(as.integer(lags))
  dims <- dim(SSI)
  Gamma <- matrix(0, nrow = dims[1], ncol = dims[2])
  SSIp <- array(dim = dims)
  for(i in 1:dims[3]){
    SSIp[, ,i] <- SSI[, ,i] %*% SSI[, ,i]
    Gamma <- Gamma + SSIp[, , i]
  }
  Gamma <- Gamma / dims[3]
  Gsvd <- svd(Gamma)
  Gsqrtinv <- Gsvd$u %*% diag(1/sqrt(Gsvd$d)) %*% t(Gsvd$u)
  idx <- 1:dims[3]
  if(identical(lags, as.integer(0))){
    idx1 <- idx
    idx2 <- idx
  } else {
    idx1 <- idx[-c(1:lags)]
    idx2 <- rev(rev(idx)[-c(1:lags)])
  }
  nl <- length(idx1)
  Gamma <- matrix(0, nrow = dims[1], ncol = dims[2])
  SSIc <- array(dim = c(dims[1], dims[2], nl))
  for(i in 1:nl){
    SSIc[, , i] <- SSI[, , idx1[i]] %*% SSI[, , idx2[i]]
    Gamma <- Gamma + SSIc[, , i]
  }
  Gamma <- Gamma / nl
  cora <- Gsqrtinv %*% Gamma %*% Gsqrtinv
  cora <- (cora + t(cora)) / 2
  return(cora)
}
