cora <- function(SSI, lag = 1, standardize = TRUE){
  lag <- abs(as.integer(lag))
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
  if(identical(lag, as.integer(0))){
    idx1 <- idx
    idx2 <- idx
  } else {
    idx1 <- idx[-c(1:lag)]
    idx2 <- rev(rev(idx)[-c(1:lag)])
  }
  nl <- length(idx1)
  Gamma <- matrix(0, nrow = dims[1], ncol = dims[2])
  SSIc <- array(dim = c(dims[1], dims[2], nl))
  for(i in 1:nl){
    SSIc[, , i] <- SSI[, , idx1[i]] %*% SSI[, , idx2[i]]
    Gamma <- Gamma + SSIc[, , i]
  }
  Gamma <- Gamma / nl
  if(standardize){
    cora <- Gsqrtinv %*% Gamma %*% Gsqrtinv
  } else {
    cora <- Gamma
  }
  cora <- (cora + t(cora)) / 2
  return(cora)
}
