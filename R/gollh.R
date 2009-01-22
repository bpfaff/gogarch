gollh <-
function(params, X, P, Dsqr, m, n, formula = ~ garch(1,1), garchlist = list(
         init.rec = "mci", delta = 2, skew = 1, shape = 4, cond.dist = "norm",
         include.mean = FALSE, include.delta = NULL, include.skew = NULL,
         include.shape = NULL, leverage = NULL, trace = FALSE,
         algorithm = "nlminb", hessian = "ropt", control = list(),
         title = NULL, description = NULL)){
  U <- UprodR(params)
  Z <- P %*% Dsqr %*% t(U)
  Zinv <- solve(Z)
  Y <- X %*% Zinv
  fitted <- apply(Y, 2, function(x) garchFit(formula = formula, data = x, include.mean = FALSE, trace = FALSE))
  H <- matrix(unlist(lapply(fitted, function(x) x@h.t)), ncol = m, nrow = n)
  Hinv <- 1.0 / H
  arg1 <- n * m * log(2 * pi)
  arg2 <- log(det(Z %*% t(Z))) * n
  arg3 <- sum(log(apply(H, 1, prod)))
  arg4 <- sum(rowSums(Y * Hinv * Y))
  ll <- -0.5 * (arg1 + arg2 + arg3 + arg4)
  negll <- -1.0 * ll
  return(negll)
}
