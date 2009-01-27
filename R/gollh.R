gollh <-
function(params, object, garchlist = list(
         init.rec = "mci", delta = 2, skew = 1, shape = 4, cond.dist = "norm",
         include.mean = FALSE, include.delta = NULL, include.skew = NULL,
         include.shape = NULL, leverage = NULL, trace = FALSE,
         algorithm = "nlminb", hessian = "ropt", control = list(),
         title = NULL, description = NULL)){
  gotheta <- gotheta(theta = params, object = object, garchlist = garchlist)
  m <- ncol(object@X)
  n <- nrow(object@X)
  H <- matrix(unlist(lapply(gotheta@models, function(x) x@h.t)), ncol = m, nrow = n)
  Hinv <- 1.0 / H
  arg1 <- n * m * log(2 * pi)
  arg2 <- log(det(gotheta@Z %*% t(gotheta@Z))) * n
  arg3 <- sum(log(apply(H, 1, prod)))
  arg4 <- sum(rowSums(gotheta@Y * Hinv * gotheta@Y))
  ll <- -0.5 * (arg1 + arg2 + arg3 + arg4)
  negll <- -1.0 * ll
  return(negll)
}
