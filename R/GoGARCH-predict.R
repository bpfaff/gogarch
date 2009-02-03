setMethod(f = "predict", signature(object = "GoGARCH"), definition = function(object, n.ahead = 10, ...){
  n.ahead <- abs(as.integer(n.ahead))
  m <- ncol(object@X)
  n <- nrow(object@X)
  Z <- object@Z
  delta <- object@models[[1]]@fit$params$params["delta"]
  predictions <- lapply(object@models, predict, n.ahead = n.ahead)
  mean.pred.y <- matrix(unlist(lapply(predictions, function(x) x[, 1])), ncol = m)
  mean.pred.x <- mean.pred.y %*% Z
  rownames(mean.pred.x) <- seq(from = 1, to = n.ahead) + n
  colnames(mean.pred.x) <- paste(colnames(object@X), ".f", sep = "")
  h.pred.y <- matrix(unlist(lapply(predictions, function(x) x[, 3]^delta)), ncol = m)
  H.pred.y <- data.frame(t(h.pred.y))
  H.pred.x <- lapply(H.pred.y, function(x) Z %*% diag(x) %*% t(Z))
  names(H.pred.x) <- rownames(mean.pred.x)
  fcst <- new("Gopredict", Hf = H.pred.x, Xf = mean.pred.x, CGARCHF = predictions)
  return(fcst)
})
