gogarch <- function(data, formula, scale = FALSE, method = c("ml"), initial = NULL, garchlist = list(init.rec = "mci", delta = 2, skew = 1, shape = 4, cond.dist = "norm", include.mean = FALSE, include.delta = NULL, include.skew = NULL, include.shape = NULL, leverage = NULL, trace = FALSE, algorithm = "nlminb", hessian = "ropt", control = list(), title = NULL, description = NULL), ...){
  method <- match.arg(method)
  Call <- match.call()
  d <- ncol(data)
  if(is.null(initial)){
    l <- d * (d - 1)/2
    initial <- seq(3.0, 0.1, length.out = l)
  } else {
    l <- length(initial)
    if (l != d * (d - 1)/2) {
      stop(paste("\nLength of initial vector does not match implied dimension of orthogonal matrix.\n", "It should have length: ", d, sep = ""))
    }
  }
  gini <- goinit(X = data, garchf = formula, scale = scale)
  gomod <- new("GoGARCH", gini)
  if(method == "ml"){
    goestml <- new("Goestml", gomod)
    gogarch <- goest(object = goestml, initial = initial, garchlist = garchlist, ...)
  }
  gogarch@CALL <- Call
  return(gogarch)
}
