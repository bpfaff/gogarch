gogarch <- function(data, formula, scale = FALSE, method = c("ml", "nls"), initial = NULL, garchlist = list(init.rec = "mci", delta = 2, skew = 1, shape = 4, cond.dist = "norm", include.mean = FALSE, include.delta = NULL, include.skew = NULL, include.shape = NULL, leverage = NULL, trace = FALSE, algorithm = "nlminb", hessian = "ropt", control = list(), title = NULL, description = NULL), ...){
  method <- match.arg(method)
  Call <- match.call()
  d <- ncol(data)
  gini <- goinit(X = data, garchf = formula, scale = scale)
  gomod <- new("GoGARCH", gini)
  if(method == "ml"){
    if(is.null(initial)){
      l <- d * (d - 1)/2
      initial <- seq(3.0, 0.1, length.out = l)
    } else {
      l <- length(initial)
      if (l != d * (d - 1)/2) {
        stop(paste("\nLength of initial vector does not match implied dimension of orthogonal matrix.\n", "It should have length: ", d * (d - 1)/2, sep = ""))
      }
    }
    goestml <- new("Goestml", gomod)
    gogarch <- goest(object = goestml, initial = initial, garchlist = garchlist, ...)
  }
  if(method == "nls"){
    if(is.null(initial)){
      l <- d * (d + 1)/2
      initial <- rep(0.1, l)
    } else {
      l <- length(initial)
      if (l != d * (d + 1)/2) {
        stop(paste("\nLength of initial vector does not match length of vech(B).\n", "It should have length: ", d * (d + 1)/2, sep = ""))
      }
    }
    goestnls <- new("Goestnls", gomod)
    gogarch <- goest(object = goestnls, initial = initial, garchlist = garchlist, ...)
  }  
  gogarch@CALL <- Call
  gogarch@name <- deparse(substitute(data))
  return(gogarch)
}
