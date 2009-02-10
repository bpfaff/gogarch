gogarch <- function(data, formula, scale = FALSE, method = c("ica", "mm", "ml", "nls"), lag.max = 1, initial = NULL, garchlist = list(init.rec = "mci", delta = 2, skew = 1, shape = 4, cond.dist = "norm", include.mean = FALSE, include.delta = NULL, include.skew = NULL, include.shape = NULL, leverage = NULL, trace = FALSE, algorithm = "nlminb", hessian = "ropt", control = list(), title = NULL, description = NULL), ...){
  method <- match.arg(method)
  Call <- match.call()
  gini <- goinit(X = data, garchf = formula, scale = scale)
  gomod <- new("GoGARCH", gini)
  if(method == "ml"){
    goestml <- new("Goestml", gomod)
    gogarch <- goest(object = goestml, initial = initial, garchlist = garchlist, ...)
  }
  if(method == "nls"){
    goestnls <- new("Goestnls", gomod)
    gogarch <- goest(object = goestnls, initial = initial, garchlist = garchlist, ...)
  }
  if(method == "mm"){
    goestmm <- new("Goestmm", gomod)
    gogarch <- goest(object = goestmm, lag.max = lag.max, garchlist = garchlist, ...)
  }  
  if(method == "ica"){
    goestica <- new("Goestica", gomod)
    gogarch <- goest(object = goestica, initial = initial, garchlist = garchlist, ...)
  }
  gogarch@CALL <- Call
  gogarch@name <- deparse(substitute(data))
  return(gogarch)
}
