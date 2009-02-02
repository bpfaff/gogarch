setMethod(f = "summary", signature(object = "GoGARCH"), definition = function(object){
  name <- as.character(object@CALL$data)
  method <- object@estby
  model <- object@garchf
  garchc <- lapply(object@models, function(x) x@fit$matcoef)
  ynames <- paste("y", 1:ncol(object@X), sep = "")
  names(garchc) <- paste("Component GARCH model of", ynames)
  garchc <- garchc
  Zinv <- solve(object@Z)
  gosum <- new("Gosum", name = name, method = method, model = model, garchc = garchc, Zinv = Zinv)
  return(gosum)
})
