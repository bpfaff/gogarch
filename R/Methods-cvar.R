setMethod(f = "cvar", signature(object = "GoGARCH"), definition = function(object){
  m <- ncol(object@X)
  n <- nrow(object@X)
  cvar <- matrix(c(unlist(lapply(object@H, function(x) diag(x)))), ncol = m, nrow = n, byrow = TRUE)
  colnames(cvar) <- paste("V.", colnames(object@X), sep = "")
  rownames(cvar) <- rownames(object@X)
  return(cvar)
})

setMethod(f = "cvar", signature(object = "Goestica"), definition = function(object){
  cvar(as(object, "GoGARCH"))
})

setMethod(f = "cvar", signature(object = "Goestmm"), definition = function(object){
  cvar(as(object, "GoGARCH"))
})

setMethod(f = "cvar", signature(object = "Goestnls"), definition = function(object){
  cvar(as(object, "GoGARCH"))
})

setMethod(f = "cvar", signature(object = "Goestml"), definition = function(object){
  cvar(as(object, "GoGARCH"))
})

setMethod(f = "cvar", signature(object = "Gopredict"), definition = function(object){
  m <- ncol(object@Xf)
  n <- nrow(object@Xf)
  cvar <- matrix(c(unlist(lapply(object@Hf, function(x) diag(x)))), ncol = m, nrow = n, byrow = TRUE)
  colnames(cvar) <- paste("V.", colnames(object@Xf), sep = "")
  rownames(cvar) <- rownames(object@Xf)
  return(cvar)
})

