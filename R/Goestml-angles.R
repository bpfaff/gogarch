setMethod(f = "angles", signature = "Goestml", definition = function(object){
  angles <- object@opt$par
  names(angles) <- paste("angle", seq(along.with = angles), sep = "")
  return(angles)
})
