setMethod(f = "show", signature = "Gopredict", function(object){
  title <- "*** Forecasts of GO-GARCH Model ***"
  stars <- paste(rep("*", nchar(title)), collapse = "")
  cat("\n")
  cat(paste(stars, "\n"))
  cat(paste(title, "\n"))
  cat(paste(stars, "\n"))  
  cat("\n")
  if(nrow(object@Xf) <= 10){
    cat("Conditional variances:\n")
    print(cvar(object))
    cat("\n")
    cat("\n")
    cat("Forecasts of Mean Equation:\n")
    print(object@Xf)
    cat("\n")    
  } else {
    cat("Head of conditional variances:\n")
    print(head(cvar(object)))
    cat("\n")
    cat("\n")
    cat("Forecasts of Mean Equation:\n")
    print(head(object@Xf))
    cat("\n")    
  } 
})
