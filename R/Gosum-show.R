setMethod(f = "show", signature(object = "Gosum"), definition = function(object){
  title <- "*** Summary of GO-GARCH Model ***"
  stars <- paste(rep("*", nchar(title)), collapse = "")
  cat("\n")
  cat(paste(stars, "\n"))
  cat(paste(title, "\n"))
  cat(paste(stars, "\n"))
  cat("\n")
  cat(paste("Used object:", object@name))
  cat("\n")  
  cat("\n")
  cat(paste("Components estimated by:", object@method))
  cat("\n")
  cat(paste("Formula for component GARCH models:", paste(as.character(object@model), collapse = " "), "\n"))
  cat("\n")  
  if(length(object@Zinv) != 0){
    cat("The Inverse of the Linar Map Z:\n")
    print(object@Zinv)
    cat("\n")
  }
  cat("\n")
  title2 <- "*** Estimated Component GARCH models ***"
  stars2 <- paste(rep("*", nchar(title2)), collapse = "")
  cat("\n")
  cat(paste(stars2, "\n"))
  cat(paste(title2, "\n"))
  cat(paste(stars2, "\n"))
  cnames <- names(object@garchc)
  n <- length(object@garchc)
  for(i in 1:n){
    cat("\n")
    cat(paste(cnames[i], "\n"))
    print(object@garchc[[i]])  
  }
  invisible(object)
})

