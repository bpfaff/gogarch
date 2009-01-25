setMethod(f = "show", signature = "Goinit", function(object){
  title <- "*** Object of class Goinit ***"
  stars <- paste(rep("*", nchar(title)), collapse = "")
  cat("\n")
  cat(paste(stars, "\n"))
  cat(paste(title, "\n"))
  cat(paste(stars, "\n"))  
  cat("\n")
  if(length(object@X) != 0){
     cat("Head of data matrix X:\n")
     print(head(object@X), quote = FALSE)
     cat("\n")
  } 
  if(length(object@P) != 0){
    cat("Projection matrix P:\n")
    print(object@P, quote = FALSE)
    cat("\n")
  }
  if(length(object@Dsqr) != 0){
    cat("Square root of eigenvalues Dsqr:\n")
    print(formatC(object@Dsqr), quote = FALSE)
    cat("\n")
  }
  if(length(object@V) != 0){
    cat("Variance/Covariance matrix:\n")
    print(formatC(object@V), quote = FALSE)
    cat("\n")
  }
  cat(paste("Formula for component GARCH models:\n", paste(as.character(object@garchf), collapse = " "), "\n"))
}
)
