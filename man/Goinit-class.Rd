\name{Goinit-class}

\docType{class}

\alias{Goinit-class}
\alias{show,Goinit-method}

\encoding{latin1}

\title{Class "Goinit": Initialisation of GO-GARCH models}

\description{This class defines the required slots for estimating
  GO-GARCH models.}

\section{Objects from the Class}{
  Objects can be created by calls of the form \code{new("Goinit", ...)},
  or more conveniently by \code{goinit()}. 
}

\section{Slots}{
	 \describe{
    \item{\code{X}:}{Object of class \code{"matrix"}: The data matrix.}
    \item{\code{V}:}{Object of class \code{"matrix"}: Covariance matrix
	 of \code{X}.}
    \item{\code{P}:}{Object of class \code{"matrix"}: Left singular
	 values of Var/Cov matrix of \code{X}.}
    \item{\code{Dsqr}:}{Object of class \code{"matrix"}: Square roots of
      eigenvalues on diagonal, else zero.}
    \item{\code{garchf}:}{Object of class \code{"formula"}: Garch
	 formula used for uncorrelated component GARCH models.}
    \item{\code{name}:}{Object of class \code{"character"}: The name of
	 the original data object.}
  }
}

\section{Methods}{
  \describe{
    \item{show}{Prints the slots, whereby for \code{X} only the head is
  displayed.} 
	 }
}

\author{Bernhard Pfaff}

\seealso{
  \code{\link[fGarch]{garchFit}}, \code{\link{goinit}} 
}

\examples{
showClass("Goinit")
}

\keyword{classes}
