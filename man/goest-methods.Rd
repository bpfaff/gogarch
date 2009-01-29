\name{goest-methods}

\encoding{latin1}

\docType{methods}

\alias{goest-methods}

\alias{goest,Goestml,numeric,list-method}

\alias{goest}

\title{Methods for Function goest}

\description{
  These are methods for estimating GO-GARCH models. Currently only a
  method for estimating GO-GARCH models by Maximum-Likelihood is implemented.
}

\section{Methods}{
  \describe{
    \item{goest}{\code{signature(object = "Goestml", initial = "numeric", garchlist = "list")}}
  }
}

\details{
  The starting values for the Euler angles are provided in the argument
  \code{initial} and the list \code{garchlist} contains the elements
  that are passed \code{garchFit}. 
}

\author{
  Bernhard Pfaff
}

\seealso{
  \code{\link[fGarch]{garchFit}}, \code{\linkS4class{Goestml}}
}

\keyword{methods}
