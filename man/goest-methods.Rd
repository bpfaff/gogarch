\name{goest-methods}

\encoding{latin1}

\docType{methods}

\alias{goest-methods}

\alias{goest}

\title{Methods for Function goest}

\description{
  These are methods for estimating GO-GARCH models. Currently only a
  method for estimating GO-GARCH models by Maximum-Likelihood is implemented.
}

\section{Methods}{
  \describe{
    \item{goest}{\code{signature(object = "Goestica")}}
    \item{goest}{\code{signature(object = "Goestmm")}}
    \item{goest}{\code{signature(object = "Goestml")}}
    \item{goest}{\code{signature(object = "Goestnls")}}
  }
}

\details{
  The declared estimation methods are called from function
  \code{gogarch}.  
}

\author{
  Bernhard Pfaff
}

\seealso{
  \code{\link[fGarch]{garchFit}}, \code{\linkS4class{Goestica}},
  \code{\linkS4class{Goestml}}, \code{\linkS4class{Goestnls}},
  \code{\linkS4class{Goestmm}}, \code{\link{gogarch}}
}

\keyword{methods}
