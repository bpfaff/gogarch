\name{VDW}

\alias{VDW}

\encoding{latin1}

\docType{data}

\title{
  Dow Jones Industrial Average and Nasdaq stock indices
}

\description{
  The daily (log) returns of the Dow Jones Industrial Average and the
  NASDAQ composite, respectively. The daily observations start at the
  first of January, 1990, and end in October 2001.
}

\usage{data(VDW)}

\format{
  A data frame with 3082 observations on the following 2 variables.
  \describe{
    \item{\code{DJIA}}{Log-return of Dow Jones Industrial Average.}
    \item{\code{NASDAQ}}{Log-return of NASDAQ.}
  }
}

\details{
  This data set has been utilized in the source below and can be
  downloaded from the web-site of the \emph{Journal of Applied
    Econometrics} (see link below). 
}

\source{
  Van der Weide, Roy (2002), GO-GARCH: A Multivariate Generalized
  Orthogonal GARCH Model, \emph{Journal of Applied Econometrics},
  \bold{17(5)}, 549 -- 564.
}

\references{
  \url{http://www.nasdaq.com}\cr
  \url{http://www.djindexes.com}\cr 
  \url{http://qed.econ.queensu.ca/jae/2002-v17.5/van_der_weide}
}

\seealso{
  \code{\link{BVDW}}
}

\examples{
data(VDW)
str(VDW)
}

\keyword{datasets}
