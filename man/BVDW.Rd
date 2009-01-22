\name{BVDW}

\alias{BVDW}

\encoding{latin1}

\docType{data}

\title{
Dow Jones Industrial Average and Nasdaq stock indices
}

\description{
  Levels of the Dow Jones Industrial Average and NASDAQ stock indices
  for the period 03/23/1990 until 03/23/2000.
}

\usage{data(BVDW)}
\format{
  A data frame with 2610 observations on the following 3 variables.
  \describe{
    \item{\code{Date}}{Date in the format YYYYMMDD.}
    \item{\code{DJIA}}{Level of the DIJA.}
    \item{\code{NASDAQ}}{Level of the NASDAQ.}
  }
}

\details{
  This data set has been utilized in the source below and was kindly
  provided by Roy van der Weide. 
}

\source{
  Boswijk, H. Peter and van der Weide, Roy (2006), Wake me up before you
  GO-GARCH, \emph{Tinbergen Institute Discussion Paper}, \bold{TI
    2006-079/4}, University of Amsterdam and Tinbergen Institute.
}

\references{
  \url{http://www.nasdaq.com}, \url{http://www.djindexes.com}
}

\seealso{
  \code{\link{VDW}}
}

\examples{
data(BVDW)
str(BVDW)
}

\keyword{datasets}
