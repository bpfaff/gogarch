\name{cora}

\encoding{latin1}

\alias{cora}

\title{
  Autocorrelations of a Matrix Process
}

\description{
  This function computes the autocorrelation matrix for a given lag. For
  instance, it is used for estimating GO-GARCH models whence the method
  of moments is utilized.   
}

\usage{
cora(SSI, lag = 1, standardize = TRUE)
}
\arguments{
  \item{SSI}{Array with dimension \code{dim = c(m, m, n)}}
  \item{lag}{Integer, the lag for which the autocorrelation is
    computed.}
  \item{standardize}{Logical, if \code{TRUE} (the default), the
    autocorrelation matrix is computed, otherwise the autocovariance
    matrix.}   
}

\details{
  This function computes the autocorrelation matrix according to:

  \deqn{
    \hat{\Gamma}_k (s) = \frac{1}{n} \sum_{t = k + 1}^n S_t S_{t-k}
  }
  \deqn{
    \hat{\Phi}_k (s) = \hat{\Gamma}_0 (s)^{-1/2} \hat{\Gamma}_k (s)
    \hat{\Gamma}_0 (s)^{-1/2}
  }
  
  It is computationally assured that \eqn{\hat{\Phi}_k (s)} is symmetric
  by setting it equal to: \eqn{\hat{\Phi}_k (s) = \frac{1}{2}(\hat{\Phi}_k (s) +
  \hat{\Phi}_k (s)')}. The standardization matrix \eqn{\hat{\Gamma}_0
  (s)^{-1/2}} is derived from the singular value decomposition of the
  co-variance matrix at lag zero.   
}

\value{
  \item{cora}{Matrix with dimension \code{dim = c(m, m)}.}
}

\references{
    Boswijk, H. Peter and van der Weide, Roy (2009), Method of Moments
    Estimation of GO-GARCH Models, \emph{Working Paper}, University of
    Amsterdam, Tinbergen Institute and World Bank.
}

\author{
  Bernhard Pfaff
}

\seealso{
  \code{\link{gogarch}}
}

\keyword{models}
