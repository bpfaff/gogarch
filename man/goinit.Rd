\name{goinit}

\alias{goinit}

\encoding{latin1}

\title{
  Constructor function for objects of class "Goinit"
}
\description{
  This function can be utilized to create objects of class
  \code{Goinit}. These objects are the starting point for estimating
  GO-GARCH models.
}
\usage{
goinit(X, garchf = ~garch(1, 1), scale = FALSE)
}

\arguments{
  \item{X}{Matrix: the data matrix.}
  \item{garchf}{Formula: A formula object that will be used in the GARCH
  models of the uncorrelated components.}
  \item{scale}{Logical, if \code{TRUE} the data \code{X} will be scaled,
  the default value is \code{FALSE} for no scaling of the data.}
}

\details{
  This function computes the variance/covariance matrix of
  \code{X}. Next the singular value decomposition is applied and the
  projection matrix as well as the diagonal matrix with the square roots
  of the eigen values are computed.
}

\value{
  An object of class \code{Goinit}.
}

\author{
  Bernhard Pfaff
}

\seealso{
  \code{\linkS4class{Goinit}}
}

\examples{
\dontrun{
library(vars)
data(VDW)
var1 <- VAR(VDW, p = 1, type = "const")
resid <- resid(var1)
goinit(resid, scale = TRUE)
}
}

\keyword{models}
\concept{GO-GARCH}
\concept{SVD}
\concept{Singular Value Decomposition}
