\name{validOrthomObject}

\alias{validOrthomObject}

\encoding{latin1}

\title{
Validation function for objects of class Orthom
}
\description{
  This function validates objects of class \code{Orthom}.
}
\usage{
validOrthomObject(object)
}
\arguments{
  \item{object}{Object of class \code{Orthom}.}
}

\details{
  This function is utilized by \code{validObject()}. It is tested
  whether \code{object@M} is a square matrix, has \eqn{det(M) = 1} and
  \eqn{MM' = I}. 
}

\value{
  \item{TRUE}{Logical, \code{TRUE} if the object passes the validation,
  otherwise an informative error message is returned.}
}

\author{
  Bernhard Pfaff
}

\seealso{
  \code{\linkS4class{Orthom}}
}

\examples{
theta <- c(pi/3, pi/5, pi/7)
U <- UprodR(theta)
validObject(U)
}

\keyword{utilities}
