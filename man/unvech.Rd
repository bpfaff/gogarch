\name{unvech}

\encoding{latin1}

\alias{unvech}

\title{
  Returns a symmetric matrix from a vector
}

\description{
  This function returns the symmetric matrix \eqn{X} from a vector that
  resulted from \eqn{v = vech(X)}.
}

\usage{
unvech(v)
}

\arguments{
  \item{v}{Vector, numeric.}
}

\details{
  The vector \code{v} must have length equal to \eqn{m * (m + 1) / 2},
  whereby \eqn{m} is a dimension of the symmetric matrix \eqn{X_{m
  \times m}}.
}

\value{
  \item{X}{Matrix, symmetric of order \eqn{m \times m}.}
}

\author{
  Bernhard Pfaff
}

\seealso{
  \code{\link[fBasics]{vec}}
}

\examples{
v <- c(1, 2, 3, 4, 5, 6)
unvech(v)
}

\keyword{algebra}
\concept{vech}
\concept{vech invert}
