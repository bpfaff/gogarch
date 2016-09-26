\name{UprodR}

\encoding{latin1}

\alias{UprodR}

\title{
Creation of an orthogonal matrix
}

\description{
This function returns an orthogonal matrix which results of the
matrix products of rotation matrices.
}

\usage{
UprodR(theta)
}

\arguments{
  \item{theta}{Vector, of angles of the rotation matrices.}
}

\details{
  The length of \code{theta} must be equal to \eqn{m * (m - 1) / 2},
  where \eqn{m} is the dimension of the orthogonal matrix. The elements
  of \code{theta} must lie in the interval \eqn{[0, \pi/2)}.
}
\value{
  \item{result}{Object of class \code{Orthom}.}
}
\references{
  Vilenkin, N. Ja. (1968), Special Functions and the Theory of Group
  Representations, Translations of Mathematical Monographs, \bold{22},
  American Math. Soc., Providence, Rhode Island, USA.
}

\author{
  Bernhard Pfaff
}
\seealso{
  \code{\link{Rd2}}, \code{\linkS4class{Orthom}}
}
\examples{
theta <- c(pi/3, pi/5, pi/7)
U <- UprodR(theta)
U
}
\keyword{algebra}
\concept{Orthogonal Matrix}