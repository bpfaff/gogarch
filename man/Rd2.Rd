\name{Rd2}

\alias{Rd2}

\encoding{latin1}

\title{
  Rotation matrix, 2-dimensional
}

\description{
  Given an angle \eqn{\theta} whereby \eqn{\theta \in [0, \pi/2)} the
  function \code{Rd2} returns a 2-dimensional rotation matrix of Euler angles.
}

\usage{
Rd2(theta)
}

\arguments{
  \item{theta}{Numeric, angle in the interval \eqn{[0, \pi/2)}.}
}

\value{
  \item{R}{A 2-dimensional rotation matrix.}
}

\author{
  Bernhard Pfaff
}

\seealso{
  \code{\link{UprodR}}
}

\examples{
Rd2(pi/3)
}

\keyword{algebra}
\concept{Rotation Matrix}
\concept{Euler angle}
\concept{Euler angles}
