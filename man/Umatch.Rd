\name{Umatch}

\encoding{latin1}

\alias{Umatch}

\title{
  Matching of Orthogonal Matrices for Cayley transforms
}

\description{
  This function matches an orthogonal matrix to the importance of the
  columns of the matrix to which it should be matched.
}

\usage{
Umatch(from, to)
}

\arguments{
  \item{from}{Matrix: orthogonal}
  \item{to}{Matrix: orthogonal}
}

\value{
  \item{mat}{Matched matrix.}
}

\references{
  Boswijk, H. Peter and van der Weide, Roy (2009), Method of Moments
  Estimation of GO-GARCH Models, \emph{Working Paper}, University of
  Amsterdam, Tinbergen Institute and World Bank.

  Liebeck, H. and Osborne, A. (1991), The Generation of All Rational
  Orthogonal Matrices, \emph{The American Mathematical Monthly},
  \bold{98 (2)} (Feb. 1991), 131 -- 133.
}

\author{
  Bernhard Pfaff
}

\seealso{
  \code{\link{gogarch}}
}

\keyword{models}
