\name{validGoinitObject}

\alias{validGoinitObject}

\encoding{latin1}

\title{
  Validation function for objects of class Goinit
}
\description{
  This function validates objects of class \code{Goinit}.
}
\usage{
validGoinitObject(object)
}

\arguments{
  \item{object}{Object of class \code{Goinit}.}
}

\details{
  This function is utilized by \code{validObject()}. It is tested
  whether \code{object@V}, \code{object@P}, \code{object@Dsqr} are
  square matrices; \code{object@V} coincides with the singular value
  decomposition. 
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  \item{TRUE}{Logical, \code{TRUE} if the object passes the validation,
  otherwise an informative error message is returned.}
}

\author{
  Bernhard Pfaff
}

\seealso{
  \code{\linkS4class{Goinit}}, \code{\link{goinit}} 
}

\examples{
data(VDW)
go <- goinit(VDW)
validObject(go)
}

\keyword{utilities}
