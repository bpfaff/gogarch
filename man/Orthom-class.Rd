\name{Orthom-class}

\docType{class}

\alias{Orthom-class}
\alias{M}
\alias{M,Orthom-method}
\alias{print,Orthom-method}
\alias{show,Orthom-method}
\alias{t,Orthom-method}

\encoding{latin1}

\title{Class "Orthom": Orthogonal matrices}

\description{
  This class defines an orthogonal matrix, which is characterized by
  \eqn{det(M) = 1} and \eqn{M M' = I}.
}

\section{Objects from the Class}{
  Objects can be created by calls of the form \code{new("Orthom",
  ...)}. In addition the function \code{UprodR} returns an object of
  formal class \code{Orthom}.
}

\section{Slots}{
  \describe{
    \item{\code{M}:}{Object of class \code{"matrix"}.}
  }
}

\section{Methods}{
  \describe{
    \item{M}{Returns the slot \code{M} of class \code{Orthom}.}
    \item{print}{print-method for objects of class \code{Orthom}.}
    \item{show}{show-method for objects of class \code{Orthom}.}
    \item{t}{Transpose of \code{object@M}.}
  }
}

\author{
  Bernhard Pfaff
}

\note{Objects are validated by \code{validOrthomObject()}. This function
  is utilised by \code{validObject()}.}

\seealso{
   \code{\link{UprodR}}, \code{\link{validOrthomObject}}
}

\examples{
showClass("Orthom")
}

\keyword{classes}
\keyword{algebra}
\concept{Orthogonal Matrix}
