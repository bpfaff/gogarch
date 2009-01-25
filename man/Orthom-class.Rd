\name{Orthom-class}

\docType{class}

\alias{Orthom-class}
\alias{show,Orthom-method}
\alias{print,Orthom-method}

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
    \S4method{show}{show-method for objects of class \code{Orthom}.}
    \S4method{print}{print-method for objects of class \code{Orthom}.}
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
