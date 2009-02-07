\name{Goestmm-class}

\encoding{latin1}

\docType{class}

\alias{Goestmm-class}
\alias{show,Goestmm-method}
\alias{goest,Goestmm-method}

\title{Class "Goestmm": Go-GARCH models estimated by Methods of Moments}

\description{
  This class contains the \code{GoGARCH} class and has the weights
  vector and the matched orthogonal matrices \eqn{U} as additional
  slots.
}

\section{Objects from the Class}{
  Objects can be created by calls of the form \code{new("Goestmm", ...)},
  or with the function \code{gogarch}. 
}

\section{Slots}{
  \describe{
    \item{\code{weights}:}{Object of class \code{"numeric"}: Weights for
  aggregating the matched orthogonal matrices \eqn{U}.}
    \item{\code{Umatched}:}{Object of class \code{"list"}: List of
  matched orthogonal matrices \eqn{U}.}
    \item{\code{Z}:}{Object of class \code{"matrix"}: Transformation matrix.}
    \item{\code{U}:}{Object of class \code{"matrix"}: Orthogonal matrix.}
    \item{\code{Y}:}{Object of class \code{"matrix"}: Extracted
	 component matrix.}
    \item{\code{H}:}{Object of class \code{"list"}: List of conditional
	 variance/covariance matrices.}
    \item{\code{models}:}{Object of class \code{"list"}: List of
	 univariate GARCH model fits.}
    \item{\code{estby}:}{Object of class \code{"character"}: Estimation method.}
    \item{\code{X}:}{Object of class \code{"matrix"}: The data matrix.}
    \item{\code{V}:}{Object of class \code{"matrix"}: Covariance matrix
	 of \code{X}.}
    \item{\code{P}:}{Object of class \code{"matrix"}: Left singular
	 values of Var/Cov matrix of \code{X}.}
    \item{\code{Dsqr}:}{Object of class \code{"matrix"}: Square roots of
      eigenvalues on diagonal, else zero.}
    \item{\code{garchf}:}{Object of class \code{"formula"}: Garch
	 formula used for uncorrelated component GARCH models.}
    \item{\code{name}:}{Object of class \code{"character"}: The name of
      the original data object.}
  }
}

\section{Extends}{
Class \code{"\linkS4class{GoGARCH}"}, directly.
Class \code{"\linkS4class{Goinit}"}, by class "GoGARCH", distance 2.
}

\section{Methods}{
  \describe{
    \item{goest}{Methods of moments estimation of Go-GARCH models.}
    \item{show}{show-method for objects of class \code{Goestnls}.}
  }
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
  \code{\linkS4class{GoGARCH}}, \code{\linkS4class{Goinit}},
  \code{\linkS4class{Gosum}}, \code{\linkS4class{Gopredict}},
  \code{\link{goest-methods}}, \code{\link{gogarch}},
  \code{\link{Umatch}}
}

\keyword{classes}
