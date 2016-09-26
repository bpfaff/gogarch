\name{GoGARCH-class}

\docType{class}

\encoding{latin1}

\alias{GoGARCH-class}
\alias{cvar,GoGARCH-method}
\alias{ccov,GoGARCH-method}
\alias{ccor,GoGARCH-method}
\alias{coef,GoGARCH-method}
\alias{converged,GoGARCH-method}
\alias{formula,GoGARCH-method}
\alias{plot,GoGARCH,missing-method}
\alias{predict,GoGARCH-method}
\alias{residuals,GoGARCH-method}
\alias{resid,GoGARCH-method}
\alias{show,GoGARCH-method}
\alias{summary,GoGARCH-method}
\alias{update,GoGARCH-method}

\title{Class "GoGARCH": Estimated GO-GARCH Models}

\description{
  This class defines the slots for estimated GO-GARCH models. It
  contains the class \code{Goinit}.
}

\section{Objects from the Class}{
  Objects can be created by calls of the form \code{new("GoGARCH", ...)}.
}

\section{Slots}{
	 \describe{
    \item{\code{Z}:}{Object of class \code{"matrix"}: Transformation matrix.}
    \item{\code{U}:}{Object of class \code{"Orthom"}: Orthonormal matrix.}
    \item{\code{Y}:}{Object of class \code{"matrix"}: Extracted
	 component matrix.}
    \item{\code{H}:}{Object of class \code{"list"}: List of conditional
	 variance/covariance matrices.}
    \item{\code{models}:}{Object of class \code{"list"}: List of
	 univariate GARCH model fits.}
    \item{\code{estby}:}{Object of class \code{"character"}: Estimation method.}
    \item{\code{CALL}:}{Object of class \code{"call"}: Result of
	 \code{match.call} in generating function.}
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
Class \code{"\linkS4class{Goinit}"}, directly.
}

\section{Methods}{
  \describe{
    \item{cvar}{Returns the conditional variances as object with class attribute
  \code{"mts" "ts"}.}
    \item{ccov}{Returns the conditional co-variances as object with
  class attribute \code{"mts" "ts"}.}
    \item{ccor}{Returns the conditional correlationsas object with class
  attribute \code{"mts" "ts"}.}
    \item{coef}{Returns the coeffiecients of the component GARCH models.}
    \item{converged}{Returns the convergence codes of the component
  GARCH models.}
    \item{formula}{Returns the formula for the component GARCH models.}
    \item{plot}{Plotting of the conditional correlations.}
    \item{predict}{Returns the conditional covariances and mean
  forecasts and the forecasts of the component GARCH models, object is
  of class \code{Gopredict}.}
    \item{residuals}{Returns the residuals of the GO-GARCH model.}
    \item{show}{show-method for objects of class \code{GoGARCH}.}
    \item{summary}{summary-method for objects of class \code{GoGARCH},
  object is of class \code{Gosum}.}
    \item{update}{Updates an object of class \code{GoGARCH}.}
  }
}

\author{
  Bernhard Pfaff
}

\seealso{
  \code{\linkS4class{Goinit}}, \code{\linkS4class{Gosum}}, \code{\linkS4class{Gopredict}}
}

\keyword{classes}
