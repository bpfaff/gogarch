\name{Goestnls-class}

\encoding{latin1}

\docType{class}

\alias{Goestnls-class}
\alias{cvar,Goestnls-method}
\alias{ccov,Goestnls-method}
\alias{ccor,Goestnls-method}
\alias{coef,Goestnls-method}
\alias{converged,Goestnls-method}
\alias{formula,Goestnls-method}
\alias{goest,Goestnls-method}
\alias{plot,Goestnls,missing-method}
\alias{predict,Goestnls-method}
\alias{residuals,Goestnls-method}
\alias{resid,Goestnls-method}
\alias{show,Goestnls-method}
\alias{summary,Goestnls-method}
\alias{update,Goestnls-method}

\title{Class "Goestnls": GO-GARCH models estimated by Non-linear Least-Squares}

\description{
  This class contains the \code{GoGARCH} class and has the
  outcome of \code{optim} as an additional slot.
}

\section{Objects from the Class}{
  Objects can be created by calls of the form \code{new("Goestnls", ...)}, 
  or with the function \code{gogarch} whereby \code{method = "nls"} has
  been set. 
}

\section{Slots}{
  \describe{
    \item{\code{nls}:}{Object of class \code{"list"}: List returned by
	 \code{optim}.}
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
    \item{goest}{NLS-Estimation of Go-GARCH models.}
    \item{plot}{Plotting of the conditional correlations.}
    \item{predict}{Returns the conditional covariances and mean
      forecasts and the forecasts of the component GARCH models, object
      is of class \code{Gopredict}.}
    \item{residuals}{Returns the residuals of the Go-GARCH model as
  object with class attribute \code{"mts" "ts"}.}
    \item{resid}{Returns the residuals of the Go-GARCH model as
      object with class attribute \code{"mts" "ts"}.}
    \item{show}{show-method for objects of class \code{Goestnls}.}
    \item{summary}{summary-method for objects of class \code{GoGARCH},
  object is of class \code{Gosum}.}
    \item{update}{Updates an object of class \code{GoGARCH}.}
  }
}

\author{
  Bernhard Pfaff
}

\seealso{
  \code{\linkS4class{GoGARCH}}, \code{\linkS4class{Goinit}},
  \code{\linkS4class{Gosum}}, \code{\linkS4class{Gopredict}},
  \code{\link{goest-methods}}, \code{\link{gogarch}}
}


\keyword{classes}
