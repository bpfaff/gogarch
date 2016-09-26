\name{Goestml-class}

\docType{class}

\encoding{latin1}

\alias{Goestml-class}
\alias{angles}
\alias{cvar}
\alias{ccov}
\alias{ccor}
\alias{converged}
\alias{logLik}
\alias{angles,Goestml-method}
\alias{cvar,Goestml-method}
\alias{ccor,Goestml-method}
\alias{ccov,Goestml-method}
\alias{coef,Goestml-method}
\alias{converged,Goestml-method}
\alias{formula,Goestml-method}
\alias{goest,Goestml-method}
\alias{logLik,Goestml-method}
\alias{plot,Goestml,missing-method}
\alias{predict,Goestml-method}
\alias{residuals,Goestml-method}
\alias{resid,Goestml-method}
\alias{show,Goestml-method}
\alias{summary,Goestml-method}
\alias{update,Goestml-method}

\title{Class "Goestml": GO-GARCH  models estimated by Maximum-Likelihood}

\description{This class contains the \code{GoGARCH} class and has the
  outcome of \code{nlminb} as an additional slot.
}

\section{Objects from the Class}{
  Objects can be created by calls of the form \code{new("Goestml",
  ...)}, or with the function \code{gogarch} whereby \code{method =
  "ml"} has been set.
}

\section{Slots}{
	 \describe{
    \item{\code{opt}:}{Object of class \code{"list"}: List returned by
	 \code{nlminb}.}
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
    \item{angles}{Returns the Eulerian angles.}
    \item{cvar}{Returns the conditional variances as object with class attribute
  \code{"mts" "ts"}.}
    \item{ccov}{Returns the conditional co-variances as object with
  class attribute \code{"mts" "ts"}.}
    \item{ccor}{Returns the conditional correlations as object with class
  attribute \code{"mts" "ts"}.}
    \item{coef}{Returns the coeffiecients of the component GARCH models.}
    \item{converged}{Returns the convergence codes of the component
      GARCH models.}
    \item{formula}{Returns the formula for the component GARCH models.}
    \item{goest}{ML-Estimation of Go-GARCH models.}
    \item{logLik}{Returns the value of the log-Likelihood function.}
    \item{plot}{Plotting of the conditional correlations.}
    \item{predict}{Returns the conditional covariances and mean
  forecasts and the forecasts of the component GARCH models, object is
  of class \code{Gopredict}.}
    \item{residuals}{Returns the residuals of the Go-GARCH model as
  object with class attribute \code{"mts" "ts"}.}
    \item{resid}{Returns the residuals of the Go-GARCH model as
      object with class attribute \code{"mts" "ts"}.}
    \item{show}{show-method for objects of class \code{Goestml}.}
    \item{summary}{summary-method for objects of class \code{Goestml},
  object is of class \code{Gosum}.}
    \item{update}{Updates an object of class \code{Goestml}.}
  }
}

\author{
  Bernhard Pfaff
}


\seealso{
  \code{\linkS4class{GoGARCH}}, \code{\linkS4class{Goinit}},
  \code{\linkS4class{Gosum}}, \code{\linkS4class{Gopredict}},
  \code{\link{goest-methods}}
}

\keyword{classes}
