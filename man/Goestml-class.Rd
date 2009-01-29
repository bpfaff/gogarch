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
\alias{logLik,Goestml-method}
\alias{residuals,Goestml-method}
\alias{resid,Goestml-method}
\alias{show,Goestml-method}

\title{Class "Goestml": GO-GARCH  models estimated by Maximum-Likelihood}

\description{This class contains the \code{GoGARCH} class and has the
  outcome of \code{optim} as an additional slot.
}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("Goestml", ...)}.
}

\section{Slots}{
	 \describe{
    \item{\code{opt}:}{Object of class \code{"list"}: List returned by
	 \code{optim}.} 
    \item{\code{Z}:}{Object of class \code{"matrix"}: Transformation matrix.}
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
  }
}

\section{Extends}{
Class \code{"\linkS4class{GoGARCH}"}, directly.
Class \code{"\linkS4class{Goinit}"}, by class "GoGARCH", distance 2.
}

\section{Methods}{
  \describe{
    \item{angles}{Returns the Eulerian angles.}
    \item{cvar}{Returns the conditional variances.}
    \item{ccor}{Returns the conditional correlations.}    
    \item{ccov}{Returns the conditional covariances.}    
    \item{coef}{Returns the coeffiecients of the component GARCH models.}
    \item{converged}{Returns the convergence codes of the component
  GARCH models.}  
    \item{goest}{ML-Estimation of Go-GARCH models.}    
    \item{logLik}{Returns the value of the log-Likelihood function.}
    \item{residuals}{Returns the residuals of the component GARCH models.}
    \item{show}{show-method for objects of class \code{Goestml}.}
  }
}

\author{
  Bernhard Pfaff
}


\seealso{
  \code{\linkS4class{GoGARCH}}, \code{\linkS4class{Goinit}},
  \code{\link{goest-methods}}
}

\keyword{classes}
