\name{Gopredict-class}

\encoding{latin1}

\docType{class}

\alias{Gopredict-class}
\alias{ccor,Gopredict-method}
\alias{ccov,Gopredict-method}
\alias{cvar,Gopredict-method}
\alias{show,Gopredict-method}

\title{Class "Gopredict": Prediction of GO-GARCH Models}

\description{
  This class defines the slots for forecasts from a GO-GARCH model.
}

\section{Objects from the Class}{
  Objects can be created by calls of the form \code{new("Gopredict",
  ...)}, or with the method \code{predict} of formal class objects
  \code{GoGARCH} and \code{Goestml}.   
}

\section{Slots}{
	 \describe{
    \item{\code{Hf}:}{Object of class \code{"list"}: The forecasted
	 conditional covariances.}
    \item{\code{Xf}:}{Object of class \code{"matrix"}: The transformed
	 forecasts of the component GARCH mean models.}
    \item{\code{CGARCHF}:}{Object of class \code{"list"}: The original
	 forecasts of the component GARCH models.}
  }
}

\section{Methods}{
  \describe{
    \item{ccor}{Returns the forecasted conditional correlations.}
    \item{ccov}{Returns the forecasted conditional co-variances.}
    \item{cvar}{Returns the forecasted conditional variances.}
    \item{show}{show-method for objects of class \code{Gopredict}.}
  }
}

\author{
  Bernhard Pfaff
}

\note{
  In case more than 10 forecasts steps are computed, the
  \code{show}-method displays only the \code{head} of the returned
  objects. Furthermore, the \code{show}-method displays the forecasted
  conditional variances only. The forecasted conditional co-variances
  and/or the forecasted conditional correlations can be retrieved with
  the methods \code{ccov} or \code{ccor}, respectively.
}


\seealso{
  \code{\linkS4class{GoGARCH}}, \code{\linkS4class{Goestml}} 
}

\keyword{classes}
