\name{gogarch}

\encoding{latin1}

\alias{gogarch}

\title{
  Specification and estimation of GO-GARCH models
}

\description{
  This function steers the specification and estimation of GO-GARCH
  models. 
}
\usage{
gogarch(data, formula, scale = FALSE, method = c("ica", "mm", "ml", "nls"),
  lag.max = 1, initial = NULL, garchlist = list(init.rec = "mci", delta
  = 2, skew = 1, shape = 4, cond.dist = "norm", include.mean = FALSE,
  include.delta = NULL, include.skew = NULL, include.shape = NULL,
  leverage = NULL, trace = FALSE, algorithm = "nlminb", hessian =
  "ropt", control = list(), title = NULL, description = NULL), ...) 
}

\arguments{
  \item{data}{Matrix: the original data set.}
  \item{formula}{Formula: valid formula for univariate GARCH models.}
  \item{scale}{Logical, if \code{TRUE} the data is scaled. The default
  is \code{scale = FALSE}.}
  \item{method}{Character: by fast ICA \code{method = "ica"} (the
  default), by Methods of Moments \code{method = "mm"} or by
  Maximum-Likelihood \code{method = "ml"} or by non-linear Least-Squares
  \code{method = "nls"}.} 
  \item{initial}{Numeric: starting values for optimization (used if
  \code{method = "ml"} or \code{method = "nls"} has been chosen (see
  Details).}
  \item{lag.max}{Integer: The number of used lags for computing the
    matched orthogonal matrices \eqn{U} (used if \code{method = "mm"}
    has been chosen).}
  \item{garchlist}{List: Elements are passed to \code{garchFit}.}
  \item{\dots}{Ellipsis argument: is passed to \code{nlminb}.}
}

\details{
If the argument \code{initial} is left \code{NULL}, the starting values
are computed according \code{seq(3.0, 0.1, length.out = l)}, whereby
\code{l} is the length of \code{initial} for \code{method = "ml"} and
are set to \code{rep(0.1, d}, whereby \code{} for \code{method =
  "nls"}. This length must be equal to \eqn{m * (m - 1)/2} for
estimation by Maximum-Likelihood and \eqn{m * (m + 1)/2} for estimation
by Non-linear Least-Squares, whereby \eqn{m} is the number of columns of
\code{data}.   
}

\value{
  Dependent on the chosen estimation method either an object of class
  \code{Goestmm} or \code{Goestml} or \code{Goestnls} is returned. All of
  these classes extend the \code{GoGARCH} class.
}

\references{
  Van der Weide, Roy (2002), GO-GARCH: A Multivariate Generalized
  Orthogonal GARCH Model, \emph{Journal of Applied Econometrics},
  \bold{17(5)}, 549 -- 564.
  
  Boswijk, H. Peter and van der Weide, Roy (2006), Wake me up before
  you GO-GARCH, \emph{Tinbergen Institute Discussion Paper}, \bold{TI
  2006-079/4}, University of Amsterdam and Tinbergen Institute. 

  Boswijk, H. Peter and van der Weide, Roy (2009), Method of Moments
  Estimation of GO-GARCH Models, \emph{Working Paper}, University of
  Amsterdam, Tinbergen Institute and World Bank.

  Broda, S.A. and Paolella, M.S. (2008): CHICAGO: A Fast and Accurate
  Method for Portfolio Risk Calculation, \emph{Swiss Finance Institute},
  Research Paper Series No. 08-08, Zuerich.  
}

\author{
  Bernhard Pfaff
}

\seealso{
  \code{\linkS4class{GoGARCH}}, \code{\linkS4class{Goestml}}, \code{\link{goest-methods}}
}
\examples{
\dontrun{
library(vars)
data(VDW)
var1 <- VAR(scale(VDW), p = 1, type = "const")
resid <- scale(residuals(var1))
gogarch(resid, ~garch(1, 1))
}
}

\keyword{models}
\concept{GARCH}
\concept{GO-GARCH}
\concept{Euler Angles}