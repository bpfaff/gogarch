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
gogarch(data, formula, scale = FALSE, estby = c("ica", "mm", "ml", "nls"),
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
  \item{estby}{Character: by fast ICA \code{estby = "ica"} (the
  default), by Estbys of Moments \code{estby = "mm"} or by
  Maximum-Likelihood \code{estby = "ml"} or by non-linear Least-Squares
  \code{estby = "nls"}.} 
  \item{initial}{Numeric: starting values for optimization (used if
  \code{estby = "ml"} or \code{estby = "nls"} has been chosen (see
  Details).}
  \item{lag.max}{Integer: The number of used lags for computing the
    matched orthogonal matrices \eqn{U} (used if \code{estby = "mm"}
    has been chosen).}
  \item{garchlist}{List: Elements are passed to \code{garchFit}.}
  \item{\dots}{Ellipsis argument: is passed to the \code{goest} method
  (see details).} 
}

\details{
  The ellipsis argument is passed to the function \code{fastICA} if
  \code{estby = "ica"} has been set, or to \code{optim} if \code{estby
  = "nls"} is employed or to \code{nlminb} if the GO-GARCH model is
  estimated by maximum likelihood, \emph{i.e.}, \code{estby = "ml"}. It
  is not employed if the methods of moments estimator is chosen.\cr

  If the argument \code{initial} is left \code{NULL}, the starting
  values are computed according \code{seq(3.0, 0.1, length.out = l)},
  whereby \code{l} is the length of \code{initial} for \code{estby =
  "ml"} and are set to \code{rep(0.1, d}, whereby \code{} for
  \code{method = "nls"}. This length must be equal to \eqn{m * (m -
  1)/2} for estimation by Maximum-Likelihood and \eqn{m * (m + 1)/2} for
  estimation by non-linear least-Squares, whereby \eqn{m} is the number
  of columns of \code{data}.   
}

\value{
  Dependent on the chosen estimation method either an object of class
  \code{Goestica} or, \code{Goestmm} or \code{Goestml} or
  \code{Goestnls} is returned. All of these classes extend the
  \code{GoGARCH} class. 
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
  \code{\linkS4class{GoGARCH}}, \code{\linkS4class{Goestica}},
  \code{\linkS4class{Goestmm}}, \code{\linkS4class{Goestnls}},
  \code{\linkS4class{Goestml}}, \code{\link{goest-methods}}
}

\examples{
\dontrun{
library(vars)
## Boswijk / van der Weide (2009)
data(BVDWSTOXX)
BVDWSTOXX <- zoo(x = BVDWSTOXX[, -1], order.by = BVDWSTOXX[, 1])
BVDWSTOXX <- window(BVDWSTOXX, end = as.POSIXct("2007-12-31"))
BVDWSTOXX <- diff(log(BVDWSTOXX))
sectors <- BVDWSTOXX[, c("AutoParts", "Banks", "OilGas")]
sectors <- apply(sectors, 2, scale, scale = FALSE)
gogmm <- gogarch(sectors, formula = ~garch(1,1), estby = "mm",
         lag.max = 100)
gogmm
## Boswijk / van der Weide (2006)
data(BVDW)
BVDW <- zoo(x = BVDW[, -1], order.by = BVDW[, 1])
BVDW <- diff(log(BVDW)) * 100
gognls <- gogarch(BVDW, formula = ~garch(1,1), scale = TRUE,
          estby = "nls")
gognls
## van der Weide (2002)
data(VDW)
var1 <- VAR(scale(VDW), p = 1, type = "const")
resid <- residuals(var1)
gogml <- gogarch(resid, ~garch(1, 1), scale = TRUE,
         estby = "ml", control = list(iter.max = 1000))
gogml
solve(gogml@Z)
}
}

\keyword{models}
\concept{GARCH}
\concept{GO-GARCH}
\concept{Euler Angles}