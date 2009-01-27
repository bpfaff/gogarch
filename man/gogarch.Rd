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
gogarch(data, formula, scale = FALSE, method = c("ml"), initial = NULL,
        garchlist = list(init.rec = "mci", delta = 2, skew = 1, shape =
        4, cond.dist = "norm", include.mean = FALSE, include.delta = NULL, 
        include.skew = NULL, include.shape = NULL, leverage = NULL, trace =
        FALSE, algorithm = "nlminb", hessian = "ropt", control = list(), title
        = NULL, description = NULL), ...) 
}

\arguments{
  \item{data}{Matrix: the original data set.}
  \item{formula}{Formula: valid formula for univariate GARCH models.}
  \item{scale}{Logical, if \code{TRUE} the data is scaled. The default
  is \code{scale = FALSE}.}
  \item{method}{Character: currently only Maximum-Likelihood estimation
  is implemented.}
  \item{initial}{Numeric: starting values for Euler angles (see Details).}
  \item{garchlist}{List: Elements are passed to \code{garchFit}.}
  \item{\dots}{Ellipsis argument: is passed to \code{nlminb}.}
}

\details{
If the argument \code{initial} is left \code{NULL}, the starting values
are computed according \code{seq(3.0, 0.1, length.out = l)}, whereby
\code{l} is the length of \code{initial}. This length must be equal to
\eqn{m * (m - 1)/2}, whereby \eqn{m} is the number of columns of
\code{data}.  
}

\value{
  An object of class \code{Goestml} is returned which extends the
  \code{GoGARCH} class.
}

\references{
  Van der Weide, Roy (2002), GO-GARCH: A Multivariate Generalized
  Orthogonal GARCH Model, \emph{Journal of Applied Econometrics},
  \bold{17(5)}, 549 -- 564.
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