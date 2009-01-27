\name{gotheta}

\alias{gotheta}

\encoding{latin1}

\title{
  Creates an object of class GoGARCH based on Euler angles
}

\description{
  This function returns an object of class \code{GoGARCH} based on an
  input vector of Euler angles.
}

\usage{
gotheta(theta, object, garchlist = list(init.rec = "mci", delta = 2,
skew = 1, shape = 4, cond.dist = "norm", include.mean = FALSE,
include.delta = NULL, include.skew = NULL, include.shape = NULL,
leverage = NULL, trace = FALSE, algorithm = "nlminb", hessian = "ropt",
control = list(), title = NULL, description = NULL))
}

\arguments{
  \item{theta}{Vector of Euler angles.}
  \item{object}{An object of formal class \code{Goinit} or an extension
    thereof.}
  \item{garchlist}{List with optional elements passed to \code{garchFit}.}  
}

\details{
  In a first step the orthogonal matrix \eqn{U} is computed as the
  product of rotation matrices given the vector \code{theta} of Euler
  angles with the function \code{UprodR}. The linear map \eqn{Z} is
  computed next as \eqn{Z = P D^{\frac{1}{2}} U'}. The unobserved
  components \eqn{Y} are calculated as \eqn{Y = X Z^{-1}}. These are
  then utilized in the estimation of the univariate GARCH models
  according to \code{object@garchf}. The conditional variance/covariance
  matrices are calculated according to \eqn{V_t = Z H_t Z'} whereby
  \eqn{H_t} signifies a matrix with the conditional variances of the
  unvariate GARCH models on its diagonal.  
}

\value{
  Returns an object of class \code{GoGARCH}.
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
  \code{\linkS4class{Goinit}}, \code{\linkS4class{GoGARCH}},
  \code{\linkS4class{Goestml}}, \code{\link[fGarch]{garchFit}}
}

\examples{
\dontrun{
library(vars)
data(VDW)
var1 <- VAR(VDW, p = 1, type = "const")
resid <- resid(var1)
gin <- goinit(resid, scale = TRUE)
gotheta(0.5, gin)
}
}

\keyword{models}
\concept{GO-GARCH}
\concept{Euler angles}