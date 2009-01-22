\name{gollh}

\alias{gollh}

\encoding{latin1}

\title{
  Log-Likelihood function of GO-GARCH models
}

\description{
  This function returns the negative of the log-Likelihood function for
  GO-GARCH models. 
}

\usage{
gollh(params, X, P, Dsqr, m, n, formula = ~garch(1, 1),
      garchlist = list(init.rec = "mci", delta = 2, skew = 1,
      shape = 4, cond.dist = "norm", include.mean = FALSE, 
      include.delta = NULL, include.skew = NULL, include.shape = NULL,
      leverage = NULL, trace = FALSE, algorithm = "nlminb",
      hessian = "ropt", control = list(), title = NULL,
      description = NULL))
}

\arguments{
  \item{params}{Vector of initial values for \code{theta}.}
  \item{X}{Matrix, the original data matrix with dimension \eqn{n \times m}.}
  \item{P}{Matrix, rotation matrix returned by \code{svd} or
  \code{prcomp}, dimension \eqn{m \times m}.}
  \item{Dsqr}{Matrix, square roots of the eigenvalue decomposition on
  its diagonal, dimension \eqn{m \times m}.}
  \item{m}{Integer, number of columns of \code{X}.}
  \item{n}{Integer, number of rows of \code{X}.}
  \item{formula}{Formula, type of GARCH model, default is \code{~ garch(1,1)}.}
  \item{garchlist}{List, elements are passed to \code{garchFit}.}
}

\details{
  The log-Likelihood function of GO-GARCH models is given as:

  \deqn{
    L_{\theta, \alpha, \beta} = - \frac{1}{2} \sum_{t=1}^T m \log(2\pi)
    + \log|Z_\theta Z_\theta '| + \log|H_t| + y' H_t^{-1}y_t
  }
  whereby \eqn{Z = P \Delta^{\frac{1}{2}} U_0}, \eqn{y_t = Z^{-1}x_t} and
    \eqn{H_t} is the conditional variance matrix of the independent
    components. 
}

\value{
  \item{negll}{Scalar, the negative value of the log-Likelihood function.}
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
  \code{\link[fGarch]{garchFit}}
}
\keyword{models}
\concept{GO-GARCH}
\concept{Likelihood}