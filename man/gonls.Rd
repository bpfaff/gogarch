\name{gonls}

\encoding{latin1}

\alias{gonls}

\title{
  Non-linear least-squares estimation of matrix B
}

\description{
  This is the target function for estimating the matrix \eqn{B} by
  non-linear least-squares. It is used in the estimation method
  \code{goest} if \code{method = "nls"} is chosen.
}

\usage{
gonls(params, SSI)
}

\arguments{
  \item{params}{The initial values of the \eqn{vech(B)}.}
  \item{SSI}{A list with two elements, each a list itself, containing
    \eqn{S_t = s_t s_t' - I_m} and \eqn{S_{t-1} = s_{t-1} s_{t-1}' -
      I_m}, respectively.}
}

\details{
  Boswijk and van der Weiden (2006) proposed the following criterion
  function:

  \deqn{
    S(A) = \frac{1}{n} \sum_{t = 1}^n tr([s_t s_t' - I_m - B(s_{t-1}
    s_{t-1}' - I_m)B]^2) = S^*(B)
  }
  
  for retrieving the matrix \eqn{U}. This matrix is the eigen vector
  matrix of \eqn{B}. The linear map \eqn{Z = P \Delta^{1/2} U} and its
  inverse can then be computed for calculating the component matrix
  \eqn{Y = X Z^{-1}}.
}
\value{
  \item{f}{\code{numeric}: The value of the target function.}
}

\references{
    Boswijk, H. Peter and van der Weide, Roy (2006), Wake me up before
    you GO-GARCH, \emph{Tinbergen Institute Discussion Paper}, \bold{TI
    2006-079/4}, University of Amsterdam and Tinbergen Institute.
}

\author{
  Bernhard Pfaff
}

\seealso{
  \code{\link{gogarch}}
}

\keyword{models}
\concept{GARCH}
\concept{GO-GARCH}
