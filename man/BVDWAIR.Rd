\name{BVDWAIR}

\alias{BVDWAIR}

\encoding{latin1}

\docType{data}

\title{
  Stock prices transportation sector, oil and kerosene prices
}
\description{
  This data frame contains the stock prices from American Airlines,
  South-West Airlines, Boeing and FedEx. In addition the spot prices for
  crude oil and kerosene are included. This data set was used in the
  article by Boswijk and van der Weide (2009). The data range is from
  July, 19 1993 until August, 12 2008. 
}

\usage{data(BVDWAIR)}
\format{
  A data frame with 3791 observations on the following 7 variables.

  \describe{
    \item{\code{Date}}{POSIXt: The dates of observations.}
    \item{\code{CrudeOil}}{Crude oil price.}
    \item{\code{Kerosene}}{Kerosene price.}
    \item{\code{AmericanAir}}{Stock prices of American Airlines.}
    \item{\code{SouthWest}}{Stock prices of South-West Airlines.}
    \item{\code{Boeing}}{Stock prices of Boeing.}
    \item{\code{FedEx}}{Stock prices of Boeing.}
  }
}

\details{
  The stock price data was downloaded from Yahoo Finance and the price
  series for crude oil and kerosene were obtained from the U.S. Energy
  Information Administration (EIA).
}

\source{
  \url{http://finance.yahoo.com} and \url{http://www.econstats.com}  
}

\references{
  Boswijk, H. Peter and van der Weide, Roy (2009), Method of Moments
  Estimation of GO-GARCH Models, \emph{Working Paper}, University of
  Amsterdam, Tinbergen Institute and World Bank.
}

\examples{
data(BVDWAIR)
str(BVDWAIR)
}

\keyword{datasets}
