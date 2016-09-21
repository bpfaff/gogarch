\name{BVDWSTOXX}

\encoding{latin1}

\alias{BVDWSTOXX}

\docType{data}

\title{
  Sector indices of the EURO STOXX 600
}
\description{
  The data frame contains the following sector indices of the EURO STOXX
  600 index: Automobiles \& Parts, Banks, Basic Resources, Chemicals,
  Construction and Materials, Financial Services, Food \& Beverages,
  Health Care, Industrial Goods \& Services, Insurance, Media, Oil \&
  Gas, Technology, Telecommunications and Utilities. The data range is
  from 31th December 1986 until 21st November 2008.
}

\usage{data(BVDWSTOXX)}
\format{
  A data frame with 5652 observations on the following 16 variables.
  \describe{
    \item{\code{Date}}{POSIXt: The dates of observations.}
    \item{\code{AutoParts}}{Sector index Automobiles \& Parts}
    \item{\code{Banks}}{Sector index Banks}
    \item{\code{BasicRes}}{Sector index Basic Resources}
    \item{\code{Chemicals}}{Sector index Chemicals}
    \item{\code{ConstrMat}}{Sector index Construction and Materials}
    \item{\code{FoodBeverage}}{Sector index Food \& Beverages}
    \item{\code{FinService}}{Sector index Financial Services}
    \item{\code{HealthCare}}{Sector index Health Care}
    \item{\code{IndustrialGoods}}{Sector index Industrial Goods \& Services}
    \item{\code{Insurance}}{Sector index Insurance}
    \item{\code{Media}}{Sector index Media}
    \item{\code{OilGas}}{Sector index Oil \& Gas}
    \item{\code{Technology}}{Sector index Technology}
    \item{\code{Telecom}}{Sector index Telecommunications}
    \item{\code{Utilities}}{Sector index Utilities}
  }
}

\source{
  \url{http://www.stoxx.com}
}

\references{
  Boswijk, H. Peter and van der Weide, Roy (2009), Method of Moments
  Estimation of GO-GARCH Models, \emph{Working Paper}, University of
  Amsterdam, Tinbergen Institute and World Bank.
}

\examples{
data(BVDWSTOXX)
str(BVDWSTOXX)
}

\keyword{datasets}
