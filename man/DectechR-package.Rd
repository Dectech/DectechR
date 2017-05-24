\name{DectechR-package}
\alias{DectechR-package}
\alias{DectechR}
\docType{package}
\title{
Useful functions for Dectech analysts
}
\description{
Collection of functions useful for Dectech analysts, such as extracting survey data from Questback output, and getting consistent coefficient tables from regressions.\cr \cr
The main functions are:
\itemize{
    \item \code{\link{getOutput}} (extract regression output)
    \item \code{\link{getScreePlot}}
    \item \code{\link{getFactorLoadingsTable}}
    \item \code{\link{cc}} (copy to clipboard)
    \item \code{\link{dectechXmlToDataframe}} (convert Questback output to a dataframe)
}

}
\details{
\tabular{ll}{
Package: \tab DectechR\cr
Type: \tab Package\cr
Version: \tab 0.1\cr
Date: \tab 2016-02-09\cr
License: \tab GPL-2\cr
}
Useful functions to have loaded during general data analysis at Dectech.
}
\author{
Keith Simpson

Maintainer: Keith Simpson <k.simpson@dectech.co.uk>
}


\keyword{ package }

\examples{
  x = c(1,2,3)
  cc(x)
}
