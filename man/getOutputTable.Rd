\name{getOutputTable}
\alias{getOutputTable}
\alias{getOutputTable.lm}
\alias{getOutputTable.glm}
\alias{getOutputTable.mlogit}
\alias{getOutputTable.polr}
\alias{getOutputTable.default}
\title{
  %%  ~~function to do ... ~~
  Get coefficient and performance tables for regressions.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
  Extract the coefficients and various performance measures from a regression model, and returns a list containing each table. See \code{\link{getOutput}} for the function that combines these tables and copies them to the clipboard.
}
\usage{
getOutputTable(mod,...)
\method{getOutputTable}{lm}(mod, tolerance = TRUE,...)
\method{getOutputTable}{glm}(mod, tolerance = TRUE,...)
\method{getOutputTable}{mlogit}(mod,...)
\method{getOutputTable}{polr}(mod,...)
\method{getOutputTable}{default}(mod,...)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mod}{
%%     ~~Describe \code{dframe} here~~
  A model object. Support types include lm, glm, polr, mlogit.
}
  \item{tolerance}{
%%     ~~Describe \code{myfile} here~~
  (optional) parameter. For lm and glm models the tolerance column is added by default but can be removed by setting \code{tolerance = FALSE}.
}
\item{...}{
  (optional) additional arguments
}
}
\details{
    Coefficient tables are relatively easy to extract from a fitted model, but this function aims to combine this table with various performance statistics e.g. AIC.\cr\cr
    For some models, additional columns are added to the coefficient table. For linear regressions, standardised betas are included. For lm and glm models, tolerance (1/VIF) is added (but can be removed by setting the \code{tolerance} parameter to \code{FALSE}. For ordinal logit models (polr) a column of p-values is added.\cr\cr



}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
    For extracting regression results, use the more user friendly \code{\link{getOutput}} function.
}
\examples{
    m1_lm = lm( mpg ~ cyl + disp + hp+ gear + carb + gear*carb,data=mtcars)
    outputTables = getOutputTable(m1_lm)
    # outputTables now contains two tables, a coefficent table and a performance table
    names(outputTables)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ regression }
\keyword{ coefficients }
