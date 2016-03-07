\name{getOutput}
\alias{getOutput}
\title{
  %%  ~~function to do ... ~~
  Get Regression Table (to clipboard)
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
  Extract the coefficients and various performance measures from a regression model, and copy to the clipboard for transferring to excel.
}
\usage{
getOutput(mod, performanceTableAtTop = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mod}{
%%     ~~Describe \code{dframe} here~~
  A model object. Support types include lm, glm, polr, mlogit.
}
  \item{performanceTableAtTop}{
%%     ~~Describe \code{myfile} here~~
  (optional) parameter to determine where the performance statistics are placed. Default is at the top, but set to \code{FALSE} to place at the bottom.
}
\item{...}{
  (optional) additional arguments. In particular for lm and glm models the tolerance column is added by default but can be removed by setting \code{tolerance = FALSE}.
}
}
\details{
    Coefficient tables are relatively easy to extract from a fitted model, but this function aims to combine this table with a range of performance statistics e.g. AIC, and then copy the results directly to the clipboard.\cr\cr
    For some models, additional columns are added to the coefficient table. For linear regressions, standardised betas are included. For lm and glm models, tolerance (1/VIF) is added (but can be removed by setting the \code{tolerance} parameter to \code{FALSE}. For ordinal logit models (polr) a column of p-values is added.\cr\cr



}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
    See also \code{\link{getOutputTable}}
}
\examples{

# Running a linear regression on the mtcars data set...
# ...predicting miles per gallon
m1 = lm( mpg ~ cyl + disp + hp + gear + carb + gear*carb, data = mtcars)
getOutput(m1)
# output table is now copied to clipboard

# running a binary logistic regression
mLogit = glm( am ~ cyl + disp + hp ,data=mtcars,family = "binomial")
getOutput(mLogit)
# output table is now copied to clipboard

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ regression }
\keyword{ coefficients }
\keyword{ clipboard }% __ONLY ONE__ keyword per line
