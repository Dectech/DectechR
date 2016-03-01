\name{getCoeffTolerance}
\alias{getCoeffTolerance}
\title{
  %%  ~~function to do ... ~~
  Gets tolerance for regression coefficients
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
  Calculates the varience inflation factors (VIF) for linear and generalized linear models, returning the tolerance (1/VIF).
}
\usage{
getCoeffTolerance(mod)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mod}{
%%     ~~Describe \code{dframe} here~~
  A lm or glm object.
}

}
\details{
    Tolerance is 1/VIF. VIF is a measure of multicolinearity. See \url{https://en.wikipedia.org/wiki/Variance_inflation_factor} for more details.\cr\cr
    This function relies on the \code{\link{vif}} function from the \code{\link{car}} package to do the main calculation. However when there are categorical/factor variables in the regresion, the \code{\link{vif}} function returns the value for categorical variable, but doesn't repeat across the levels. Hence the resulting vector is a different length as the coefficient table.\cr\cr
    This function simple expands that result so it can be added to the coefficient table.


}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
    For more details on the vif calculation see \code{\link{vif}} in the \code{\link{car}} package.
}
\examples{
# Running a linear regression on the mtcars data set...
# ...predicting miles per gallon
m1 = lm( mpg ~ cyl + disp + hp + gear + carb + gear*carb,data=mtcars)
tol = getCoeffTolerance(m1)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ regression }
\keyword{ coefficients }
\keyword{ clipboard }% __ONLY ONE__ keyword per line
