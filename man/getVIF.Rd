\name{getVIF}
\alias{getVIF}
\title{
    %%  ~~function to do ... ~~
        Calculates the varience inflation factors.
}
\description{
    %%  ~~ A concise (1-5 lines) description of what the function does. ~~
        Calculates the varience inflation factors (VIF) for linear and generalized linear models.
}
\usage{
    getVIF(mod)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
    \item{mod}{
        %%     ~~Describe \code{dframe} here~~
            A lm or glm object.
    }

}
\details{
    VIF is a measure of multicolinearity. See \url{https://en.wikipedia.org/wiki/Variance_inflation_factor} for more details.\cr\cr
    This function is a slight modificaton of the \code{\link{vif}} function in the \code{\link{car}} package. It has been included here to (a) reduce external dependancies, and (b) deal with some evironment name confusion (e.g. the orginal calls \code{\link{model.matrix}} which is in both the \code{\link{stats}} package and the \code{\link{SparseM}} package, and in some situations the latter was incorrectly called).\cr\cr



}


%% ~Make other sections like Warning with \section{Warning }{....} ~

    \seealso{
        For more details on the vif calculation see \code{\link{vif}} in the \code{\link{car}} package.
    }
\examples{
    # Running a linear regression on the mtcars data set...
    # ...predicting miles per gallon
    m1 = lm( mpg ~ factor(cyl) + gear + carb + gear*carb,data=mtcars)
    # compare VIF...
    v = getVIF(m1)
    # ...to tolerance (converting to numeric)
    tol = as.numeric(getCoeffTolerance(m1)[-1])
    # tol = 1 / vif
    # but getCoeffTolerance repeats vif across factor levels
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ regression }
\keyword{ coefficients }
\keyword{ clipboard }% __ONLY ONE__ keyword per line
