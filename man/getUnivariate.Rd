\name{getUnivariate}
\title{
    %%  ~~function to do ... ~~
        Run "univarite" regressions for each variable in a model, and copy to clipboard
}
\description{
    %%  ~~ A concise (1-5 lines) description of what the function does. ~~
        Based on the input model, a series of models are run that predict the dependant variable using each of the independant variables, one at a time.
}
\usage{
    getUnivariate(mod, returnIntercept = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
    \item{mod}{
        %%     ~~Describe \code{dframe} here~~
            A model object. Support types include lm, glm, polr.
    }
    \item{returnIntercept}{
        %%     ~~Describe \code{myfile} here~~
            (optional) if set to \code{TRUE} will also include a column for the intercept value (or for an ordinal logit regression, it will return columns for each boundary)
    }

}
\details{
    Loops over each independent variable in the input model, predicting the dependent variable using that IV alone.\cr\cr
    This is useful for comparing against the results of a regression with all IV's included together. The univariate results show the individual impact of each IV, while the full regression shows how they might interact. For example if two IV's are correlated, they might both come out as significant predictors in a univariate regression, but in a full regression their betas may impact each other in unpredictable ways (they may be suppressed or exagerated depending on the nature of their interaction). For lm and glm regressions you should also refer to the "tolerance" outputted by \code{\link{getOutput}} to identify collinearity.\cr\cr
    NB: For categorical/factor variables, each level is outputted on its own line (except the baseline), even though only one model was run for that IV. For example, consider an "Income" variable with levels "Low", "Med" and "High" predicting age as an IV. The univariate model run would be lm(age ~ Income), but there would be two betas (and p-values added to the result table), for "IncomeMed" and "IncomeHigh". If \code{returnIntercept} is set to \code{TRUE}, there is only one model intecept, so this would be returned on each row i.e. the same value is repeated.\cr\cr



}


%% ~Make other sections like Warning with \section{Warning }{....} ~
\seealso{
    See also \code{\link{runUnivariate}} which returns the result as a dataframe rather than copying to clipboard
}


\examples{

    # Running a linear regression using the built in mtcars data set...
    m1 = lm(mpg ~ gear + factor(carb) + hp, mtcars)
    # to run unvariate results for each IV and copy result to clipboard:
    getUnivariate(m1)

    # to also return the intercept for each univariate model, run:
    getUnivariate(m1, returnIntercept = T)



}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ regression }
\keyword{ univariate }

