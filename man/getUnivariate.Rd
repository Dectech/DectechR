\name{getUnivariate}
\alias{getUnivariate}
\title{
    %%  ~~function to do ... ~~
        Run "univariate" regressions for each variable in a model, and copy to clipboard
}
\description{
    %%  ~~ A concise (1-5 lines) description of what the function does. ~~
        Based on the input model, runs series of models predicting the dependant variable with one  independant variables at a time (univariate).
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
    Loops over each independent variable in the input model, predicting the dependent variable using that IV alone. Hence each row represents a seperate model*.\cr\cr
    This is useful for comparing against a multivariate model containing the same IVs. The univariate results show the individual impact of each IV, while the full regression shows how they might interact. For example if two IV's are correlated, they might both come out as significant predictors in a univariate regression, but in a full regression their betas may impact each other in unpredictable ways (they may be suppressed or exagerated depending on the nature of their interaction). For lm and glm regressions you should also refer to the "tolerance" outputted by \code{\link{getOutput}} to identify collinearity.\cr\cr
    * NB: For factor and categorical variables, each level is outputted on its own line (except the baseline), but it's important to remember only one model is actually run. For example, consider predicting age using an "Income" variable with levels "Low", "Med" and "High". The univariate model run would be: \code{lm(age ~ Income)}, and there would be two betas added to the result table, one for "IncomeMed" and one for "IncomeHigh". In these cases, if \code{returnIntercept} is set to \code{TRUE}, there is only one model intecept to output, so this value would be repeated for each level of this factor.\cr\cr



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

