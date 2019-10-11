\name{getFactorLoadingsTable}
\alias{getFactorLoadingsTable}
\title{
  %%  ~~function to do ... ~~
  Extract an ordered factor loadings table from a factor analysis object
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
  After running a factor analysis (using \code{\link{factanal}}) this function will extract an ordered table of loadings from the result, with an additional column displaying uniqueness. The result is copied to the clipboard.
}
\usage{
getFactorLoadingsTable(FAResult, toClipboard = TRUE)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{FAResult}{
%%     ~~Describe \code{dframe} here~~
  An object of class "factanal" i.e. the output from running the \code{\link{factanal}} function.
}
\item{toClipboard}{
    Indicates if the table of variance should be written to the cliboard (default) or printed to the console.
}
}
\details{
    The loadings from factanal are returned with the rows ordered the same as the input columns. This function reorders them so that the variables are grouped based on the factor the load most heavily on.\cr\cr
    A uniqueness value is also added. Uniqueness is the proportion of variance of the variable that is not accounted for by all of the factors taken together. A high uniqueness can indicate that a variable may not belong with any of the factors. (Uniqueness is equal to 1 - communality.).
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
    See also \code{\link{getScreePlot}}
}
\examples{
# Using the built in data frame, mtcars:
fa1 = factanal(mtcars, factors = 3, rotation = "promax")
getFactorLoadingsTable(fa1)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ factor analysis }
\keyword{ factor }
\keyword{ loadings }

