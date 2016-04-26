\name{getScreePlot}
\alias{getScreePlot}
\title{
  %%  ~~function to do ... ~~
  Displays a scree plot, and copies the table of explained variance to the clipboard.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
  Before running a factor analysis, this function is used to display a scree plot, and copy the table of explained variance to the clipboard.
}
\usage{
getScreePlot(data, toClipboard = TRUE)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{dframe} here~~
  A matrix of numeric data.
  }
  \item{toClipboard}{
  Indicates if the table of variance should be written to the cliboard (default) or printed to the console.
  }
}
\details{
    This function shows the scree plot to help determine the number of factors to extract, and creates a table of eigenvalues and the percentage of variance explained by different numbers of factors. The table is copied to the clipboard.
}



%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
    See also \code{\link{...}}
}
\examples{
# Using the built in data frame, mtcars:
getScreePlot(mtcars)
# ...results suggest a two factor solution

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ factor analysis }
\keyword{ factor }
\keyword{ scree }
\keyword{ eigen }

