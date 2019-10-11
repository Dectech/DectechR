\name{sc}
\alias{sc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
    Search column names of a data frame
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
    Lets you search the variable names of a data.frame for a particular string/pattern. A simple wrapper for the \code{\link{grep}} function.
}
\usage{
sc(pattern, dataframe, value = TRUE, ignore.case = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pattern}{
  %%     ~~Describe \code{x} here~~
    Character string containing the pattern you want to match. This is treated as a \code{\link[base:regex]{regular expression}}.
  }
  \item{dataframe}{
  %%     ~~Describe \code{x} here~~
    A data.frame, or a matrix with column names
  }
  \item{value}{
  %%     ~~Describe \code{x} here~~
    If \code{TRUE} returns the text of the matched values (default). If \code{FALSE} returns the indicies of the matched values.
  }
  \item{ignore.case}{
  %%     ~~Describe \code{x} here~~
    If \code{FALSE}, the pattern matching is case sensitive (default)
  }

}
\details{
%%  ~~ If necessary, more details than the description above ~~
    This is just a simple wrapper for the \code{\link{grep}} function. It searches through \code{colnames(data.frame)} for the pattern given. \cr
    If you don't want your code to be reliant on the DectechR package, you will get the same functionailty using something like:\cr

    \code{grep("pattern", names(dataframe), value = TRUE)}



}
\value{
%%  ~Describe the value returned
    Returns a vector of column names that match the pattern given. If \code{value == FALSE} returns a vector of indicies of the matched columns.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
In Rstudio you can add a "code snippet" so that when you type "sc" and hit shift+tab, it inserts the equivalent base code into your script (i.e. \code{grep("pattern", names(dataframe), value = TRUE)}).\cr
This means you still have the advantage of a quick to type 2-letter function, but you are just using a base R funcion (\code{\link{grep}}) and aren't tied to DectechR. \cr\cr
    To do this go to \emph{Tools>Global Options>Code>Edit Snippets}, and then add the following:\cr
    \code{snippet sc grep("${1:pattern}", names(${2:dataframe}), value = TRUE)}\cr
    ...then at any time, in your script just type the letters "sc" followed by shift+tab.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
    See also \code{\link{grep}} and \code{\link{regex}}
}
\examples{
    # search the "iris" data set for variables containing the word "Width"
    sc("Width", iris)

    # search the "mtcars" data set for variables ending with the letter "t"...
    sc("t$", mtcars)

    # get the indicies of all variables containing the letter "e"
    sc("e", mtcars, value = F)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~grep }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~search }% __ONLY ONE__ keyword per line
\keyword{ ~find }% __ONLY ONE__ keyword per line
