\name{unpackUserVariable}
\alias{unpackUserVariable}
\title{
  %%  ~~function to do ... ~~
  Unpack user defined variables
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
  Unpack a user defined variable (output from a QuestBack custom question), where multiple responses are stored in one column e.g. "1*3*5*7"
}
\usage{
unpackUserVariable(rawText, maxCols = NULL, emptyReplacement = -99, numeric = FALSE, delimiter = "*")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rawText}{
%%     ~~Describe \code{dframe} here~~
  A (character) vector that you want to split
}
  \item{maxCols}{
%%     ~~Describe \code{myfile} here~~
  (optional) If rows have different lengths, then this should define the maximum number of columns returned
}
\item{emptyReplacement}{
  (optional) value to use when some rows are shorter than maxCols (default is -99)
}
\item{numeric}{
  (optional) When set to TRUE, will attempt to convert output to numeric matrix (default is FALSE)
}
\item{delimiter}{
  (optional) character used to split elements in each row (default is *)
}
}
\details{
    Sometimes the output from a custom question on QuestBack is stored as a string representing multiple values e.g. "1*3*5*7". This function takes a vector of values stored in this format and splits them out into a matrix of values. If the rows have variable lengths, and maxCols has been set, then each row will be padded out using the emptyReplacement value.\cr\cr

}

\value{
    Returns a matrix with same number of rows as input. If input had variable lengths and maxCol was not set, then returns a list.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
    See also \code{\link{strsplit}}
}
\examples{
    # most common usage...
    test1 = c("a*1*2*3","b*10*20*30","c*11*22*33","d*1*2*3")
    unpackUserVariable(test1)

    # if you know the input is numeric then can convert..
    test2 = c("1*2*3","10*20*30","11*22*33","1*2*3")
    unpackUserVariable(test2,numeric = T)

    # if input has variable number of elements per row..
    # ...then set maxCols to match the longest row...
    test3 = c("1*2*3","10*20","11*22*33","1*2*3")
    thisResult = unpackUserVariable(test3,numeric = T, maxCols = 3)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ split }
\keyword{ unpack }% __ONLY ONE__ keyword per line
