\name{cc}
\alias{cc}
\alias{cc2}
\title{
  %%  ~~function to do ... ~~
  Copy to Clipboard
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
  Copy a table to the clipboard for transferring to excel.
}
\usage{
cc(data, destination = NA, includeRowNames = FALSE, nestedOrderOutToIn = TRUE, forceNested = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{dframe} here~~
  A table or data frame to copy to the clipboard.
}
  \item{destination}{
%%     ~~Describe \code{myfile} here~~
  (optional) output file, in case the data is to be written to a file rather than the clipboard.
}
\item{includeRowNames}{
  (optional) parameter to save the row names. Generally row names don't contain useful information, so by default they are dropped. But if required they can be kept by setting this value to TRUE.
}
\item{nestedOrderOutToIn}{
  (optional) parameter to determine the order of levels for a nested table. By default it is ordered first by te outer variable, then the next outer most variable, etc. This is reveresed if set to FALSE.
}
\item{forceNested}{
  (optional) if input is not a nested table, then can still attempt to use the second last column to reshape into a wide format table.
}
}
\details{
    A quick and simple way to transfer data to excel. A table or data frame can copied directly to the clipboard, so you can just switch to Excel and hit paste (or ctrl+v).\cr\cr

}

\note{
cc2 is indentical, but exists for compatibility with older versions of DectechR package.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
    See also \code{\link{write.table}} and \code{\link{writeClipboard}}.
    See \code{\link{cc_varlist}} for getting a character vector in a nice format for use in a script.
}
\examples{
# Using the built in data frame, mtcars:

# make a frequency table of number of gears
gearsTable <- with(mtcars, table(gear))

# Copy the table to clipboard...
cc2(gearsTable)
# ...Now go to Excel and paste!

# Two variable cross tab...
gearsVsCylindersTab <- with(mtcars, table(gear,cyl))
cc(gearsVsCylindersTab)


# Three variable cross tab...
threeWayTable <- with(mtcars, table(gear,cyl,carb))
cc(threeWayTable)


# Forcing a data.frame to be "nested"...
# ...the following is a data.frame, so won't be automatically nested
averageMpg <- with(mtcars, aggregate(mpg,list("cyl" = cyl,"gear" = gear),mean))
# ...but compare the output of:
cc(averageMpg)
# ...to this:
cc(averageMpg, forceNested = T)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ copy }
\keyword{ clipboard }% __ONLY ONE__ keyword per line
