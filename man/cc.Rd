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
cc(data, destination = "clipboard-128")
cc2(data, destination = NA, includeRowNames = FALSE)
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
  (optional) parameter to save the row names for cc2. Generally row names don't contain useful information, so be default they are dropped by cc2. But if required they can be kept by setting this value to TRUE.
}
}
\details{
    A quick and simple way to transfer data to excel. A table or data frame can copied directly to the clipboard, so you can just switch to Excel and hit paste (or ctrl+v).\cr\cr
    cc2 is generally the better function to use.\cr\cr
    cc should work in most occasions but has a memory limit, so may fail for big tables or data frames. Cross tabulations may loss variable names, with when there is more that two variables the table will be flattened into a long format.\cr\cr
    cc2 has no memory limit, and gives a more intuitive result for cross tabulations.\cr\cr


}

\note{
cc is just a simple wrapper to the write.table function, whereas cc2 uses writeClipboard but first manipulates the data into a useful format.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
    See also \code{\link{write.table}} and \code{\link{writeClipboard}}
}
\examples{
# Using the built in data frame, mtcars:

# make a frequency table of number of gears
gearsTable = with(mtcars, table(gear))

# Copy the table to clipboard...
cc(gearsTable)
# ...Now go to Excel and paste!

## Two variable cross tab...
gearsVsCylindersTab = with(mtcars, table(gear,cyl))

# first try cc, when pasted to excel structure retained, but variable names dropped
cc(gearsVsCylindersTab)
# compare with cc2, where variable names retained...
cc2(gearsVsCylindersTab)


## Three variable cross tab...
threeWayTable = with(mtcars, table(gear,cyl,carb))

# with a three way table, cc will flaten output into long format...
cc(threeWayTable)
#...while cc2 should retain the shape with last variable used as columns
cc2(threeWayTable)


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ copy }
\keyword{ clipboard }% __ONLY ONE__ keyword per line
